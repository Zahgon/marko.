//! A backtracking matcher with the semantics of CPython's `re` for the
//! pattern subset marko uses: classes, groups, alternation, greedy and lazy
//! quantifiers, fixed-width lookaround, backreferences, `^ $ \Z`, and the
//! `i` / `m` inline flags.

use crate::pystr;

#[derive(Clone, Debug)]
enum Item {
    Ch(char),
    Range(char, char),
    Space(bool),
    Word(bool),
    Digit(bool),
}

#[derive(Clone, Debug)]
enum Node {
    Lit(char),
    LitCi(char),
    Any,
    Class(Vec<Item>, bool, bool),
    Bol,
    Eol,
    EndZ,
    Group(Box<Node>, Option<usize>),
    Concat(Vec<Node>),
    Alt(Vec<Node>),
    Repeat(Box<Node>, usize, usize, bool),
    Look(Box<Node>, bool, bool, usize),
    Backref(usize),
}

pub struct Regex {
    node: Node,
    groups: usize,
    multiline: bool,
}

#[derive(Clone, Debug)]
pub struct Match {
    pub spans: Vec<Option<(usize, usize)>>,
}

impl Match {
    pub fn start(&self) -> usize {
        self.spans[0].unwrap().0
    }
    pub fn end(&self) -> usize {
        self.spans[0].unwrap().1
    }
    pub fn span(&self, n: usize) -> Option<(usize, usize)> {
        self.spans[n]
    }
    pub fn group<'a>(&self, text: &'a [char], n: usize) -> Option<&'a [char]> {
        self.spans[n].map(|(a, b)| &text[a..b])
    }
}

struct Parser<'a> {
    p: &'a [char],
    i: usize,
    groups: usize,
    ci: bool,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<char> {
        self.p.get(self.i).copied()
    }

    fn alt(&mut self) -> Node {
        let mut branches = vec![self.concat()];
        while self.peek() == Some('|') {
            self.i += 1;
            branches.push(self.concat());
        }
        if branches.len() == 1 {
            branches.pop().unwrap()
        } else {
            Node::Alt(branches)
        }
    }

    fn concat(&mut self) -> Node {
        let mut items = Vec::new();
        while let Some(c) = self.peek() {
            if c == '|' || c == ')' {
                break;
            }
            let atom = self.atom();
            let atom = self.quant(atom);
            items.push(atom);
        }
        Node::Concat(items)
    }

    fn number(&mut self) -> Option<usize> {
        let s = self.i;
        while matches!(self.peek(), Some('0'..='9')) {
            self.i += 1;
        }
        if s == self.i {
            None
        } else {
            Some(pystr::string(&self.p[s..self.i]).parse().unwrap())
        }
    }

    fn quant(&mut self, atom: Node) -> Node {
        let (min, max) = match self.peek() {
            Some('*') => {
                self.i += 1;
                (0, usize::MAX)
            }
            Some('+') => {
                self.i += 1;
                (1, usize::MAX)
            }
            Some('?') => {
                self.i += 1;
                (0, 1)
            }
            Some('{') => {
                let save = self.i;
                self.i += 1;
                let lo = self.number();
                let (min, max) = if self.peek() == Some(',') {
                    self.i += 1;
                    let hi = self.number();
                    (lo.unwrap_or(0), hi.unwrap_or(usize::MAX))
                } else {
                    match lo {
                        Some(n) => (n, n),
                        None => {
                            self.i = save;
                            return atom;
                        }
                    }
                };
                if self.peek() != Some('}') {
                    self.i = save;
                    return atom;
                }
                self.i += 1;
                (min, max)
            }
            _ => return atom,
        };
        let greedy = if self.peek() == Some('?') {
            self.i += 1;
            false
        } else {
            true
        };
        Node::Repeat(Box::new(atom), min, max, greedy)
    }

    fn escape_char(&mut self) -> Result<char, Item> {
        let c = self.p[self.i];
        self.i += 1;
        Ok(match c {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            'f' => '\x0c',
            'v' => '\x0b',
            'a' => '\x07',
            's' => return Err(Item::Space(false)),
            'S' => return Err(Item::Space(true)),
            'w' => return Err(Item::Word(false)),
            'W' => return Err(Item::Word(true)),
            'd' => return Err(Item::Digit(false)),
            'D' => return Err(Item::Digit(true)),
            'u' => {
                let h = pystr::string(&self.p[self.i..self.i + 4]);
                self.i += 4;
                char::from_u32(u32::from_str_radix(&h, 16).unwrap()).unwrap()
            }
            other => other,
        })
    }

    fn atom(&mut self) -> Node {
        let c = self.p[self.i];
        self.i += 1;
        match c {
            '.' => Node::Any,
            '^' => Node::Bol,
            '$' => Node::Eol,
            '(' => {
                let mut cap = None;
                let mut look = None;
                if self.peek() == Some('?') {
                    self.i += 1;
                    match self.p[self.i] {
                        ':' => self.i += 1,
                        '=' => {
                            self.i += 1;
                            look = Some((false, false));
                        }
                        '!' => {
                            self.i += 1;
                            look = Some((false, true));
                        }
                        '<' => {
                            let neg = self.p[self.i + 1] == '!';
                            self.i += 2;
                            look = Some((true, neg));
                        }
                        _ => panic!("unsupported group"),
                    }
                } else {
                    self.groups += 1;
                    cap = Some(self.groups);
                }
                let inner = self.alt();
                assert_eq!(self.peek(), Some(')'));
                self.i += 1;
                match look {
                    Some((behind, neg)) => {
                        let w = width(&inner);
                        Node::Look(Box::new(inner), behind, neg, w)
                    }
                    None => Node::Group(Box::new(inner), cap),
                }
            }
            '[' => self.class(),
            '\\' => {
                let e = self.p[self.i];
                if e == 'Z' {
                    self.i += 1;
                    return Node::EndZ;
                }
                if e.is_ascii_digit() && e != '0' {
                    self.i += 1;
                    return Node::Backref(e.to_digit(10).unwrap() as usize);
                }
                match self.escape_char() {
                    Ok(ch) => self.lit(ch),
                    Err(item) => Node::Class(vec![item], false, self.ci),
                }
            }
            ch => self.lit(ch),
        }
    }

    fn lit(&self, ch: char) -> Node {
        if self.ci {
            Node::LitCi(ch)
        } else {
            Node::Lit(ch)
        }
    }

    fn class(&mut self) -> Node {
        let mut neg = false;
        if self.peek() == Some('^') {
            neg = true;
            self.i += 1;
        }
        let mut items = Vec::new();
        let mut first = true;
        loop {
            let c = self.p[self.i];
            if c == ']' && !first {
                self.i += 1;
                break;
            }
            first = false;
            self.i += 1;
            let lo = if c == '\\' {
                match self.escape_char() {
                    Ok(ch) => ch,
                    Err(item) => {
                        items.push(item);
                        continue;
                    }
                }
            } else {
                c
            };
            if self.peek() == Some('-') && self.p.get(self.i + 1).map_or(false, |&n| n != ']') {
                self.i += 1;
                let h = self.p[self.i];
                self.i += 1;
                let hi = if h == '\\' {
                    self.escape_char().ok().expect("range end")
                } else {
                    h
                };
                items.push(Item::Range(lo, hi));
            } else {
                items.push(Item::Ch(lo));
            }
        }
        Node::Class(items, neg, self.ci)
    }
}

fn width(n: &Node) -> usize {
    match n {
        Node::Lit(_) | Node::LitCi(_) | Node::Any | Node::Class(..) => 1,
        Node::Bol | Node::Eol | Node::EndZ | Node::Look(..) => 0,
        Node::Group(n, _) => width(n),
        Node::Concat(v) => v.iter().map(width).sum(),
        Node::Alt(v) => width(&v[0]),
        Node::Repeat(n, a, b, _) => {
            assert_eq!(a, b);
            width(n) * a
        }
        Node::Backref(_) => panic!("backref in lookbehind"),
    }
}

fn item_matches(items: &[Item], ci: bool, c: char) -> bool {
    items.iter().any(|it| match *it {
        Item::Ch(x) => x == c || (ci && pystr::ci_fold(x) == pystr::ci_fold(c)),
        Item::Range(a, b) => (a <= c && c <= b),
        Item::Space(n) => pystr::is_space(c) != n,
        Item::Word(n) => pystr::is_word(c) != n,
        Item::Digit(n) => pystr::is_digit(c) != n,
    })
}

struct Ctx<'t> {
    text: &'t [char],
    caps: Vec<Option<(usize, usize)>>,
    ml: bool,
}

type K<'k, 't> = &'k mut dyn FnMut(usize, &mut Ctx<'t>) -> bool;

impl Regex {
    pub fn new(pattern: &str) -> Regex {
        let p: Vec<char> = pattern.chars().collect();
        let mut i = 0;
        let mut ci = false;
        let mut ml = false;
        if p.starts_with(&['(', '?']) && p.len() > 2 && p[2].is_ascii_lowercase() {
            i = 2;
            while p[i] != ')' {
                match p[i] {
                    'i' => ci = true,
                    'm' => ml = true,
                    _ => panic!("flag"),
                }
                i += 1;
            }
            i += 1;
        }
        let mut parser = Parser { p: &p[i..], i: 0, groups: 0, ci };
        let node = parser.alt();
        assert_eq!(parser.i, parser.p.len(), "trailing pattern: {}", pattern);
        Regex { node, groups: parser.groups, multiline: ml }
    }

    pub fn multiline(mut self) -> Regex {
        self.multiline = true;
        self
    }

    fn single(&self, n: &Node, ctx: &Ctx, pos: usize) -> Option<bool> {
        let t = ctx.text;
        Some(match n {
            Node::Lit(c) => pos < t.len() && t[pos] == *c,
            Node::LitCi(c) => pos < t.len() && pystr::ci_fold(t[pos]) == pystr::ci_fold(*c),
            Node::Any => pos < t.len() && t[pos] != '\n',
            Node::Class(items, neg, ci) => pos < t.len() && item_matches(items, *ci, t[pos]) != *neg,
            _ => return None,
        })
    }

    fn m<'t>(&self, n: &Node, pos: usize, cx: &mut Ctx<'t>, k: K<'_, 't>) -> bool {
        if let Some(ok) = self.single(n, cx, pos) {
            return ok && k(pos + 1, cx);
        }
        match n {
            Node::Bol => {
                let ok = pos == 0 || (cx.ml && cx.text[pos - 1] == '\n');
                ok && k(pos, cx)
            }
            Node::Eol => {
                let t = cx.text;
                let ok = pos == t.len() || (t[pos] == '\n' && (cx.ml || pos == t.len() - 1));
                ok && k(pos, cx)
            }
            Node::EndZ => pos == cx.text.len() && k(pos, cx),
            Node::Group(inner, cap) => match cap {
                None => self.m(inner, pos, cx, k),
                Some(i) => {
                    let i = *i;
                    self.m(inner, pos, cx, &mut |p, cx: &mut Ctx<'t>| {
                        let old = cx.caps[i];
                        cx.caps[i] = Some((pos, p));
                        if k(p, cx) {
                            true
                        } else {
                            cx.caps[i] = old;
                            false
                        }
                    })
                }
            },
            Node::Concat(items) => self.seq(items, 0, pos, cx, k),
            Node::Alt(branches) => {
                for b in branches {
                    let saved = cx.caps.clone();
                    if self.m(b, pos, cx, k) {
                        return true;
                    }
                    cx.caps = saved;
                }
                false
            }
            Node::Repeat(inner, min, max, greedy) => {
                if self.single(inner, cx, pos).is_some() {
                    let (min, max, greedy) = (*min, *max, *greedy);
                    let mut n = 0usize;
                    if greedy {
                        while n < max && self.single(inner, cx, pos + n).unwrap() {
                            n += 1;
                        }
                        if n < min {
                            return false;
                        }
                        let mut c = n;
                        loop {
                            if k(pos + c, cx) {
                                return true;
                            }
                            if c == min {
                                return false;
                            }
                            c -= 1;
                        }
                    } else {
                        while n < min {
                            if !self.single(inner, cx, pos + n).unwrap() {
                                return false;
                            }
                            n += 1;
                        }
                        loop {
                            if k(pos + n, cx) {
                                return true;
                            }
                            if n >= max || !self.single(inner, cx, pos + n).unwrap() {
                                return false;
                            }
                            n += 1;
                        }
                    }
                }
                self.rep(inner, *min, *max, *greedy, 0, pos, cx, k)
            }
            Node::Look(inner, behind, neg, w) => {
                let saved = cx.caps.clone();
                let ok = if *behind {
                    if pos < *w {
                        false
                    } else {
                        self.m(inner, pos - *w, cx, &mut |p, _| p == pos)
                    }
                } else {
                    self.m(inner, pos, cx, &mut |_, _| true)
                };
                if *neg {
                    cx.caps = saved;
                    !ok && k(pos, cx)
                } else if ok {
                    if k(pos, cx) {
                        true
                    } else {
                        cx.caps = saved;
                        false
                    }
                } else {
                    cx.caps = saved;
                    false
                }
            }
            Node::Backref(i) => match cx.caps[*i] {
                Some((a, b)) => {
                    let t = cx.text;
                    let l = b - a;
                    pos + l <= t.len() && t[a..b] == t[pos..pos + l] && k(pos + l, cx)
                }
                None => false,
            },
            _ => unreachable!(),
        }
    }

    fn seq<'t>(&self, items: &[Node], i: usize, pos: usize, cx: &mut Ctx<'t>, k: K<'_, 't>) -> bool {
        if i == items.len() {
            return k(pos, cx);
        }
        self.m(&items[i], pos, cx, &mut |p, cx: &mut Ctx<'t>| self.seq(items, i + 1, p, cx, k))
    }

    #[allow(clippy::too_many_arguments)]
    fn rep<'t>(
        &self,
        inner: &Node,
        min: usize,
        max: usize,
        greedy: bool,
        count: usize,
        pos: usize,
        cx: &mut Ctx<'t>,
        k: K<'_, 't>,
    ) -> bool {
        if greedy {
            if count < max {
                let saved = cx.caps.clone();
                let hit = self.m(inner, pos, cx, &mut |p, cx: &mut Ctx<'t>| {
                    if p == pos {
                        return false;
                    }
                    self.rep(inner, min, max, greedy, count + 1, p, cx, k)
                });
                if hit {
                    return true;
                }
                cx.caps = saved;
            }
            count >= min && k(pos, cx)
        } else {
            if count >= min && k(pos, cx) {
                return true;
            }
            if count >= max {
                return false;
            }
            self.m(inner, pos, cx, &mut |p, cx: &mut Ctx<'t>| {
                if p == pos {
                    return false;
                }
                self.rep(inner, min, max, greedy, count + 1, p, cx, k)
            })
        }
    }

    /// `pattern.match(text, pos)`.
    pub fn match_at(&self, text: &[char], pos: usize) -> Option<Match> {
        if pos > text.len() {
            return None;
        }
        let mut cx = Ctx { text, caps: vec![None; self.groups + 1], ml: self.multiline };
        let mut end = None;
        let ok = self.m(&self.node, pos, &mut cx, &mut |p, _| {
            end = Some(p);
            true
        });
        if !ok {
            return None;
        }
        let mut spans = cx.caps;
        spans[0] = Some((pos, end.unwrap()));
        Some(Match { spans })
    }

    pub fn is_match_at(&self, text: &[char], pos: usize) -> bool {
        self.match_at(text, pos).is_some()
    }

    /// `pattern.search(text, pos)`.
    pub fn search(&self, text: &[char], pos: usize) -> Option<Match> {
        (pos..=text.len()).find_map(|s| self.match_at(text, s))
    }

    /// `pattern.finditer(text)`.
    pub fn find_iter(&self, text: &[char]) -> Vec<Match> {
        let mut out = Vec::new();
        let mut s = 0;
        while s <= text.len() {
            match self.search(text, s) {
                Some(m) => {
                    s = if m.end() == m.start() { m.end() + 1 } else { m.end() };
                    out.push(m);
                }
                None => break,
            }
        }
        out
    }

    /// `pattern.sub(f, text)` with a computed replacement.
    pub fn sub_with(&self, text: &[char], mut f: impl FnMut(&Match) -> Vec<char>) -> Vec<char> {
        let mut out = Vec::with_capacity(text.len());
        let mut last = 0;
        for m in self.find_iter(text) {
            out.extend_from_slice(&text[last..m.start()]);
            out.extend(f(&m));
            last = m.end();
        }
        out.extend_from_slice(&text[last..]);
        out
    }

    pub fn sub_str(&self, text: &[char], repl: &str) -> Vec<char> {
        let r: Vec<char> = repl.chars().collect();
        self.sub_with(text, |_| r.clone())
    }
}

/// `re.escape(s)` followed by compilation is a literal; marko's container
/// prefixes are built from literals and a few space runs, so they are matched
/// by `crate::block::Prefix` directly. This helper escapes for dynamic
/// patterns such as the HTML block end tag.
pub fn escape(s: &[char]) -> String {
    let mut out = String::new();
    for &c in s {
        if c.is_ascii_alphanumeric() || c == '_' || !c.is_ascii() {
            out.push(c);
        } else {
            out.push('\\');
            out.push(c);
        }
    }
    out
}
