//! Inline parsing: `marko.inline` and `marko.inline_parser`.

use crate::pystr::{self, chars, is_space, strip};
use crate::re::{Match, Regex};
use std::collections::HashMap;
use std::sync::OnceLock;

pub type RefDefs = HashMap<Vec<char>, (Vec<char>, Option<Vec<char>>)>;

#[derive(Clone, Debug)]
pub enum Inline {
    Raw(Vec<char>),
    Literal(Vec<char>),
    LineBreak(bool),
    Html(Vec<char>),
    Code(Vec<char>),
    AutoLink { dest: Vec<char>, text: Vec<char> },
    Url { dest: Vec<char>, text: Vec<char> },
    Link { image: bool, dest: Vec<char>, title: Option<Vec<char>>, children: Vec<Inline> },
    Emph(Vec<Inline>),
    Strong(Vec<Inline>),
    Strike(Vec<Inline>),
    FootnoteRef(Vec<char>),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum IEl {
    AutoLink,
    CodeSpan,
    InlineHtml,
    LineBreak,
    Literal,
    Strike,
    Url,
    FootnoteRef,
}

impl IEl {
    fn priority(self) -> u8 {
        match self {
            IEl::Literal | IEl::InlineHtml | IEl::CodeSpan | IEl::AutoLink => 7,
            IEl::LineBreak => 2,
            IEl::Strike | IEl::Url => 5,
            IEl::FootnoteRef => 6,
        }
    }
}

macro_rules! re {
    ($name:ident, $pat:expr) => {
        fn $name() -> &'static Regex {
            static R: OnceLock<Regex> = OnceLock::new();
            R.get_or_init(|| Regex::new(&$pat))
        }
    };
}

pub const TAG_NAME: &str = r"[A-Za-z][A-Za-z0-9\-]*";
pub const ATTRIBUTE: &str = r#"\s+[A-Za-z:_][A-Za-z0-9\-_\.:]*(?:\s*=\s*(?:[^\s"'`=<>]+|'[^']*'|"[^"]*"))?"#;
pub const ATTRIBUTE_NO_LF: &str =
    r#"[^\n\S]+[A-Za-z:_][A-Za-z0-9\-_\.:]*(?:[^\n\S]*=[^\n\S]*(?:[^\s"'`=<>]+|'[^\n']*'|"[^\n"]*"))?"#;
const URI: &str = r"[A-Za-z][A-Za-z\-.+]{1,31}:[^\s<>]*?";
const EMAIL: &str = r"[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*";
const URL_EMAIL: &str = r"[\w.\-+]+@[\w.\-]*?\.[\w.\-]*[a-zA-Z0-9]";

re!(literal_re, r##"\\([!"#\$%&'()*+,\-./:;<=>?@\[\\\]^_`{|}~])"##);
re!(linebreak_re, r"( *|\\)\n(?!\Z)");
re!(
    inline_html_re,
    format!(
        r"(<{t}(?:{a})* */?>|</{t} *>|<!--(?:>|->|[\s\S]*?-->)|<\?[\s\S]*?\?>|<![A-Z]+ +[\s\S]*?>|<!\[CDATA\[[\s\S]*?\]\]>)",
        t = TAG_NAME,
        a = ATTRIBUTE
    )
);
re!(code_re, r"(?<!`)(`+)(?!`)([\s\S]+?)(?<!`)\1(?!`)");
re!(autolink_re, format!("<({}|{})>", URI, EMAIL));
re!(email_re, EMAIL);
re!(strike_re, r"(?<!~)(~|~~)([^~]+)\1(?!~)");
re!(www_re, r"(?:^|(?<=[\s*_~(\uff00-\uffef]))(www\.([\w.\-]*?\.[\w.\-]+)[^<\s]*)");
re!(
    bare_re,
    format!(
        r"(?:^|(?<=[\s*_~(\uff00-\uffef]))((?:https?|ftp)://([\w.\-]*?\.[\w.\-]+)[^<\s]*|{}(?=[\s.<]|\Z))",
        URL_EMAIL
    )
);
re!(entity_tail_re, r"&[a-zA-Z]+;$");
re!(fnref_re, r"\[\^([^\]]+)\]");
re!(delim_re, r"(?:!?\[|\*+|_+)");
re!(ws_run_re, r"\s+");

pub fn strip_backslash(s: &[char]) -> Vec<char> {
    let r = literal_re();
    r.sub_with(s, |m| m.group(s, 1).unwrap().to_vec())
}

pub fn normalize_label(label: &[char]) -> Vec<char> {
    let s = ws_run_re().sub_str(label, " ");
    pystr::casefold(strip(&s))
}

pub fn email_prefix(s: &[char]) -> bool {
    email_re().is_match_at(s, 0)
}

/// `helpers.find_next`.
pub fn find_next(text: &[char], target: &[char], start: usize, end: Option<usize>, disallowed: &[char]) -> isize {
    let end = end.unwrap_or(text.len());
    let mut i = start;
    let mut escaped = false;
    while i < end {
        let c = text[i];
        if escaped {
            escaped = false;
        } else if target.contains(&c) {
            return i as isize;
        } else if disallowed.contains(&c) {
            return -2;
        } else if c == '\\' {
            escaped = true;
        }
        i += 1;
    }
    -1
}

pub fn is_paired(text: &[char], open: char, close: char) -> bool {
    let mut count = 0i64;
    let mut escape = false;
    for &c in text {
        if escape {
            escape = false;
        } else if c == '\\' {
            escape = true;
        } else if c == open {
            count += 1;
        } else if c == close {
            if count == 0 {
                return false;
            }
            count -= 1;
        }
    }
    count == 0
}

/// `helpers.partition_by_spaces` with the default `" \t"`.
pub fn partition_by_spaces(text: &[char]) -> (Vec<char>, Vec<char>, Vec<char>) {
    let mut start: isize = -1;
    let mut end: isize = -1;
    for (i, &c) in text.iter().enumerate() {
        if c == ' ' || c == '\t' {
            if start >= 0 {
                continue;
            }
            start = i as isize;
        } else if start >= 0 {
            end = i as isize;
            break;
        }
    }
    if start < 0 {
        return (text.to_vec(), vec![], vec![]);
    }
    let s = start as usize;
    if end < 0 {
        return (text[..s].to_vec(), text[s..].to_vec(), vec![]);
    }
    let e = end as usize;
    (text[..s].to_vec(), text[s..e].to_vec(), text[e..].to_vec())
}

#[derive(Clone, Debug)]
pub struct Group {
    pub start: isize,
    pub end: isize,
    pub text: Option<Vec<char>>,
}

pub fn empty_group() -> Group {
    Group { start: -1, end: -1, text: None }
}

fn group(text: &[char], a: usize, b: usize) -> Group {
    Group { start: a as isize, end: b as isize, text: Some(text[a..b].to_vec()) }
}

const WHITESPACE: [char; 3] = [' ', '\n', '\t'];

fn is_ascii_control(c: char) -> bool {
    (c as u32) < 32 || c as u32 == 127
}

pub fn parse_link_separator(text: &[char], start: usize) -> usize {
    let mut i = start;
    let mut has_newline = false;
    while i < text.len() {
        if text[i] == '\n' {
            if has_newline {
                break;
            }
            has_newline = true;
        } else if !WHITESPACE.contains(&text[i]) {
            break;
        }
        i += 1;
    }
    i
}

pub fn parse_link_label(text: &[char], start: usize) -> Option<Group> {
    if text.get(start) != Some(&'[') {
        return None;
    }
    let i = find_next(text, &[']'], start + 1, None, &['[']);
    if i < 0 {
        return None;
    }
    let i = i as usize;
    let label = &text[start + 1..i];
    if strip(label).is_empty() || label.len() > 999 {
        return None;
    }
    Some(group(text, start, i + 1))
}

pub struct ParseError;

pub fn parse_link_dest_title(text: &[char], start: usize, is_inline: bool) -> Result<(Group, Group), ParseError> {
    if start >= text.len() {
        return Err(ParseError);
    }
    let link_dest;
    let mut i;
    if text[start] == '<' {
        let rb = find_next(text, &['>'], start + 1, None, &['<', '\n']);
        if rb < 0 {
            return Err(ParseError);
        }
        i = rb as usize + 1;
        link_dest = group(text, start, i);
    } else {
        let mut escaped = false;
        let mut pairs = 0i64;
        i = start;
        let mut broke = false;
        while i < text.len() {
            let c = text[i];
            if escaped {
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if WHITESPACE.contains(&c) {
                broke = true;
                break;
            } else if is_ascii_control(c) {
                return Err(ParseError);
            } else if c == '(' {
                pairs += 1;
            } else if c == ')' {
                if pairs > 0 {
                    pairs -= 1;
                } else if is_inline {
                    return Ok((group(text, start, i), empty_group()));
                } else {
                    return Err(ParseError);
                }
            }
            i += 1;
        }
        if !broke && is_inline {
            return Err(ParseError);
        }
        link_dest = group(text, start, i);
        if link_dest.text.as_ref().unwrap().is_empty() {
            return Err(ParseError);
        }
    }
    let prev = i;
    i = parse_link_separator(text, i);
    if i >= text.len() || text[i] == '\n' || (text[i] == ')' && is_inline) {
        return Ok((link_dest, empty_group()));
    }
    let end = if text[i] == '"' {
        find_next(text, &['"'], i + 1, None, &[])
    } else if text[i] == '\'' {
        find_next(text, &['\''], i + 1, None, &[])
    } else if text[i] == '(' {
        find_next(text, &[')'], i + 1, None, &['('])
    } else if text[prev..i].contains(&'\n') {
        return Ok((link_dest, empty_group()));
    } else {
        return Err(ParseError);
    };
    if 0 < i && i < text.len() && !WHITESPACE.contains(&text[i - 1]) {
        return Err(ParseError);
    }
    if end < 0 {
        return Err(ParseError);
    }
    let end = end as usize;
    if pystr::contains(&text[i..end], &['\n', '\n']) {
        return Err(ParseError);
    }
    Ok((link_dest, group(text, i, end + 1)))
}

fn expect_inline_link(text: &[char], start: usize) -> Option<(Group, Group, usize)> {
    if start as isize >= text.len() as isize - 1 || text[start] != '(' {
        return None;
    }
    let i = parse_link_separator(text, start + 1);
    let (dest, title) = parse_link_dest_title(text, i, true).ok()?;
    let end = dest.end.max(title.end) as usize;
    let end = parse_link_separator(text, end);
    if end >= text.len() || text[end] != ')' {
        return None;
    }
    Some((dest, title, end + 1))
}

fn expect_reference_link(text: &[char], start: usize, link_text: &[char], defs: &RefDefs) -> Option<(Group, Group, usize)> {
    let mut link_label = parse_link_label(text, start);
    let mut label = link_text.to_vec();
    if let Some(l) = &link_label {
        let t = l.text.as_ref().unwrap();
        let inner = &t[1..t.len() - 1];
        if !inner.is_empty() {
            label = inner.to_vec();
        }
    } else if text.len() >= start + 2 && text[start] == '[' && text[start + 1] == ']' {
        link_label = Some(Group { start: start as isize, end: start as isize + 2, text: Some(chars("[]")) });
    }
    let (dest, title) = defs.get(&normalize_label(&label))?;
    let s = start as isize;
    let end = link_label.map(|l| l.end as usize).unwrap_or(start);
    Some((Group { start: s, end: s, text: Some(dest.clone()) }, Group { start: s, end: s, text: title.clone() }, end))
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum MKind {
    Link,
    Image,
    Strong,
    Emph,
}

#[derive(Clone, Debug)]
struct MObj {
    kind: MKind,
    start: usize,
    end: usize,
    groups: Vec<Group>,
}

struct Delimiter {
    start: usize,
    end: usize,
    ch: char,
    len: usize,
    bracket: bool,
    image: bool,
    active: bool,
    can_open: bool,
    can_close: bool,
}

fn followed_by_punc(text: &[char], end: usize) -> bool {
    end < text.len() && pystr::is_punctuation(text[end])
}

fn preceded_by_punc(text: &[char], start: usize) -> bool {
    start > 0 && pystr::is_punctuation(text[start - 1])
}

impl Delimiter {
    fn new(text: &[char], m: &Match) -> Delimiter {
        let (start, end) = (m.start(), m.end());
        let c = text[start];
        let mut d = Delimiter {
            start,
            end,
            ch: c,
            len: end - start,
            bracket: c == '[' || c == '!',
            image: c == '!',
            active: true,
            can_open: false,
            can_close: false,
        };
        if c == '*' || c == '_' {
            let lf = d.is_left_flanking(text);
            let rf = d.is_right_flanking(text);
            if c == '*' {
                d.can_open = lf;
                d.can_close = rf;
            } else {
                d.can_open = lf && (!rf || preceded_by_punc(text, start));
                d.can_close = rf && (!lf || followed_by_punc(text, end));
            }
        }
        d
    }

    fn is_left_flanking(&self, text: &[char]) -> bool {
        (self.end < text.len() && !is_space(text[self.end]))
            && (!followed_by_punc(text, self.end)
                || self.start == 0
                || preceded_by_punc(text, self.start)
                || is_space(text[self.start - 1]))
    }

    fn is_right_flanking(&self, text: &[char]) -> bool {
        (self.start > 0 && !is_space(text[self.start - 1]))
            && (!preceded_by_punc(text, self.start)
                || self.end == text.len()
                || followed_by_punc(text, self.end)
                || is_space(text[self.end]))
    }

    fn closed_by(&self, other: &Delimiter) -> bool {
        !(self.ch != other.ch
            || ((self.can_open && self.can_close) || (other.can_open && other.can_close))
                && (self.len + other.len) % 3 == 0
                && !(self.len % 3 == 0 && other.len % 3 == 0))
    }

    fn remove(&mut self, n: usize, left: bool) -> bool {
        if self.len <= n {
            return true;
        }
        if left {
            self.start += n;
        } else {
            self.end -= n;
        }
        self.len -= n;
        false
    }
}

fn find_links_or_emphs(text: &[char], defs: &RefDefs) -> Vec<MObj> {
    let mut i = 0;
    let mut delimiters: Vec<Delimiter> = Vec::new();
    let mut escape = false;
    let mut matches = Vec::new();
    while i < text.len() {
        if escape {
            escape = false;
            i += 1;
        } else if text[i] == '\\' {
            escape = true;
            i += 1;
        } else if let Some(m) = code_re().match_at(text, i) {
            i = m.end();
        } else if text[i] == ']' {
            match look_for_image_or_link(text, &mut delimiters, i, defs, &mut matches) {
                Some(node) => {
                    i = node.end;
                    matches.push(node);
                }
                None => i += 1,
            }
        } else if let Some(m) = delim_re().match_at(text, i) {
            delimiters.push(Delimiter::new(text, &m));
            i = m.end();
        } else {
            i += 1;
        }
    }
    process_emphasis(text, &mut delimiters, -1, &mut matches);
    matches
}

fn look_for_image_or_link(
    text: &[char],
    delimiters: &mut Vec<Delimiter>,
    close: usize,
    defs: &RefDefs,
    matches: &mut Vec<MObj>,
) -> Option<MObj> {
    let mut idx = None;
    for i in (0..delimiters.len()).rev() {
        let d = &delimiters[i];
        if !d.bracket {
            continue;
        }
        idx = Some(i);
        if !d.active {
            break;
        }
        if !is_paired(&text[d.end..close], '[', ']') {
            break;
        }
        let link_text = group(text, d.end, close);
        let image = d.image;
        let found = expect_inline_link(text, close + 1)
            .or_else(|| expect_reference_link(text, close + 1, link_text.text.as_ref().unwrap(), defs));
        let (dest, title, end) = match found {
            Some(f) => f,
            None => break,
        };
        let rv = MObj {
            kind: if image { MKind::Image } else { MKind::Link },
            start: d.start,
            end,
            groups: vec![link_text, dest, title],
        };
        process_emphasis(text, delimiters, i as isize, matches);
        if !image {
            for d in delimiters[..i].iter_mut() {
                if d.bracket && !d.image {
                    d.active = false;
                }
            }
        }
        delimiters.remove(i);
        return Some(rv);
    }
    if let Some(i) = idx {
        delimiters.remove(i);
    }
    None
}

fn next_closer(delimiters: &[Delimiter], bound: isize) -> isize {
    let mut i = (bound + 1) as usize;
    while i < delimiters.len() {
        let d = &delimiters[i];
        if !d.bracket && d.can_close {
            return i as isize;
        }
        i += 1;
    }
    -1
}

fn nearest_opener(delimiters: &[Delimiter], higher: usize, lower: isize) -> isize {
    let mut i = higher as isize - 1;
    while i > lower {
        let d = &delimiters[i as usize];
        if !d.bracket && d.can_open && d.closed_by(&delimiters[higher]) {
            return i;
        }
        i -= 1;
    }
    -1
}

fn process_emphasis(text: &[char], delimiters: &mut Vec<Delimiter>, stack_bottom: isize, matches: &mut Vec<MObj>) {
    let mut star_bottom = stack_bottom;
    let mut underscore_bottom = stack_bottom;
    let mut cur = next_closer(delimiters, stack_bottom);
    while cur >= 0 {
        let c = cur as usize;
        let bottom = if delimiters[c].ch == '*' { star_bottom } else { underscore_bottom };
        let opener = nearest_opener(delimiters, c, bottom);
        if opener >= 0 {
            let o = opener as usize;
            let n = if delimiters[o].len >= 2 && delimiters[c].len >= 2 { 2 } else { 1 };
            let (oe, cs) = (delimiters[o].end, delimiters[c].start);
            matches.push(MObj {
                kind: if n == 2 { MKind::Strong } else { MKind::Emph },
                start: oe - n,
                end: cs + n,
                groups: vec![group(text, oe, cs)],
            });
            delimiters.drain(o + 1..c);
            let mut cur2 = o + 1;
            if delimiters[o].remove(n, false) {
                delimiters.remove(o);
                cur2 -= 1;
            }
            if delimiters[cur2].remove(n, true) {
                delimiters.remove(cur2);
            }
            cur = if cur2 > 0 { cur2 as isize - 1 } else { -1 };
        } else {
            let b = if c > 1 { c as isize - 1 } else { -1 };
            if delimiters[c].ch == '*' {
                star_bottom = b;
            } else {
                underscore_bottom = b;
            }
            if !delimiters[c].can_open {
                delimiters.remove(c);
                cur = if c > 0 { c as isize - 1 } else { -1 };
            }
        }
        cur = next_closer(delimiters, cur);
    }
    let lower = (stack_bottom + 1) as usize;
    if lower < delimiters.len() {
        delimiters.truncate(lower);
    }
}

enum Pay {
    Obj(MObj),
    Re(IEl, Match, usize),
}

struct Token {
    pay: Pay,
    priority: u8,
    parse_children: bool,
    start: usize,
    end: usize,
    inner_start: usize,
    inner_end: usize,
    children: Vec<Token>,
}

const PRECEDE: u8 = 0;
const INTERSECT: u8 = 1;
const CONTAIN: u8 = 2;
const SHADE: u8 = 3;

impl Token {
    fn relation(&self, other: &Token) -> u8 {
        if self.end <= other.start {
            return PRECEDE;
        }
        if self.end >= other.end {
            if self.parse_children && other.start >= self.inner_start && other.end <= self.inner_end {
                return CONTAIN;
            }
            if self.parse_children && self.inner_end <= other.start {
                return SHADE;
            }
        }
        INTERSECT
    }
}

fn resolve_overlap(tokens: Vec<Token>) -> Vec<Token> {
    let mut it = tokens.into_iter();
    let mut prev = match it.next() {
        Some(t) => t,
        None => return Vec::new(),
    };
    let mut result = Vec::new();
    for cur in it {
        let r = prev.relation(&cur);
        if r == PRECEDE {
            result.push(std::mem::replace(&mut prev, cur));
        } else if r == CONTAIN {
            if prev.parse_children {
                prev.children.push(cur);
            }
        } else if r == INTERSECT && prev.priority < cur.priority {
            prev = cur;
        }
    }
    result.push(prev);
    result
}

fn make_elements(tokens: Vec<Token>, text: &[char], start: usize, end: usize) -> Vec<Inline> {
    let end = if end == 0 { text.len() } else { end };
    let mut result = Vec::new();
    let mut prev_end = start;
    for t in tokens {
        if prev_end < t.start {
            result.push(Inline::Raw(text[prev_end..t.start].to_vec()));
        }
        let e = t.end;
        result.push(as_element(t, text));
        prev_end = e;
    }
    if prev_end < end {
        result.push(Inline::Raw(text[prev_end..end].to_vec()));
    }
    result
}

fn link_parts(m: &MObj) -> (Vec<char>, Option<Vec<char>>) {
    let g2 = m.groups[1].text.clone();
    let dest = match &g2 {
        Some(g) if !g.is_empty() && g[0] == '<' && *g.last().unwrap() == '>' => g[1..g.len() - 1].to_vec(),
        Some(g) => g.clone(),
        None => vec![],
    };
    let dest = strip_backslash(&dest);
    let title = match &m.groups[2].text {
        Some(t) if !t.is_empty() => Some(strip_backslash(&t[1..t.len() - 1])),
        _ => None,
    };
    (dest, title)
}

fn as_element(t: Token, text: &[char]) -> Inline {
    let (is, ie) = (t.inner_start, t.inner_end);
    let kids = |children: Vec<Token>| make_elements(resolve_overlap(children), text, is, ie);
    match t.pay {
        Pay::Obj(m) => match m.kind {
            MKind::Link | MKind::Image => {
                let (dest, title) = link_parts(&m);
                Inline::Link { image: m.kind == MKind::Image, dest, title, children: kids(t.children) }
            }
            MKind::Strong => Inline::Strong(kids(t.children)),
            MKind::Emph => Inline::Emph(kids(t.children)),
        },
        Pay::Re(el, m, end) => {
            let g = |n: usize| m.group(text, n).unwrap_or(&[]).to_vec();
            match el {
                IEl::Literal => Inline::Literal(g(1)),
                IEl::LineBreak => {
                    let g1 = g(1);
                    Inline::LineBreak(!(g1.starts_with(&[' ', ' ']) || g1.starts_with(&['\\'])))
                }
                IEl::InlineHtml => Inline::Html(g(1)),
                IEl::CodeSpan => {
                    let mut c = pystr::replace(&g(2), &['\n'], &[' ']);
                    if !strip(&c).is_empty() && c[0] == ' ' && *c.last().unwrap() == ' ' {
                        c = c[1..c.len() - 1].to_vec();
                    }
                    Inline::Code(c)
                }
                IEl::AutoLink => {
                    let g1 = g(1);
                    let dest = if email_prefix(&g1) { [chars("mailto:"), g1.clone()].concat() } else { g1.clone() };
                    Inline::AutoLink { dest, text: g1 }
                }
                IEl::Url => {
                    let (s1, e1) = m.span(1).unwrap();
                    let g1 = text[s1..e1.min(end).max(s1)].to_vec();
                    let mut dest = if email_prefix(&g1) { [chars("mailto:"), g1.clone()].concat() } else { g1.clone() };
                    if www_re().is_match_at(&dest, 0) {
                        dest = [chars("http://"), dest].concat();
                    }
                    Inline::Url { dest, text: g1 }
                }
                IEl::Strike => Inline::Strike(kids(t.children)),
                IEl::FootnoteRef => Inline::FootnoteRef(normalize_label(&g(1))),
            }
        }
    }
}

fn url_matches(text: &[char]) -> Vec<(Match, usize)> {
    let mut out = Vec::new();
    let all = www_re().find_iter(text).into_iter().chain(bare_re().find_iter(text));
    for m in all {
        if let Some(domain) = m.group(text, 2) {
            if !domain.is_empty() {
                let parts: Vec<&[char]> = domain.split(|&c| c == '.').collect();
                let n = parts.len();
                if n < 2 || parts[n - 2..].iter().any(|p| p.contains(&'_')) {
                    continue;
                }
            }
        }
        let lt = m.group(text, 0).unwrap();
        let last = *lt.last().unwrap();
        let mut end = m.end();
        if "?!.,:*_~".contains(last) {
            end -= 1;
        } else if last == ')' && lt.iter().filter(|&&c| c == ')').count() > lt.iter().filter(|&&c| c == '(').count() {
            let shift = lt.iter().filter(|&&c| c == ')').count() - lt.iter().filter(|&&c| c == '(').count();
            end = end.saturating_sub(shift);
        } else if let Some(t) = entity_tail_re().search(lt, 0) {
            end -= t.end() - t.start();
        }
        out.push((m, end));
    }
    out
}

pub struct InlineCtx<'a> {
    pub defs: &'a RefDefs,
    pub footnotes: &'a HashMap<Vec<char>, usize>,
    pub elements: &'a [IEl],
}

pub fn parse(text: &[char], cx: &InlineCtx) -> Vec<Inline> {
    let mut tokens = Vec::new();
    for m in find_links_or_emphs(text, cx.defs) {
        let (is, ie) = (m.groups[0].start as usize, m.groups[0].end as usize);
        tokens.push(Token {
            start: m.start,
            end: m.end,
            inner_start: is,
            inner_end: ie,
            priority: 5,
            parse_children: true,
            children: Vec::new(),
            pay: Pay::Obj(m),
        });
    }
    for &el in cx.elements {
        let found: Vec<(Match, usize)> = match el {
            IEl::AutoLink => autolink_re().find_iter(text).into_iter().map(|m| { let e = m.end(); (m, e) }).collect(),
            IEl::CodeSpan => code_re().find_iter(text).into_iter().map(|m| { let e = m.end(); (m, e) }).collect(),
            IEl::InlineHtml => inline_html_re().find_iter(text).into_iter().map(|m| { let e = m.end(); (m, e) }).collect(),
            IEl::LineBreak => {
                if !text.contains(&'\n') {
                    Vec::new()
                } else {
                    linebreak_re().find_iter(text).into_iter().map(|m| { let e = m.end(); (m, e) }).collect()
                }
            }
            IEl::Literal => literal_re().find_iter(text).into_iter().map(|m| { let e = m.end(); (m, e) }).collect(),
            IEl::Strike => strike_re().find_iter(text).into_iter().map(|m| { let e = m.end(); (m, e) }).collect(),
            IEl::Url => url_matches(text),
            IEl::FootnoteRef => fnref_re()
                .find_iter(text)
                .into_iter()
                .filter(|m| cx.footnotes.contains_key(&normalize_label(m.group(text, 1).unwrap())))
                .map(|m| { let e = m.end(); (m, e) })
                .collect(),
        };
        let group_n = if el == IEl::Strike { 2 } else { 1 };
        for (m, end) in found {
            let start = m.start();
            let (gs, ge) = m.span(group_n).unwrap();
            let (is, ie) = (gs.max(start), ge.min(end));
            tokens.push(Token {
                start,
                end,
                inner_start: is,
                inner_end: ie,
                priority: el.priority(),
                parse_children: el == IEl::Strike,
                children: Vec::new(),
                pay: Pay::Re(el, m, end),
            });
        }
    }
    tokens.sort_by_key(|t| t.start);
    let tokens = resolve_overlap(tokens);
    make_elements(tokens, text, 0, 0)
}
