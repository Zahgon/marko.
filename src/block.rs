//! Block parsing: `marko.source`, `marko.block`, `marko.parser` and the
//! block elements of the gfm and footnote extensions.

use crate::inline::{self, find_next, normalize_label, parse_link_dest_title, parse_link_label, parse_link_separator, Group, IEl, Inline, RefDefs};
use crate::pystr::{self, chars, expandtabs, isdigit, lstrip, rstrip, strip};
use crate::re::{self, Match, Regex};
use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Clone, Copy, PartialEq, Debug)]
enum PClass {
    Space,
    WsNoNl,
    Lit(char),
}

#[derive(Clone, Copy, Debug)]
struct PItem {
    class: PClass,
    min: usize,
    max: usize,
}

type Prefix = Vec<PItem>;

fn pi(class: PClass, min: usize, max: usize) -> PItem {
    PItem { class, min, max }
}

fn quote_prefix() -> Prefix {
    vec![pi(PClass::Space, 0, 3), pi(PClass::Lit('>'), 1, 1), pi(PClass::WsNoNl, 0, 1)]
}

fn lit_prefix(s: &[char]) -> Prefix {
    s.iter().map(|&c| pi(PClass::Lit(c), 1, 1)).collect()
}

fn spaces(n: usize) -> Prefix {
    if n == 0 {
        vec![]
    } else {
        vec![pi(PClass::Space, n, n)]
    }
}

fn class_ok(c: PClass, ch: char) -> bool {
    match c {
        PClass::Space => ch == ' ',
        PClass::WsNoNl => ch != '\n' && pystr::is_space(ch),
        PClass::Lit(l) => ch == l,
    }
}

/// `re.match(prefix, s)`: end of the first match in backtracking order.
fn prefix_match(items: &[PItem], s: &[char], pos: usize) -> Option<usize> {
    if items.is_empty() {
        return Some(pos);
    }
    let it = items[0];
    let mut n = 0;
    while n < it.max && pos + n < s.len() && class_ok(it.class, s[pos + n]) {
        n += 1;
    }
    if n < it.min {
        return None;
    }
    let mut c = n;
    loop {
        if let Some(e) = prefix_match(&items[1..], s, pos + c) {
            return Some(e);
        }
        if c == it.min {
            return None;
        }
        c -= 1;
    }
}

/// `Source.match_prefix`.
fn match_prefix(prefix: &[PItem], line: &[char]) -> isize {
    let exp = expandtabs(line);
    let pos = match prefix_match(prefix, &exp, 0) {
        Some(p) => p,
        None => {
            let mut padded = Vec::with_capacity(exp.len() + 99);
            for &c in &exp {
                if c == '\n' {
                    padded.extend(std::iter::repeat(' ').take(99));
                }
                padded.push(c);
            }
            if prefix_match(prefix, &padded, 0).is_some() {
                return line.len() as isize - 1;
            }
            return -1;
        }
    };
    if pos == 0 {
        return 0;
    }
    for i in 1..=line.len() {
        if pystr::expanded_len(&line[..i]) >= pos {
            return i as isize;
        }
    }
    -1
}

fn strip_prefix(line: &[char], prefix: &[PItem]) -> Vec<char> {
    let end = match prefix_match(prefix, &expandtabs(line), 0) {
        Some(e) => e,
        None => return vec![],
    };
    for i in 0..line.len() {
        let expanded = expandtabs(&line[..i + 1]);
        if expanded.len() < end {
            continue;
        }
        let d = expanded.len() - end;
        if d == 0 {
            return line[i + 1..].to_vec();
        }
        let mut out = expanded[expanded.len() - d..].to_vec();
        out.extend_from_slice(&line[i + 1..]);
        return out;
    }
    vec![]
}

#[derive(Clone, Debug)]
pub enum Block {
    BlankLine(usize),
    Heading { level: usize, body: Vec<char>, children: Vec<Inline> },
    Code { lang: Vec<char>, code: Vec<char> },
    ThematicBreak,
    Html(Vec<char>),
    Paragraph { body: Vec<char>, tight: bool, checked: Option<bool>, children: Vec<Inline> },
    Quote(Vec<Block>),
    Alert { kind: Vec<char>, children: Vec<Block> },
    List { ordered: bool, start: u64, items: Vec<Block> },
    ListItem(Vec<Block>),
    LinkRefDef,
    Table(Vec<Block>),
    TableRow(Vec<Block>),
    TableCell { body: Vec<char>, header: bool, align: Option<&'static str>, children: Vec<Inline> },
    FootnoteDef(usize),
}

pub struct FootnoteDef {
    pub label: Vec<char>,
    pub children: Vec<Block>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum El {
    BlankLine,
    CodeBlock,
    FencedCode,
    HtmlBlock,
    Heading,
    LinkRefDef,
    List,
    Paragraph,
    Quote,
    ThematicBreak,
    Table,
    Alert,
    FootnoteDef,
}

impl El {
    fn priority(self) -> u8 {
        match self {
            El::ThematicBreak => 8,
            El::FencedCode | El::Alert => 7,
            El::Heading | El::List | El::Quote | El::FootnoteDef => 6,
            El::BlankLine | El::HtmlBlock | El::LinkRefDef | El::Table => 5,
            El::CodeBlock => 4,
            El::Paragraph => 1,
        }
    }
    fn breaks_paragraph(self) -> bool {
        matches!(self, El::BlankLine | El::FencedCode | El::Heading | El::Quote | El::Alert)
    }
}

#[derive(Clone, Debug)]
enum Kind {
    Document,
    Quote,
    Alert,
    List { bullet: Vec<char>, ordered: bool },
    ListItem,
    Table { aligns: Vec<Option<&'static str>> },
    FootnoteDef,
}

struct State {
    kind: Kind,
    prefix: Prefix,
    second: Option<Prefix>,
}

#[derive(Default)]
struct Ctx {
    code_line: Vec<char>,
    code_info: (Vec<char>, Vec<char>, Vec<char>, Vec<char>),
    html_end: Option<Regex>,
    list_info: (Vec<char>, bool, u64),
    list_item_info: (usize, Vec<char>, usize),
    linkref_info: Option<(Group, Group, Group, usize)>,
    cells: Vec<Vec<char>>,
    is_delimiter: bool,
    table_info: (Vec<Vec<char>>, Vec<Vec<char>>),
}

pub struct Config {
    pub gfm: bool,
    pub block_elements: Vec<El>,
    pub breaking: Vec<El>,
    pub inline_elements: Vec<IEl>,
}

impl Config {
    pub fn new(ext_elements: &[(Option<El>, Option<IEl>)], gfm: bool) -> Config {
        let base = [
            El::BlankLine,
            El::CodeBlock,
            El::FencedCode,
            El::HtmlBlock,
            El::Heading,
            El::LinkRefDef,
            El::List,
            El::Paragraph,
            El::Quote,
            El::ThematicBreak,
        ];
        let mut dict: Vec<El> = base.to_vec();
        let mut inl = vec![IEl::AutoLink, IEl::CodeSpan, IEl::InlineHtml, IEl::LineBreak, IEl::Literal];
        for (b, i) in ext_elements {
            if let Some(b) = b {
                if !dict.contains(b) {
                    dict.push(*b);
                }
            }
            if let Some(i) = i {
                if !inl.contains(i) {
                    inl.push(*i);
                }
            }
        }
        let breaking = dict.iter().copied().filter(|e| e.breaks_paragraph()).collect();
        let mut sorted = dict.clone();
        sorted.sort_by(|a, b| b.priority().cmp(&a.priority()));
        Config { gfm, block_elements: sorted, breaking, inline_elements: inl }
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

const TAGS: [&str; 63] = [
    "address", "article", "aside", "base", "basefont", "blockquote", "body", "caption", "center", "col", "colgroup", "dd",
    "details", "dialog", "dir", "div", "dl", "dt", "fieldset", "figcaption", "figure", "footer", "form", "frame",
    "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", "hr", "html", "iframe", "legend", "li", "link",
    "main", "menu", "menuitem", "meta", "nav", "noframes", "ol", "optgroup", "option", "p", "param", "section",
    "source", "summary", "table", "tbody", "td", "tfoot", "th", "thead", "title", "tr", "track", "ul",
];

re!(heading_re, r"(?m) {0,3}(#{1,6})((?=\s)[^\n]*?|[^\n\S]*)(?:(?<=\s)(?<!\\)#+)?[^\n\S]*$\n?");
re!(fence_re, r"(?m)( {,3})(`{3,}|~{3,})[^\n\S]*(.*?)$");
re!(fence_close_re, r"(?m) {,3}(~+|`+)[^\n\S]*$");
re!(tb_re, r"(?m) {,3}([-_*][^\n\S]*){3,}$\n?");
re!(html1_re, r"(?i) {,3}<(script|pre|style|textarea)[>\s]");
re!(html2_re, r" {,3}<!--");
re!(html3_re, r" {,3}<\?");
re!(html4_re, r" {,3}<!");
re!(html5_re, r" {,3}<!\[CDATA\[");
re!(html6_re, format!(r"(?im) {{,3}}</?(?:{}{}(?: +|/?>|$)", TAGS.join("|"), ")"));
re!(
    html7_re,
    format!(
        r"(?m) {{,3}}(<{t}(?:{a})*[^\n\S]*/?>|</{t}[^\n\S]*>)[^\n\S]*$",
        t = inline::TAG_NAME,
        a = inline::ATTRIBUTE_NO_LF
    )
);
re!(para_re, r"(?m)[^\n]+$\n?");
re!(setext_re, r" {,3}(=+|-+)[^\n\S]*$");
re!(quote_re, r" {,3}>");
re!(list_re, r" {,3}(\d{1,9}[.)]|[*\-+])[ \t\n\r\f]");
re!(lrd_re, r"(?m) {,3}(\[[\s\S]*?)(?=\n\n|\Z)");
re!(task_re, r"(\[[\sxX]\])\s+\S");
re!(splitter_re, r"\s*(?<!\\)\|\s*");
re!(delim_cell_re, r":?-+:?");
re!(row_start_re, r" {,3}\S");
re!(alert_re, r"(?im) {,3}>\s*\[\!(WARNING|NOTE|TIP|IMPORTANT|CAUTION)\]\s*$");
re!(fndef_re, r" {,3}\[\^([^\]]+)\]:[^\n\S]*(?=\S| {4})");
re!(ws_re, r"\s+");

pub struct Source<'c> {
    buf: Vec<char>,
    pos: usize,
    anchor: usize,
    states: Vec<usize>,
    arena: Vec<State>,
    m: Option<Match>,
    ctx: Ctx,
    cfg: &'c Config,
    pub link_ref_defs: RefDefs,
    pub footnotes: HashMap<Vec<char>, usize>,
    pub defs: Vec<FootnoteDef>,
}

fn line_end(buf: &[char], pos: usize) -> usize {
    match buf[pos..].iter().position(|&c| c == '\n') {
        Some(i) => pos + i + 1,
        None => buf.len(),
    }
}

fn line_match(buf: &[char], pos: usize) -> Match {
    Match { spans: vec![Some((pos, line_end(buf, pos)))] }
}

impl<'c> Source<'c> {
    pub fn new(text: &str, cfg: &'c Config) -> Source<'c> {
        let mut buf = Vec::with_capacity(text.len());
        let t: Vec<char> = text.chars().collect();
        let mut i = 0;
        while i < t.len() {
            let c = t[i];
            match c {
                '\r' => {
                    buf.push('\n');
                    if i + 1 < t.len() && t[i + 1] == '\n' {
                        i += 1;
                    }
                }
                '\x0c' => buf.push('\n'),
                '\0' => buf.push('\u{fffd}'),
                _ => buf.push(c),
            }
            i += 1;
        }
        Source {
            buf,
            pos: 0,
            anchor: 0,
            states: vec![],
            arena: vec![],
            m: None,
            ctx: Ctx::default(),
            cfg,
            link_ref_defs: HashMap::new(),
            footnotes: HashMap::new(),
            defs: vec![],
        }
    }

    fn exhausted(&self) -> bool {
        self.pos >= self.buf.len()
    }

    fn push_state(&mut self, kind: Kind, prefix: Prefix, second: Option<Prefix>) {
        self.arena.push(State { kind, prefix, second });
        self.states.push(self.arena.len() - 1);
    }

    fn pop_state(&mut self) {
        self.states.pop();
    }

    fn top(&self) -> &Kind {
        &self.arena[*self.states.last().unwrap()].kind
    }

    fn prefix(&self) -> Prefix {
        self.states.iter().flat_map(|&s| self.arena[s].prefix.iter().copied()).collect()
    }

    fn next_line_raw(&mut self) -> Vec<char> {
        let m = line_match(&self.buf, self.pos);
        let (a, b) = m.spans[0].unwrap();
        self.m = Some(m);
        self.buf[a..b].to_vec()
    }

    fn prefix_len(&mut self) -> isize {
        let line = self.next_line_raw();
        match_prefix(&self.prefix(), &line)
    }

    fn expect(&mut self, r: &Regex) -> Option<Match> {
        let pl = self.prefix_len();
        if pl < 0 {
            return None;
        }
        let m = r.match_at(&self.buf, self.pos + pl as usize);
        self.m = m.clone();
        m
    }

    fn next_line(&mut self) -> Option<Vec<char>> {
        let pl = self.prefix_len();
        if pl < 0 {
            self.m = None;
            return None;
        }
        let m = line_match(&self.buf, self.pos + pl as usize);
        let (a, b) = m.spans[0].unwrap();
        self.m = Some(m);
        Some(self.buf[a..b].to_vec())
    }

    fn consume(&mut self) {
        if let Some(m) = self.m.take() {
            let (a, b) = m.spans[0].unwrap();
            self.pos = b;
            if b > a && self.buf[b - 1] == '\n' {
                for &s in &self.states {
                    if let Some(sec) = &self.arena[s].second {
                        self.arena[s].prefix = sec.clone();
                    }
                }
            }
        }
    }

    fn set_anchor(&mut self) {
        self.anchor = self.pos;
    }

    fn reset(&mut self) {
        self.pos = self.anchor;
    }

    fn group(&self, n: usize) -> Vec<char> {
        self.m.as_ref().unwrap().group(&self.buf, n).unwrap_or(&[]).to_vec()
    }

    pub fn parse_document(&mut self) -> Vec<Block> {
        self.push_state(Kind::Document, vec![], None);
        let mut children = self.parse_source();
        let cx_defs = std::mem::take(&mut self.link_ref_defs);
        let footnotes = std::mem::take(&mut self.footnotes);
        let mut defs = std::mem::take(&mut self.defs);
        {
            let icx = inline::InlineCtx { defs: &cx_defs, footnotes: &footnotes, elements: &self.cfg.inline_elements };
            for b in children.iter_mut() {
                parse_inline(b, &icx, &mut defs);
            }
        }
        self.link_ref_defs = cx_defs;
        self.footnotes = footnotes;
        self.defs = defs;
        self.pop_state();
        children
    }

    fn parse_source(&mut self) -> Vec<Block> {
        let list = self.cfg.block_elements.clone();
        let mut ast = Vec::new();
        while !self.exhausted() {
            let mut hit = false;
            for &e in &list {
                if self.el_match(e) {
                    let b = self.el_parse(e);
                    ast.push(b);
                    hit = true;
                    break;
                }
            }
            if !hit {
                break;
            }
        }
        ast
    }

    fn el_match(&mut self, e: El) -> bool {
        match e {
            El::BlankLine => self.blank_match(),
            El::CodeBlock => self.code_match(),
            El::FencedCode => self.fence_match(),
            El::HtmlBlock => self.html_match() != 0,
            El::Heading => self.expect(heading_re()).is_some(),
            El::LinkRefDef => self.lrd_match(),
            El::List => self.list_match(),
            El::Paragraph => self.expect(para_re()).is_some(),
            El::Quote => self.expect(quote_re()).is_some(),
            El::ThematicBreak => self.tb_match(),
            El::Table => self.table_match(),
            El::Alert => self.expect(alert_re()).is_some(),
            El::FootnoteDef => self.expect(fndef_re()).is_some(),
        }
    }

    fn el_parse(&mut self, e: El) -> Block {
        match e {
            El::BlankLine => self.blank_parse(),
            El::CodeBlock => self.code_parse(),
            El::FencedCode => self.fence_parse(),
            El::HtmlBlock => self.html_parse(),
            El::Heading => {
                let level = self.group(1).len();
                let body = strip(&self.group(2)).to_vec();
                self.consume();
                Block::Heading { level, body, children: vec![] }
            }
            El::LinkRefDef => self.lrd_parse(),
            El::List => self.list_parse(),
            El::Paragraph => self.para_parse(),
            El::Quote => {
                self.push_state(Kind::Quote, quote_prefix(), None);
                let children = self.parse_source();
                self.pop_state();
                Block::Quote(children)
            }
            El::ThematicBreak => {
                self.consume();
                Block::ThematicBreak
            }
            El::Table => self.table_parse(),
            El::Alert => {
                let kind = pystr::upper(&self.group(1));
                self.next_line_raw();
                self.consume();
                self.push_state(Kind::Alert, quote_prefix(), None);
                let children = self.parse_source();
                self.pop_state();
                Block::Alert { kind, children }
            }
            El::FootnoteDef => {
                let label = normalize_label(&self.group(1));
                let whole = self.group(0);
                self.push_state(Kind::FootnoteDef, lit_prefix(&whole), Some(vec![pi(PClass::Space, 1, 4)]));
                let children = self.parse_source();
                self.pop_state();
                self.defs.push(FootnoteDef { label: label.clone(), children });
                let id = self.defs.len() - 1;
                self.footnotes.insert(label, id);
                Block::FootnoteDef(id)
            }
        }
    }

    fn blank_match(&mut self) -> bool {
        match self.next_line() {
            Some(l) => strip(&l).is_empty(),
            None => false,
        }
    }

    fn blank_parse(&mut self) -> Block {
        let start = self.pos;
        while !self.exhausted() && self.blank_match() {
            self.consume();
        }
        Block::BlankLine(start)
    }

    fn code_prefix(&self, quote_aware: bool) -> Prefix {
        let mut p = self.prefix();
        if quote_aware && matches!(self.top(), Kind::Quote | Kind::Alert) {
            let last = p.last_mut().unwrap();
            last.min = 1;
        }
        p.push(pi(PClass::Space, 4, 4));
        p
    }

    fn code_match(&mut self) -> bool {
        let line = self.next_line_raw();
        let prefix = self.code_prefix(true);
        let line = strip_prefix(&line, &prefix);
        let ok = !line.is_empty();
        self.ctx.code_line = line;
        ok
    }

    fn code_parse(&mut self) -> Block {
        let prefix = self.code_prefix(false);
        let mut lines = vec![self.ctx.code_line.clone()];
        self.consume();
        self.set_anchor();
        while !self.exhausted() {
            let line = self.next_line();
            if line.as_ref().map_or(false, |l| strip(l).is_empty()) {
                self.consume();
                let s = strip_prefix(&line.unwrap(), &prefix);
                lines.push(if s.is_empty() { vec!['\n'] } else { s });
            } else if self.code_match() {
                lines.push(self.ctx.code_line.clone());
                self.consume();
                self.set_anchor();
            } else {
                self.reset();
                break;
            }
        }
        let joined: Vec<char> = lines.concat();
        let mut code = pystr::rstrip_char(&joined, '\n').to_vec();
        code.push('\n');
        Block::Code { lang: vec![], code }
    }

    fn fence_match(&mut self) -> bool {
        let m = match self.expect(fence_re()) {
            Some(m) => m,
            None => return false,
        };
        let g = |n| m.group(&self.buf, n).unwrap().to_vec();
        let (prefix, leading, info) = (g(1), g(2), g(3));
        if leading[0] == '`' && info.contains(&'`') {
            return false;
        }
        let (lang, _, extra) = inline::partition_by_spaces(&info);
        self.ctx.code_info = (prefix, leading, lang, extra);
        true
    }

    fn fence_parse(&mut self) -> Block {
        self.next_line();
        self.consume();
        let (prefix, leading, lang, _extra) = self.ctx.code_info.clone();
        let fence_prefix = lit_prefix(&prefix);
        let mut lines: Vec<Vec<char>> = Vec::new();
        while !self.exhausted() {
            let line = match self.next_line() {
                Some(l) => l,
                None => break,
            };
            self.consume();
            if let Some(m) = fence_close_re().match_at(&line, 0) {
                if pystr::contains(m.group(&line, 1).unwrap(), &leading) {
                    break;
                }
            }
            let pl = match_prefix(&fence_prefix, &line);
            let l = if pl >= 0 { line[pl as usize..].to_vec() } else { lstrip(&line).to_vec() };
            lines.push(l);
        }
        Block::Code { lang: inline::strip_backslash(&lang), code: lines.concat() }
    }

    fn tb_match(&mut self) -> bool {
        match self.expect(tb_re()) {
            Some(m) => {
                let g = m.group(&self.buf, 0).unwrap();
                let mut set: Vec<char> = ws_re().sub_str(g, "");
                set.sort();
                set.dedup();
                set.len() == 1
            }
            None => false,
        }
    }

    /// Returns the HTML block type, 0 for no match.
    fn html_match(&mut self) -> u8 {
        self.ctx.html_end = None;
        if self.expect(html1_re()).is_some() {
            let g = pystr::string(&self.group(1));
            self.ctx.html_end = Some(Regex::new(&format!("(?i)</{}>", g)));
            return 1;
        }
        if self.expect(html2_re()).is_some() {
            self.ctx.html_end = Some(Regex::new("-->"));
            return 2;
        }
        if self.expect(html3_re()).is_some() {
            self.ctx.html_end = Some(Regex::new(r"\?>"));
            return 3;
        }
        if self.expect(html4_re()).is_some() {
            self.ctx.html_end = Some(Regex::new(">"));
            return 4;
        }
        if self.expect(html5_re()).is_some() {
            self.ctx.html_end = Some(Regex::new(r"\]\]>"));
            return 5;
        }
        if self.expect(html6_re()).is_some() {
            self.ctx.html_end = None;
            return 6;
        }
        if self.expect(html7_re()).is_some() {
            self.ctx.html_end = None;
            return 7;
        }
        0
    }

    fn html_parse(&mut self) -> Block {
        let mut lines: Vec<Vec<char>> = Vec::new();
        while !self.exhausted() {
            let line = match self.next_line() {
                Some(l) => l,
                None => break,
            };
            lines.push(line.clone());
            if let Some(end) = &self.ctx.html_end {
                if end.search(&line, 0).is_some() {
                    self.consume();
                    break;
                }
            } else if strip(&line).is_empty() {
                lines.pop();
                break;
            }
            self.consume();
        }
        Block::Html(lines.concat())
    }

    fn lrd_match(&mut self) -> bool {
        let m = match self.expect(lrd_re()) {
            Some(m) => m,
            None => return false,
        };
        let text = self.buf.clone();
        let s1 = m.span(1).unwrap().0;
        let label = match parse_link_label(&text, s1) {
            Some(l) => l,
            None => return false,
        };
        let le = label.end as usize;
        if le >= text.len() || text[le] != ':' {
            return false;
        }
        let i = parse_link_separator(&text, le + 1);
        let (dest, mut title) = match parse_link_dest_title(&text, i, false) {
            Ok(r) => r,
            Err(_) => return false,
        };
        let i = dest.end.max(title.end) as usize;
        let e = find_next(&text, &['\n'], i, None, &[]);
        let mut end = if e >= 0 { e as usize + 1 } else { text.len() };
        if !strip(&text[i..end]).is_empty() {
            let has_title = title.text.as_ref().map_or(false, |t| !t.is_empty());
            if has_title && text[dest.end as usize..title.start as usize].contains(&'\n') {
                title = inline::empty_group();
                end = (find_next(&text, &['\n'], dest.end as usize, None, &[]) + 1) as usize;
            } else {
                return false;
            }
        }
        self.ctx.linkref_info = Some((label, dest, title, end));
        true
    }

    fn lrd_parse(&mut self) -> Block {
        let (label, dest, title, pos) = self.ctx.linkref_info.clone().unwrap();
        let lt = label.text.unwrap();
        let normalized = normalize_label(&lt[1..lt.len() - 1]);
        self.link_ref_defs.entry(normalized).or_insert((dest.text.unwrap(), title.text));
        self.pos = pos;
        Block::LinkRefDef
    }

    fn list_match(&mut self) -> bool {
        if self.expect(list_re()).is_none() {
            return false;
        }
        let bullet = self.group(1);
        let head = &bullet[..bullet.len() - 1];
        let (ordered, start) = if isdigit(head) { (true, pystr::int_of(head)) } else { (false, 1) };
        self.ctx.list_info = (bullet, ordered, start);
        true
    }

    fn list_parse(&mut self) -> Block {
        let (bullet, ordered, start) = self.ctx.list_info.clone();
        self.push_state(Kind::List { bullet, ordered }, vec![], None);
        let mut items: Vec<Block> = Vec::new();
        let mut tight = true;
        let mut has_blank = false;
        while !self.exhausted() {
            if self.list_item_match() {
                let el = self.list_item_parse();
                items.push(el);
                self.set_anchor();
                if has_blank {
                    tight = false;
                }
            } else if self.blank_match() {
                self.blank_parse();
                has_blank = true;
            } else {
                self.reset();
                break;
            }
        }
        self.pop_state();
        let any_blank = items.iter().any(|it| match it {
            Block::ListItem(ch) => ch.iter().any(|c| matches!(c, Block::BlankLine(_))),
            _ => false,
        });
        let tight = tight && !any_blank;
        if tight {
            for it in items.iter_mut() {
                if let Block::ListItem(ch) = it {
                    for c in ch.iter_mut() {
                        if let Block::Paragraph { tight: t, .. } = c {
                            *t = true;
                        }
                    }
                }
            }
        }
        Block::List { ordered, start, items }
    }

    fn parse_leading(line: &[char], prefix_pos: usize) -> (isize, Vec<char>, usize, Vec<char>) {
        let exp = expandtabs(&line[prefix_pos..]);
        let stripped = lstrip(&exp).to_vec();
        let indent = line.len() as isize - prefix_pos as isize - stripped.len() as isize;
        let (bullet, sp, tail) = inline::partition_by_spaces(&stripped);
        let mut mid = sp.len();
        if mid > 4 {
            mid = 1;
        }
        (indent, bullet, mid, tail)
    }

    fn list_item_match(&mut self) -> bool {
        if self.tb_match() {
            return false;
        }
        if self.expect(list_re()).is_none() {
            return false;
        }
        let nl = expandtabs(&self.next_line_raw());
        let prefix_pos = prefix_match(&self.prefix(), &nl, 0).unwrap_or(0);
        let (indent, bullet, mid, _) = Self::parse_leading(rstrip(&nl), prefix_pos);
        let (pb, po) = match self.top() {
            Kind::List { bullet, ordered } => (bullet.clone(), *ordered),
            _ => unreachable!(),
        };
        let head = &bullet[..bullet.len() - 1];
        if (po && !isdigit(head)) || bullet.last() != pb.last() {
            return false;
        }
        if !po && bullet != pb {
            return false;
        }
        self.ctx.list_item_info = (indent as usize, bullet, mid);
        true
    }

    fn list_item_parse(&mut self) -> Block {
        let (indent, bullet, mid) = self.ctx.list_item_info.clone();
        let mut prefix = spaces(indent);
        prefix.extend(lit_prefix(&bullet));
        prefix.extend(spaces(mid));
        let second = spaces(bullet.len() + indent + if mid == 0 { 1 } else { mid });
        self.push_state(Kind::ListItem, prefix, Some(second));
        let first = self.next_line().unwrap_or_default();
        if strip(&first).is_empty() {
            self.consume();
            let n = self.next_line();
            if n.as_ref().map_or(true, |l| l.is_empty() || strip(l).is_empty()) {
                self.pop_state();
                return Block::ListItem(vec![]);
            }
        }
        let mut children = self.parse_source();
        self.pop_state();
        if let Some(Block::BlankLine(anchor)) = children.last() {
            let anchor = *anchor;
            children.pop();
            if !children.is_empty() {
                self.pos = anchor;
            }
        }
        Block::ListItem(children)
    }

    fn is_setext(line: &[char]) -> bool {
        setext_re().is_match_at(line, 0)
    }

    fn break_paragraph(&mut self, lazy: bool) -> bool {
        let prev = self.m.clone();
        let r = self.break_paragraph_inner(lazy);
        self.m = prev;
        r
    }

    fn break_paragraph_inner(&mut self, lazy: bool) -> bool {
        for e in self.cfg.breaking.clone() {
            if self.el_match(e) {
                return true;
            }
        }
        if lazy && matches!(self.top(), Kind::List { .. }) && self.list_item_match() {
            return true;
        }
        if self.list_match() {
            let line = self.next_line().unwrap();
            let (_, bullet, _, tail) = Self::parse_leading(rstrip(&line), 0);
            let one = !bullet.is_empty() && bullet[..bullet.len() - 1] == ['1'];
            let bstr = pystr::string(&bullet);
            if lazy || ((one || "*-+".contains(bstr.as_str())) && !tail.is_empty()) {
                return true;
            }
        }
        let h = self.html_match();
        if h != 0 && h != 7 {
            return true;
        }
        if self.tb_match() {
            if !lazy && Self::is_setext(&self.next_line().unwrap()) {
                return false;
            }
            return true;
        }
        false
    }

    fn make_paragraph(&self, lines: &[Vec<char>]) -> Block {
        let joined: Vec<char> = lines.iter().flat_map(|l| lstrip(l).iter().copied()).collect();
        let mut body = pystr::rstrip_char(&joined, '\n').to_vec();
        let mut checked = None;
        if self.cfg.gfm {
            if let Some(m) = task_re().match_at(&body, 0) {
                let (a, b) = m.span(1).unwrap();
                checked = Some(pystr::lower(&body[a + 1..b - 1]) == ['x']);
                body = body[b..].to_vec();
            }
        }
        Block::Paragraph { body, tight: false, checked, children: vec![] }
    }

    fn para_parse(&mut self) -> Block {
        let first = self.next_line().unwrap();
        let mut lines = vec![first];
        self.consume();
        let mut end_parse = false;
        while !self.exhausted() && !end_parse {
            if self.break_paragraph(false) {
                break;
            }
            let line = self.next_line();
            match line {
                Some(l) if !l.is_empty() => {
                    lines.push(l.clone());
                    self.consume();
                    if Self::is_setext(&l) {
                        let underline = lines.pop().unwrap();
                        let level = if strip(&underline)[0] == '=' { 1 } else { 2 };
                        let joined: Vec<char> = lines.iter().flat_map(|l| lstrip(l).iter().copied()).collect();
                        return Block::Heading { level, body: strip(&joined).to_vec(), children: vec![] };
                    }
                }
                _ => {
                    let states = self.states.clone();
                    while self.states.len() > 1 {
                        self.pop_state();
                        let nl = self.next_line();
                        if let Some(nl) = nl.filter(|l| !l.is_empty()) {
                            if self.break_paragraph(true) {
                                end_parse = true;
                            } else {
                                lines.push(nl);
                                self.consume();
                            }
                            break;
                        }
                    }
                    self.states = states;
                }
            }
        }
        self.make_paragraph(&lines)
    }

    fn row_match(&mut self) -> bool {
        let line = match self.next_line() {
            Some(l) if !l.is_empty() => l,
            _ => return false,
        };
        if !row_start_re().is_match_at(&line, 0) {
            return false;
        }
        let stripped = strip(&line).to_vec();
        let mut parts: Vec<Vec<char>> = Vec::new();
        let mut last = 0;
        for m in splitter_re().find_iter(&stripped) {
            parts.push(stripped[last..m.start()].to_vec());
            last = m.end();
        }
        parts.push(stripped[last..].to_vec());
        if !parts.is_empty() && parts[0].is_empty() {
            parts.remove(0);
        }
        if !parts.is_empty() && parts.last().unwrap().is_empty() {
            parts.pop();
        }
        if parts.is_empty() {
            return false;
        }
        self.ctx.is_delimiter = parts.iter().all(|c| delim_cell_re().is_match_at(c, 0));
        self.ctx.cells = parts;
        true
    }

    fn table_match(&mut self) -> bool {
        self.set_anchor();
        if !self.row_match() || self.ctx.is_delimiter {
            return false;
        }
        let line = self.next_line().unwrap();
        if splitter_re().search(&line, 0).is_none() {
            return false;
        }
        self.pos = self.m.as_ref().unwrap().end();
        let head = self.ctx.cells.clone();
        if !self.row_match() || !self.ctx.is_delimiter || self.ctx.cells.len() != head.len() {
            self.reset();
            return false;
        }
        self.ctx.table_info = (head, self.ctx.cells.clone());
        self.consume();
        true
    }

    fn cell(text: &[char], header: bool, align: Option<&'static str>) -> Block {
        let body = pystr::replace(strip(text), &['\\', '|'], &['|']);
        Block::TableCell { body, header, align, children: vec![] }
    }

    fn table_parse(&mut self) -> Block {
        let (head, delims) = self.ctx.table_info.clone();
        self.push_state(Kind::Table { aligns: vec![] }, vec![], None);
        let mut aligns = Vec::new();
        for d in delims.iter().take(head.len()) {
            let s = strip(d);
            let a = if s[0] == ':' && *s.last().unwrap() == ':' {
                Some("center")
            } else if s[0] == ':' {
                Some("left")
            } else if *s.last().unwrap() == ':' {
                Some("right")
            } else {
                None
            };
            aligns.push(a);
        }
        let top = *self.states.last().unwrap();
        self.arena[top].kind = Kind::Table { aligns: aligns.clone() };
        let head_row = Block::TableRow(head.iter().zip(&aligns).map(|(c, a)| Self::cell(c, true, *a)).collect());
        let mut rows = vec![head_row];
        let list = self.cfg.block_elements.clone();
        while !self.exhausted() {
            let mut broke = false;
            for &e in &list {
                if e == El::Table || e == El::Paragraph {
                    continue;
                }
                if self.el_match(e) {
                    broke = true;
                    break;
                }
            }
            if broke {
                break;
            }
            if self.row_match() {
                rows.push(self.row_parse());
                continue;
            }
            break;
        }
        self.pop_state();
        Block::Table(rows)
    }

    fn row_parse(&mut self) -> Block {
        self.consume();
        let aligns = match self.top() {
            Kind::Table { aligns } => aligns.clone(),
            _ => unreachable!(),
        };
        let n = aligns.len();
        let mut cells = self.ctx.cells.clone();
        if cells.len() < n {
            cells.resize(n, vec![]);
        } else {
            cells.truncate(n);
        }
        Block::TableRow(cells.iter().zip(&aligns).map(|(c, a)| Self::cell(c, false, *a)).collect())
    }
}

fn parse_inline(b: &mut Block, cx: &inline::InlineCtx, defs: &mut Vec<FootnoteDef>) {
    match b {
        Block::Heading { body, children, .. }
        | Block::Paragraph { body, children, .. }
        | Block::TableCell { body, children, .. } => {
            if !body.is_empty() {
                *children = inline::parse(body, cx);
            }
        }
        Block::Quote(ch) | Block::ListItem(ch) | Block::Table(ch) | Block::TableRow(ch) | Block::Alert { children: ch, .. } => {
            for c in ch.iter_mut() {
                parse_inline(c, cx, defs);
            }
        }
        Block::List { items, .. } => {
            for c in items.iter_mut() {
                parse_inline(c, cx, defs);
            }
        }
        Block::FootnoteDef(id) => {
            let mut ch = std::mem::take(&mut defs[*id].children);
            for c in ch.iter_mut() {
                parse_inline(c, cx, defs);
            }
            defs[*id].children = ch;
        }
        _ => {}
    }
}

#[allow(dead_code)]
fn unused(_: &str) -> String {
    re::escape(&chars(""))
}
