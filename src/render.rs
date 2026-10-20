//! `marko.html_renderer` plus the gfm, footnote and toc renderer mixins.

use crate::block::{Block, FootnoteDef};
use crate::htmlx::{escape_html, escape_string, escape_url};
use crate::inline::Inline;
use crate::pystr::{self, chars};
use crate::re::Regex;
use crate::Error;
use std::collections::HashMap;
use std::sync::OnceLock;

macro_rules! re {
    ($name:ident, $pat:expr) => {
        fn $name() -> &'static Regex {
            static R: OnceLock<Regex> = OnceLock::new();
            R.get_or_init(|| Regex::new(&$pat))
        }
    };
}

re!(tagfilter_re, r"(?i)<(title|textarea|style|xmp|iframe|noembed|noframes|script|plaintext)");
re!(tagfilter_no_open_re, r"(?i)(?<!^)( *)<(title|textarea|style|xmp|iframe|noembed|noframes|script|plaintext)");
re!(tag_strip_re, r"<.+?>");
re!(slug_drop_re, r"[^\w\- ]+");
re!(p_end_re, r"</p>$");

/// Mutable renderer state that outlives one `convert` call.
#[derive(Default)]
pub struct RenderState {
    pub plain: bool,
    pub footnotes: Vec<Vec<char>>,
    pub headings: Option<Vec<(usize, String, String)>>,
}

pub struct Renderer<'a> {
    pub st: &'a mut RenderState,
    pub gfm: bool,
    pub footnote: bool,
    pub toc: bool,
    pub defs: &'a [FootnoteDef],
    pub fnmap: &'a HashMap<Vec<char>, usize>,
}

type R = Result<String, Error>;

fn attr_err() -> Error {
    Error::new("AttributeError")
}

impl<'a> Renderer<'a> {
    pub fn render_document(&mut self, children: &[Block]) -> R {
        if self.st.plain || !self.footnote {
            return self.blocks(children);
        }
        let text = self.blocks(children)?;
        let mut items = Vec::new();
        for label in &self.st.footnotes {
            match self.fnmap.get(label) {
                Some(&id) => items.push(id),
                None => return Err(Error::new("KeyError")),
            }
        }
        if items.is_empty() {
            return Ok(text);
        }
        let mut body = String::new();
        for id in items {
            body.push_str(&self.footnote_def(id)?);
        }
        self.st.footnotes.clear();
        Ok(format!("{}<div class=\"footnotes\">\n<ol>\n{}</ol>\n</div>\n", text, body))
    }

    fn footnote_def(&mut self, id: usize) -> R {
        let defs = self.defs;
        let def = &defs[id];
        let rendered = self.blocks(&def.children)?;
        let rc = chars(&rendered);
        let mut children = pystr::string(pystr::rstrip(&rc));
        let label = pystr::string(&def.label);
        let back = format!("<a href=\"#fnref-{}\" class=\"footnote\">&#8617;</a>", label);
        if children.ends_with("</p>") {
            let repl = expand_template(&chars(&format!("{}</p>", back)))?;
            let c = chars(&children);
            children = pystr::string(&p_end_re().sub_with(&c, |_| repl.clone()));
        } else {
            children = format!("{}<p>{}</p>\n", children, back);
        }
        Ok(format!("<li id=\"fn-{}\">\n{}</li>\n", escape_url(&def.label), children))
    }

    pub fn blocks(&mut self, bs: &[Block]) -> R {
        let mut out = String::new();
        for b in bs {
            out.push_str(&self.block(b)?);
        }
        Ok(out)
    }

    pub fn inlines(&mut self, xs: &[Inline]) -> R {
        let mut out = String::new();
        for x in xs {
            out.push_str(&self.inline(x)?);
        }
        Ok(out)
    }

    fn block(&mut self, b: &Block) -> R {
        if self.st.plain {
            return match b {
                Block::Heading { children, .. }
                | Block::Paragraph { children, .. }
                | Block::TableCell { children, .. } => self.inlines(children),
                Block::Code { code, .. } => Ok(escape_html(code)),
                Block::Quote(c) | Block::ListItem(c) | Block::Table(c) | Block::TableRow(c) => self.blocks(c),
                Block::Alert { children, .. } => self.blocks(children),
                Block::List { items, .. } => self.blocks(items),
                Block::FootnoteDef(id) => {
                    let defs = self.defs;
                    self.blocks(&defs[*id].children)
                }
                Block::BlankLine(_) | Block::ThematicBreak | Block::Html(_) | Block::LinkRefDef => Ok(String::new()),
            };
        }
        match b {
            Block::BlankLine(_) | Block::LinkRefDef | Block::FootnoteDef(_) => Ok(String::new()),
            Block::Heading { level, children, .. } => {
                let inner = self.inlines(children)?;
                if self.toc {
                    let c = chars(&inner);
                    let raw = tag_strip_re().sub_str(&c, "");
                    let slug = slugify(&raw);
                    self.st.headings.get_or_insert_with(Vec::new).push((*level, slug.clone(), inner.clone()));
                    Ok(format!("<h{0} id=\"{1}\">{2}</h{0}>\n", level, slug, inner))
                } else {
                    Ok(format!("<h{0}>{1}</h{0}>\n", level, inner))
                }
            }
            Block::Code { lang, code } => {
                let l = if lang.is_empty() { String::new() } else { format!(" class=\"language-{}\"", escape_html(lang)) };
                Ok(format!("<pre><code{}>{}</code></pre>\n", l, escape_string(code)))
            }
            Block::ThematicBreak => Ok("<hr />\n".to_string()),
            Block::Html(body) => {
                if self.gfm {
                    let out = tagfilter_no_open_re().sub_with(body, |m| {
                        let mut v = m.group(body, 1).unwrap().to_vec();
                        v.extend(chars("&lt;"));
                        v.extend_from_slice(m.group(body, 2).unwrap());
                        v
                    });
                    Ok(pystr::string(&out))
                } else {
                    Ok(pystr::string(body))
                }
            }
            Block::Paragraph { tight, checked, children, .. } => {
                let mut inner = self.inlines(children)?;
                if self.gfm {
                    if let Some(c) = checked {
                        inner = format!("<input{} disabled=\"\" type=\"checkbox\">{}", if *c { " checked=\"\"" } else { "" }, inner);
                    }
                }
                if *tight {
                    Ok(inner)
                } else {
                    Ok(format!("<p>{}</p>\n", inner))
                }
            }
            Block::Quote(c) => Ok(format!("<blockquote>\n{}</blockquote>\n", self.blocks(c)?)),
            Block::Alert { kind, children } => {
                let header = chars(&escape_html(kind));
                let inner = self.blocks(children)?;
                Ok(format!(
                    "<blockquote class=\"alert alert-{}\">\n<p>{}</p>\n{}</blockquote>\n",
                    pystr::string(&pystr::lower(kind)),
                    pystr::string(&pystr::title(&header)),
                    inner
                ))
            }
            Block::List { ordered, start, items } => {
                let (tag, extra) = if *ordered {
                    ("ol", if *start != 1 { format!(" start=\"{}\"", start) } else { String::new() })
                } else {
                    ("ul", String::new())
                };
                Ok(format!("<{0}{1}>\n{2}</{0}>\n", tag, extra, self.blocks(items)?))
            }
            Block::ListItem(c) => {
                let sep = if c.len() == 1 && matches!(c[0], Block::Paragraph { tight: true, .. }) { "" } else { "\n" };
                Ok(format!("<li>{}{}</li>\n", sep, self.blocks(c)?))
            }
            Block::Table(rows) => {
                let head = self.block(&rows[0])?;
                let mut tbody = String::new();
                if rows.len() > 1 {
                    tbody = format!("\n<tbody>\n{}</tbody>", self.blocks(&rows[1..])?);
                }
                Ok(format!("<table>\n<thead>\n{}</thead>{}</table>", head, tbody))
            }
            Block::TableRow(cells) => Ok(format!("<tr>\n{}</tr>\n", self.blocks(cells)?)),
            Block::TableCell { header, align, children, .. } => {
                let tag = if *header { "th" } else { "td" };
                let a = match align {
                    Some(a) => format!(" align=\"{}\"", a),
                    None => String::new(),
                };
                Ok(format!("<{0}{1}>{2}</{0}>\n", tag, a, self.inlines(children)?))
            }
        }
    }

    fn link(&mut self, dest: &[char], title: Option<&[char]>, children: &[Inline]) -> R {
        let t = match title {
            Some(t) if !t.is_empty() => format!(" title=\"{}\"", escape_html(t)),
            _ => String::new(),
        };
        let url = escape_url(dest);
        let body = self.inlines(children)?;
        Ok(format!("<a href=\"{}\"{}>{}</a>", url, t, body))
    }

    fn inline(&mut self, x: &Inline) -> R {
        if self.st.plain {
            return match x {
                Inline::Raw(t) | Inline::Literal(t) | Inline::Html(t) | Inline::Code(t) => Ok(escape_html(t)),
                Inline::LineBreak(_) => Ok("\n".to_string()),
                Inline::AutoLink { text, .. } | Inline::Url { text, .. } => Ok(escape_html(text)),
                Inline::Link { children, .. } => self.inlines(children),
                Inline::Emph(c) | Inline::Strong(c) | Inline::Strike(c) => self.inlines(c),
                Inline::FootnoteRef(_) => Err(attr_err()),
            };
        }
        match x {
            Inline::Raw(t) | Inline::Literal(t) => Ok(escape_html(t)),
            Inline::LineBreak(soft) => Ok(if *soft { "\n" } else { "<br />\n" }.to_string()),
            Inline::Html(t) => {
                if self.gfm {
                    Ok(pystr::string(&tagfilter_re().sub_with(t, |m| {
                        let mut v = chars("&lt;");
                        v.extend_from_slice(m.group(t, 1).unwrap());
                        v
                    })))
                } else {
                    Ok(pystr::string(t))
                }
            }
            Inline::Code(t) => Ok(format!("<code>{}</code>", escape_string(t))),
            Inline::AutoLink { dest, text } | Inline::Url { dest, text } => {
                self.link(dest, None, &[Inline::Raw(text.clone())])
            }
            Inline::Link { image: false, dest, title, children } => self.link(dest, title.as_deref(), children),
            Inline::Link { image: true, dest, title, children } => {
                let t = match title {
                    Some(t) if !t.is_empty() => format!(" title=\"{}\"", escape_html(t)),
                    _ => String::new(),
                };
                let url = escape_url(dest);
                self.st.plain = true;
                let body = self.inlines(children)?;
                self.st.plain = false;
                Ok(format!("<img src=\"{}\" alt=\"{}\"{} />", url, body, t))
            }
            Inline::Emph(c) => Ok(format!("<em>{}</em>", self.inlines(c)?)),
            Inline::Strong(c) => Ok(format!("<strong>{}</strong>", self.inlines(c)?)),
            Inline::Strike(c) => Ok(format!("<del>{}</del>", self.inlines(c)?)),
            Inline::FootnoteRef(label) => {
                if !self.st.footnotes.contains(label) {
                    self.st.footnotes.push(label.clone());
                }
                let idx = self.st.footnotes.iter().position(|l| l == label).unwrap() + 1;
                let u = escape_url(label);
                Ok(format!("<sup class=\"footnote-ref\" id=\"fnref-{0}\"><a href=\"#fn-{0}\">{1}</a></sup>", u, idx))
            }
        }
    }
}

fn slugify(s: &[char]) -> String {
    let kept = slug_drop_re().sub_str(s, "");
    let lowered = pystr::lower(pystr::strip(&kept));
    lowered.iter().map(|&c| if c == ' ' { '-' } else { c }).collect()
}

/// `re.sub` replacement-template expansion for a pattern with no groups
/// (CPython 3.11 `sre_parse.parse_template` / `expand_template`).
fn expand_template(t: &[char]) -> Result<Vec<char>, Error> {
    let bad = || Error::new("error");
    let mut out = Vec::new();
    let mut i = 0;
    let n = t.len();
    let oct = |c: char| ('0'..='7').contains(&c);
    while i < n {
        let c = t[i];
        if c != '\\' {
            out.push(c);
            i += 1;
            continue;
        }
        if i + 1 >= n {
            return Err(bad());
        }
        let e = t[i + 1];
        i += 2;
        match e {
            'g' => {
                if t.get(i) != Some(&'<') {
                    return Err(bad());
                }
                i += 1;
                let close = match t[i..].iter().position(|&c| c == '>') {
                    Some(p) => i + p,
                    None => return Err(bad()),
                };
                let name = &t[i..close];
                i = close + 1;
                if name.is_empty() {
                    return Err(bad());
                }
                if is_identifier(name) {
                    return Err(Error::new("IndexError"));
                }
                match py_int(name) {
                    Some(0) => out.extend(chars("</p>")),
                    _ => return Err(bad()),
                }
            }
            '0' => {
                let mut v = 0u32;
                let mut k = 0;
                while k < 2 && i < n && oct(t[i]) {
                    v = v * 8 + t[i].to_digit(8).unwrap();
                    i += 1;
                    k += 1;
                }
                out.push(char::from_u32(v & 0xff).unwrap());
            }
            d if d.is_ascii_digit() => {
                if i < n && t[i].is_ascii_digit() {
                    let d2 = t[i];
                    i += 1;
                    if oct(d) && oct(d2) && i < n && oct(t[i]) {
                        let v = d.to_digit(8).unwrap() * 64 + d2.to_digit(8).unwrap() * 8 + t[i].to_digit(8).unwrap();
                        i += 1;
                        if v > 0o377 {
                            return Err(bad());
                        }
                        out.push(char::from_u32(v).unwrap());
                        continue;
                    }
                }
                return Err(bad());
            }
            'a' => out.push('\x07'),
            'b' => out.push('\x08'),
            'f' => out.push('\x0c'),
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            't' => out.push('\t'),
            'v' => out.push('\x0b'),
            '\\' => out.push('\\'),
            x if x.is_ascii_alphabetic() => return Err(bad()),
            x => {
                out.push('\\');
                out.push(x);
            }
        }
    }
    Ok(out)
}

fn is_identifier(s: &[char]) -> bool {
    let first = s[0];
    (first == '_' || first.is_alphabetic()) && s.iter().all(|&c| c == '_' || c.is_alphanumeric())
}

/// `int(s)` for the group-name path; returns None where CPython raises.
fn py_int(s: &[char]) -> Option<u64> {
    let t = pystr::strip(s);
    let t = match t.first() {
        Some('+') | Some('-') => &t[1..],
        _ => t,
    };
    if t.is_empty() || t[0] == '_' || *t.last().unwrap() == '_' {
        return None;
    }
    let mut v: u64 = 0;
    let mut prev_us = false;
    for &c in t {
        if c == '_' {
            if prev_us {
                return None;
            }
            prev_us = true;
            continue;
        }
        prev_us = false;
        let d = pystr::digit_value(c)? as u64;
        v = v.saturating_mul(10).saturating_add(d);
    }
    Some(v)
}
