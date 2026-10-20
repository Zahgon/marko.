//! A Rust port of marko 2.2.4 (CommonMark parser and HTML renderer) with
//! its gfm, footnote and toc extensions.

mod block;
mod htmlx;
mod inline;
mod pystr;
mod re;
mod render;
mod tables;

use block::{Config, El, Source};
use inline::IEl;
use render::{RenderState, Renderer};
use std::fmt;

/// A failure that marko raises as a Python exception; `kind()` is the
/// exception class name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    kind: String,
}

impl Error {
    pub(crate) fn new(kind: &str) -> Error {
        Error { kind: kind.to_string() }
    }

    pub fn kind(&self) -> &str {
        &self.kind
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.kind)
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Extension {
    Gfm,
    Footnote,
    Toc { opening: Option<String>, closing: Option<String>, item_format: Option<String> },
}

impl Extension {
    pub fn from_name(name: &str) -> Result<Extension, Error> {
        match name {
            "gfm" => Ok(Extension::Gfm),
            "footnote" => Ok(Extension::Footnote),
            "toc" => Ok(Extension::Toc { opening: None, closing: None, item_format: None }),
            _ => Err(Error::new("ImportError")),
        }
    }
}

#[derive(Default)]
pub struct Markdown {
    exts: Vec<Extension>,
    config: Option<Config>,
    state: RenderState,
}

impl Markdown {
    pub fn new() -> Markdown {
        Markdown::default()
    }

    pub fn use_extension(&mut self, ext: Extension) -> Result<(), Error> {
        if self.config.is_some() {
            return Err(Error::new("SetupDone"));
        }
        self.exts.push(ext);
        Ok(())
    }

    fn has(&self, f: impl Fn(&Extension) -> bool) -> bool {
        self.exts.iter().any(f)
    }

    /// Installs the extensions on first use.  marko composes one renderer
    /// class from every extension's mixins, so a mixin class listed twice
    /// (gfm or footnote used twice) is a duplicate base and setup fails with
    /// TypeError, leaving the instance unset so later `use` calls still work.
    /// Each toc use builds a fresh mixin class, so repeating toc is allowed.
    fn setup(&mut self) -> Result<(), Error> {
        if self.config.is_some() {
            return Ok(());
        }
        for shared in [Extension::Gfm, Extension::Footnote] {
            if self.exts.iter().filter(|e| **e == shared).count() > 1 {
                return Err(Error::new("TypeError"));
            }
        }
        let mut elements = Vec::new();
        for e in &self.exts {
            match e {
                Extension::Gfm => {
                    elements.push((None, Some(IEl::Strike)));
                    elements.push((None, Some(IEl::Url)));
                    elements.push((Some(El::Table), None));
                    elements.push((Some(El::Alert), None));
                }
                Extension::Footnote => {
                    elements.push((Some(El::FootnoteDef), Some(IEl::FootnoteRef)));
                }
                Extension::Toc { .. } => {}
            }
        }
        let gfm = self.has(|e| *e == Extension::Gfm);
        self.config = Some(Config::new(&elements, gfm));
        Ok(())
    }

    pub fn convert(&mut self, text: &str) -> Result<String, Error> {
        self.setup()?;
        let gfm = self.has(|e| *e == Extension::Gfm);
        let footnote = self.has(|e| *e == Extension::Footnote);
        let toc = self.has(|e| matches!(e, Extension::Toc { .. }));
        let cfg = self.config.as_ref().unwrap();
        let mut src = Source::new(text, cfg);
        let doc = src.parse_document();
        if toc {
            self.state.headings = Some(Vec::new());
        }
        let mut r = Renderer {
            st: &mut self.state,
            gfm,
            footnote,
            toc,
            defs: &src.defs,
            fnmap: &src.footnotes,
        };
        r.render_document(&doc)
    }

    fn toc_option(&self, pick: impl Fn(&Extension) -> Option<&Option<String>>, default: &str) -> String {
        for e in self.exts.iter().rev() {
            if let Some(Some(v)) = pick(e) {
                if !v.is_empty() {
                    return v.clone();
                }
            }
        }
        default.to_string()
    }

    pub fn render_toc(&self, maxdepth: i64) -> Result<String, Error> {
        let toc = self.has(|e| matches!(e, Extension::Toc { .. }));
        if self.config.is_none() || !toc {
            return Err(Error::new("AttributeError"));
        }
        let headings = match &self.state.headings {
            Some(h) => h,
            None => return Err(Error::new("AttributeError")),
        };
        if headings.is_empty() {
            return Ok(String::new());
        }
        let opening = self.toc_option(|e| if let Extension::Toc { opening, .. } = e { Some(opening) } else { None }, "<ul>");
        let closing = self.toc_option(|e| if let Extension::Toc { closing, .. } = e { Some(closing) } else { None }, "</ul>");
        let item_format = self.toc_option(
            |e| if let Extension::Toc { item_format, .. } = e { Some(item_format) } else { None },
            "<li><a href=\"#{slug}\">{text}</a></li>",
        );
        let mut first: Option<i64> = None;
        let mut last: i64 = 0;
        let mut rv = String::new();
        for (level, slug, text) in headings {
            let level = *level as i64;
            if let Some(f) = first {
                if level - f >= maxdepth {
                    continue;
                }
            }
            if first.is_none() {
                first = Some(level);
                last = level;
                rv.push_str(&opening);
                rv.push('\n');
            }
            if last == level - 1 {
                rv.push_str(&opening);
                rv.push('\n');
                last = level;
            }
            while last > level {
                rv.push_str(&closing);
                rv.push('\n');
                last -= 1;
            }
            rv.push_str(&format_item(&item_format, slug, text)?);
            rv.push('\n');
        }
        for _ in first.unwrap()..last + 1 {
            rv.push_str(&closing);
            rv.push('\n');
        }
        Ok(rv)
    }
}

/// `str.format(slug=..., text=...)` for the `{slug}`, `{text}`, `{{` and `}}`
/// forms.
fn format_item(fmt: &str, slug: &str, text: &str) -> Result<String, Error> {
    let mut out = String::new();
    let mut it = fmt.chars().peekable();
    while let Some(c) = it.next() {
        match c {
            '{' if it.peek() == Some(&'{') => {
                it.next();
                out.push('{');
            }
            '}' if it.peek() == Some(&'}') => {
                it.next();
                out.push('}');
            }
            '{' => {
                let mut name = String::new();
                loop {
                    match it.next() {
                        Some('}') => break,
                        Some(ch) => name.push(ch),
                        None => return Err(Error::new("ValueError")),
                    }
                }
                // str.format reads an empty or all-decimal argument name as a position, and there are none.
                let arg = name.split(['.', '[', '!', ':']).next().unwrap_or("");
                if arg.chars().all(|ch| pystr::digit_value(ch).is_some()) {
                    return Err(Error::new("IndexError"));
                }
                match name.as_str() {
                    "slug" => out.push_str(slug),
                    "text" => out.push_str(text),
                    _ => return Err(Error::new("KeyError")),
                }
            }
            '}' => return Err(Error::new("ValueError")),
            _ => out.push(c),
        }
    }
    Ok(out)
}
