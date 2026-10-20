//! `html.escape`, `html.unescape` (with marko's narrowed charref pattern),
//! `urllib.parse.quote`, and marko's `escape_html` / `escape_url`.

use crate::re::Regex;
use crate::tables::{HTML5, INVALID_CHARREFS, INVALID_CODEPOINTS};
use std::sync::OnceLock;

pub fn escape(s: &[char], out: &mut String) {
    for &c in s {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#x27;"),
            _ => out.push(c),
        }
    }
}

pub fn escape_string(s: &[char]) -> String {
    let mut out = String::new();
    escape(s, &mut out);
    out
}

fn html5(name: &str) -> Option<&'static str> {
    HTML5.binary_search_by(|&(k, _)| k.cmp(name)).ok().map(|i| HTML5[i].1)
}

fn numeric(num: u64) -> String {
    if num <= u32::MAX as u64 {
        if let Ok(i) = INVALID_CHARREFS.binary_search_by_key(&(num as u32), |&(k, _)| k) {
            return INVALID_CHARREFS[i].1.to_string();
        }
    }
    if (0xD800..=0xDFFF).contains(&num) || num > 0x10FFFF {
        return "\u{FFFD}".to_string();
    }
    let n = num as u32;
    if INVALID_CODEPOINTS.iter().any(|&(a, b)| a <= n && n <= b) {
        return String::new();
    }
    char::from_u32(n).unwrap().to_string()
}

/// Match marko's `_charref` at `i` (which holds `&`); returns the end.
fn charref_at(s: &[char], i: usize) -> Option<usize> {
    let n = s.len();
    let mut j = i + 1;
    if j < n && s[j] == '#' {
        j += 1;
        let mut k = j;
        while k < n && k - j < 7 && s[k].is_ascii_digit() {
            k += 1;
        }
        if k > j && k < n && s[k] == ';' {
            return Some(k + 1);
        }
        if j < n && (s[j] == 'x' || s[j] == 'X') {
            let h = j + 1;
            let mut k = h;
            while k < n && k - h < 6 && s[k].is_ascii_hexdigit() {
                k += 1;
            }
            if k > h && k < n && s[k] == ';' {
                return Some(k + 1);
            }
        }
        return None;
    }
    let mut k = j;
    while k < n && k - j < 32 && !matches!(s[k], '\t' | '\n' | '\x0c' | ' ' | '<' | '&' | '#' | ';') {
        k += 1;
    }
    if k > j && k < n && s[k] == ';' {
        Some(k + 1)
    } else {
        None
    }
}

fn replace_charref(body: &[char]) -> String {
    if body[0] == '#' {
        let digits: String = body[1..body.len() - 1].iter().collect();
        let num = if digits.starts_with(['x', 'X']) {
            u64::from_str_radix(&digits[1..], 16).unwrap()
        } else {
            digits.parse::<u64>().unwrap()
        };
        return numeric(num);
    }
    let s: String = body.iter().collect();
    if let Some(v) = html5(&s) {
        return v.to_string();
    }
    for x in (2..body.len()).rev() {
        let pre: String = body[..x].iter().collect();
        if let Some(v) = html5(&pre) {
            let rest: String = body[x..].iter().collect();
            return format!("{}{}", v, rest);
        }
    }
    format!("&{}", s)
}

pub fn unescape(s: &[char]) -> Vec<char> {
    if !s.contains(&'&') {
        return s.to_vec();
    }
    let mut out = Vec::with_capacity(s.len());
    let mut i = 0;
    while i < s.len() {
        if s[i] == '&' {
            if let Some(e) = charref_at(s, i) {
                out.extend(replace_charref(&s[i + 1..e]).chars());
                i = e;
                continue;
            }
        }
        out.push(s[i]);
        i += 1;
    }
    out
}

pub fn quote(s: &[char], safe: &str) -> String {
    let mut out = String::new();
    let mut buf = [0u8; 4];
    for &c in s {
        for &b in c.encode_utf8(&mut buf).as_bytes() {
            let ch = b as char;
            if b.is_ascii_alphanumeric() || "_.-~".contains(ch) || (b < 128 && safe.contains(ch)) {
                out.push(ch);
            } else {
                out.push_str(&format!("%{:02X}", b));
            }
        }
    }
    out
}

/// `HTMLRenderer.escape_html`.
pub fn escape_html(raw: &[char]) -> String {
    let mut out = String::new();
    for c in unescape(raw) {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(c),
        }
    }
    out
}

fn harmful() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(r"(?i)^\s*(javascript|vbscript|data):"))
}

/// `HTMLRenderer.escape_url`.
pub fn escape_url(raw: &[char]) -> String {
    let q = quote(&unescape(raw), "/#:()*?=%@+,&");
    let q: Vec<char> = q.chars().collect();
    let escaped = escape_string(&q);
    let e: Vec<char> = escaped.chars().collect();
    if harmful().is_match_at(&e, 0) {
        return "#harmful-link".to_string();
    }
    escaped
}
