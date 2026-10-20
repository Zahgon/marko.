//! CPython 3.11 string semantics over code-point slices.

use crate::tables::*;

fn in_ranges(r: &[(u32, u32)], c: char) -> bool {
    let c = c as u32;
    let i = r.partition_point(|&(_, hi)| hi < c);
    i < r.len() && r[i].0 <= c
}

fn lookup(map: &'static [(u32, &'static str)], c: char) -> Option<&'static str> {
    map.binary_search_by_key(&(c as u32), |&(k, _)| k).ok().map(|i| map[i].1)
}

/// `str.isspace()` and the `\s` class of `re`.
pub fn is_space(c: char) -> bool {
    in_ranges(SPACE, c)
}

/// The `\w` class of `re`.
pub fn is_word(c: char) -> bool {
    in_ranges(WORD, c)
}

/// Decimal value of a `\d` character.
pub fn digit_value(c: char) -> Option<u32> {
    let u = c as u32;
    let i = DIGIT.partition_point(|&(_, hi, _)| hi < u);
    if i < DIGIT.len() && DIGIT[i].0 <= u {
        Some(DIGIT[i].2 + (u - DIGIT[i].0))
    } else {
        None
    }
}

pub fn is_digit(c: char) -> bool {
    digit_value(c).is_some()
}

/// `marko.patterns.is_punctuation`.
pub fn is_punctuation(c: char) -> bool {
    in_ranges(PUNCT, c)
}

fn is_cased(c: char) -> bool {
    in_ranges(CASED, c)
}

fn is_case_ignorable(c: char) -> bool {
    in_ranges(CASE_IGNORABLE, c)
}

/// Fold used by `re` IGNORECASE literals.
pub fn ci_fold(c: char) -> char {
    match lookup(CI_FOLD, c) {
        Some(s) => s.chars().next().unwrap(),
        None => c,
    }
}

fn final_sigma(s: &[char], i: usize) -> bool {
    let mut j = i as isize - 1;
    while j >= 0 && is_case_ignorable(s[j as usize]) {
        j -= 1;
    }
    if !(j >= 0 && is_cased(s[j as usize])) {
        return false;
    }
    let mut k = i + 1;
    while k < s.len() && is_case_ignorable(s[k]) {
        k += 1;
    }
    k == s.len() || !is_cased(s[k])
}

fn push_lower(out: &mut Vec<char>, s: &[char], i: usize) {
    let c = s[i];
    if c == '\u{3a3}' {
        out.push(if final_sigma(s, i) { '\u{3c2}' } else { '\u{3c3}' });
        return;
    }
    match lookup(LOWER, c) {
        Some(m) => out.extend(m.chars()),
        None => out.push(c),
    }
}

pub fn lower(s: &[char]) -> Vec<char> {
    let mut out = Vec::with_capacity(s.len());
    for i in 0..s.len() {
        push_lower(&mut out, s, i);
    }
    out
}

pub fn upper(s: &[char]) -> Vec<char> {
    let mut out = Vec::with_capacity(s.len());
    for &c in s {
        match lookup(UPPER, c) {
            Some(m) => out.extend(m.chars()),
            None => out.push(c),
        }
    }
    out
}

pub fn title(s: &[char]) -> Vec<char> {
    let mut out = Vec::with_capacity(s.len());
    let mut prev_cased = false;
    for i in 0..s.len() {
        let c = s[i];
        if prev_cased {
            push_lower(&mut out, s, i);
        } else {
            match lookup(TITLE, c) {
                Some(m) => out.extend(m.chars()),
                None => out.push(c),
            }
        }
        prev_cased = is_cased(c);
    }
    out
}

pub fn casefold(s: &[char]) -> Vec<char> {
    let mut out = Vec::with_capacity(s.len());
    for &c in s {
        match lookup(CASEFOLD, c) {
            Some(m) => out.extend(m.chars()),
            None => out.push(c),
        }
    }
    out
}

pub fn strip(s: &[char]) -> &[char] {
    rstrip(lstrip(s))
}

pub fn lstrip(s: &[char]) -> &[char] {
    let mut i = 0;
    while i < s.len() && is_space(s[i]) {
        i += 1;
    }
    &s[i..]
}

pub fn rstrip(s: &[char]) -> &[char] {
    let mut j = s.len();
    while j > 0 && is_space(s[j - 1]) {
        j -= 1;
    }
    &s[..j]
}

pub fn rstrip_char(s: &[char], c: char) -> &[char] {
    let mut j = s.len();
    while j > 0 && s[j - 1] == c {
        j -= 1;
    }
    &s[..j]
}

/// `str.expandtabs(4)`.
pub fn expandtabs(s: &[char]) -> Vec<char> {
    let mut out = Vec::with_capacity(s.len());
    let mut col = 0usize;
    for &c in s {
        if c == '\t' {
            let n = 4 - col % 4;
            for _ in 0..n {
                out.push(' ');
            }
            col += n;
        } else {
            out.push(c);
            col = if c == '\n' || c == '\r' { 0 } else { col + 1 };
        }
    }
    out
}

pub fn expanded_len(s: &[char]) -> usize {
    let mut col = 0usize;
    let mut total = 0usize;
    for &c in s {
        if c == '\t' {
            let n = 4 - col % 4;
            col += n;
            total += n;
        } else {
            total += 1;
            col = if c == '\n' || c == '\r' { 0 } else { col + 1 };
        }
    }
    total
}

pub fn contains(hay: &[char], needle: &[char]) -> bool {
    find(hay, needle, 0).is_some()
}

pub fn find(hay: &[char], needle: &[char], from: usize) -> Option<usize> {
    if needle.is_empty() {
        return if from <= hay.len() { Some(from) } else { None };
    }
    if needle.len() > hay.len() {
        return None;
    }
    (from..=hay.len() - needle.len()).find(|&i| &hay[i..i + needle.len()] == needle)
}

pub fn replace(s: &[char], from: &[char], to: &[char]) -> Vec<char> {
    let mut out = Vec::with_capacity(s.len());
    let mut i = 0;
    while i < s.len() {
        if !from.is_empty() && s[i..].starts_with(from) {
            out.extend_from_slice(to);
            i += from.len();
        } else {
            out.push(s[i]);
            i += 1;
        }
    }
    out
}

pub fn chars(s: &str) -> Vec<char> {
    s.chars().collect()
}

pub fn string(s: &[char]) -> String {
    s.iter().collect()
}

/// `int(s)` for a run of `\d` characters.
pub fn int_of(s: &[char]) -> u64 {
    s.iter().fold(0u64, |a, &c| a * 10 + digit_value(c).unwrap() as u64)
}

/// `s.isdigit()` restricted to what marko feeds it (runs of `\d`).
pub fn isdigit(s: &[char]) -> bool {
    !s.is_empty() && s.iter().all(|&c| is_digit(c))
}
