use std::io;
use std::os::raw::*;
use std::rc::Rc;
use pos::*;
use prefix::*;
use std::iter::Fuse;

struct Tagged<I: Iterator<Item = char>> {
    pos: Pos,
    iter: Fuse<I>,
}

impl<I: Iterator<Item = char>> Tagged<I> {
    fn new(file_name: &str, iter: Fuse<I>) -> Self {
        Tagged { pos: Pos::new(Rc::new(file_name.to_owned()), 1, 0), iter }
    }
}

impl<I: Iterator<Item = char>> Iterator for Tagged<I> {
    type Item = Tag<char>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(c) => {
                let pos = self.pos.clone();
                self.pos.advance(c);
                Some(Tag::new(c, pos))
            },
            None => None,
        }
    }
}

struct Prelexer<I: Iterator<Item = char>> {
    iter: PrefixIterator<Tagged<I>>,
}

impl<I: Iterator<Item = char>> Prelexer<I> {
    fn new(file_name: &str, iter: Fuse<I>) -> Self {
        Prelexer {
            iter: PrefixIterator::new(Tagged::new(file_name, iter)),
        }
    }

    fn push(&mut self, c: Tag<char>) {
        self.iter.push(c)
    }
}

impl<I: Iterator<Item = char>> Iterator for Prelexer<I> {
    type Item = io::Result<Tag<char>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(c) => {
                if c.value == '\\' {
                    match self.iter.next() {
                        Some(escaped) => {
                            if escaped.value == '\n' {
                                self.iter.next().map(Ok)
                            } else {
                                self.iter.push(escaped);
                                Some(Ok(c))
                            }
                        },
                        None => {
                            Some(Ok(c))
                        }
                    }
                } else if c.value == '/' {
                    match self.iter.next() {
                        Some(Tag { value: '*', pos: _ }) => {
                            let mut next = self.iter.next();
                            loop {
                                match next {
                                    Some(Tag { value: '*', pos: _ }) => {
                                        match self.iter.next() {
                                            Some(Tag { value: '/', pos: _ }) => return Some(Ok(Tag::new(' ', c.pos))),
                                            Some(c) => next = Some(c),
                                            None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                                   format!("{} Found EOF when expected end of comment started here", c.pos)))),
                                        }
                                    },
                                    Some(_) => next = self.iter.next(),
                                    None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                           format!("{} Found EOF when expected end of comment started here", c.pos)))),
                                }
                            }
                        },
                        Some(n) => {
                            self.iter.push(n);
                            Some(Ok(c))
                        },
                        None => None
                    }
                } else {
                    Some(Ok(c))
                }
            },
            None => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Label(String),
    If,
    Else,
    While,
    Const,
    Volatile,
    Int(c_int),
    Long(c_long),
    KeywordInt,
    KeywordLong,
    Void,
    Semicolon,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    Comma,
    Set,
    Macro(Vec<Tag<Token>>),
}
use self::Token::*;

pub struct Lexer<I: Iterator<Item = char>> {
    iter: Prelexer<I>,
    hash_valid_here: bool,
    reserve: Vec<Tag<Token>>,
}

fn narrow_number(x: c_long) -> Token {
    if x >= c_int::min_value() as c_long && x <= c_int::max_value() as c_long {
        Int(x as c_int)
    } else {
        Long(x as c_long)
    }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(file_name: &str, iter: I) -> Self {
        Lexer {
            iter: Prelexer::new(file_name, iter.fuse()),
            hash_valid_here: true,
            reserve: Vec::new(),
        }
    }

    pub fn push(&mut self, t: Tag<Token>) {
        self.reserve.push(t);
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = io::Result<Tag<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let result =
            match self.reserve.pop() {
                Some(result) => Some(Ok(result)),
                None => 'outer: loop {
                    let ch = match self.iter.next() {
                        Some(Ok(ch)) => ch,
                        Some(Err(e)) => break 'outer Some(Err(e)),
                        None => break 'outer None,
                    };
                    if ch.value == '\n' {
                        self.hash_valid_here = true;
                    } else if ch.value.is_whitespace() {
                    } else if ch.value == '#' {
                        let mut vec = Vec::new();
                        loop {
                            match self.next() {
                                Some(Ok(t)) => {
                                    if t.pos.same_line(&ch.pos) {
                                        vec.push(t);
                                    } else {
                                        self.reserve.push(t);
                                        break;
                                    }
                                },
                                Some(Err(e)) => break 'outer Some(Err(e)),
                                None => break,
                            }
                        }
                        break 'outer Some(Ok(Tag::new(Macro(vec), ch.pos)));
                    } else if ch.value == ';' {
                        break 'outer Some(Ok(Tag::new(Semicolon, ch.pos)));
                    } else if ch.value == ',' {
                        break 'outer Some(Ok(Tag::new(Comma, ch.pos)));
                    } else if ch.value == '=' {
                        break 'outer Some(Ok(Tag::new(Set, ch.pos)));
                    } else if ch.value == '(' {
                        break 'outer Some(Ok(Tag::new(OpenParen, ch.pos)));
                    } else if ch.value == ')' {
                        break 'outer Some(Ok(Tag::new(CloseParen, ch.pos)));
                    } else if ch.value == '[' {
                        break 'outer Some(Ok(Tag::new(OpenSquare, ch.pos)));
                    } else if ch.value == ']' {
                        break 'outer Some(Ok(Tag::new(CloseSquare, ch.pos)));
                    } else if ch.value == '{' {
                        break 'outer Some(Ok(Tag::new(OpenCurly, ch.pos)));
                    } else if ch.value == '}' {
                        break 'outer Some(Ok(Tag::new(CloseCurly, ch.pos)));
                    } else if ch.value.is_digit(10) {
                        let mut res: c_long = ch.value.to_digit(10).unwrap() as c_long;
                        let start_pos = ch.pos;
                        let mut c = self.iter.next();
                        loop {
                            match c {
                                Some(Ok(cc)) => {
                                    if cc.value.is_digit(10) {
                                        res *= 10;
                                        res += cc.value.to_digit(10).unwrap() as c_long;
                                        c = self.iter.next();
                                    } else if cc.value.is_whitespace() || cc.value == ')' || cc.value == ',' ||
                                        cc.value == '[' || cc.value == ']' || cc.value == '{' || cc.value == '}' || cc.value == ';' {
                                            self.iter.push(cc);
                                            break;
                                        } else {
                                            break 'outer Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                                 format!("{} Invalid end of number `{}`", cc.pos, cc.value))));
                                        }
                                },
                                Some(Err(e)) => break 'outer Some(Err(e)),
                                None => break,
                            }
                        }
                        break 'outer Some(Ok(Tag::new(narrow_number(res), start_pos)));
                    } else if ch.value.is_alphabetic() || ch.value == '_' {
                        let mut label = String::new();
                        label.push(ch.value);
                        let start_pos = ch.pos;
                        let mut c = self.iter.next();
                        loop {
                            match c {
                                Some(Ok(cc)) => {
                                    if cc.value.is_alphanumeric() || cc.value == '_' {
                                        label.push(cc.value);
                                        c = self.iter.next();
                                    } else if cc.value.is_whitespace() || cc.value == '(' || cc.value == ')' || cc.value == ',' ||
                                        cc.value == '[' || cc.value == ']' || cc.value == '{' || cc.value == '}' || cc.value == ';' {
                                            self.iter.push(cc);
                                            break 'outer Some(Ok(Tag::new(Label(label), start_pos)));
                                        } else {
                                            break 'outer Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                                 format!("{} Invalid end of label `{}`", cc.pos, cc.value))));
                                        }
                                },
                                Some(Err(e)) => break 'outer Some(Err(e)),
                                None => break,
                            }
                        }
                        break 'outer Some(Ok(Tag::new(Label(label), start_pos)));
                    } else {
                        break 'outer Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                             format!("{} Invalid character `{}`", ch.pos, ch.value))));
                    }
                },
            };
        self.hash_valid_here = false;
        result
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::error::Error;

    fn pos(line: usize, column: usize) -> Pos {
        Pos::new(Rc::new("*stdin*".to_owned()), line, column)
    }

    fn prelex(s: &str) -> io::Result<Vec<Tag<char>>> {
        Prelexer::new("*stdin*", s.chars().fuse()).collect()
    }

    #[test]
    fn prelex_backslash_newline() {
        assert_eq!(vec![Tag::new('h', pos(1, 0)),
                        Tag::new('e', pos(1, 1)),
                        Tag::new('l', pos(2, 0)),
                        Tag::new('l', pos(2, 1)),
                        Tag::new('o', pos(2, 2))],
                   prelex("he\\\nllo").unwrap());
    }

    #[test]
    fn prelex_delete_block_comment() {
        assert_eq!(vec![Tag::new('h', pos(1, 0)),
                        Tag::new(' ', pos(1, 1)),
                        Tag::new('e', pos(1, 13))],
                   prelex("h/*ello lov*/e").unwrap());
    }

    #[test]
    fn prelex_delete_multiline_block_comment() {
        assert_eq!(vec![Tag::new('h', pos(1, 0)),
                        Tag::new(' ', pos(1, 1)),
                        Tag::new('e', pos(3, 10))],
                   prelex("h/*ao  nteuheou\nao hte  nu\toehun\noh\tenueo*/e").unwrap());
    }

    #[test]
    fn prelex_dont_delete_line_comment() {
        assert_eq!(vec![Tag::new('h', pos(1, 0)),
                        Tag::new('/', pos(1, 1)),
                        Tag::new('/', pos(1, 2)),
                        Tag::new('c', pos(1, 3))],
                   prelex("h//c").unwrap());
    }

    #[test]
    fn prelex_unclosed_line_comment() {
        assert_eq!("*stdin*:1:12: Found EOF when expected end of comment started here",
                   format!("{}", prelex("hello world /*/").unwrap_err()));
    }

    fn lex(s: &str) -> io::Result<Vec<Tag<Token>>> {
        Lexer::new("*stdin*", s.chars()).collect()
    }

    #[test]
    fn lex_label_panic() {
        let err = lex("heyo'").err().unwrap();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:4: Invalid end of label `'`",
            err.description());
    }

    #[test]
    fn lex_label_1() {
        assert_eq!(
            vec![Tag::new(Label("aoeu".to_owned()), pos(1, 0))],
            lex("aoeu").unwrap());
    }

    #[test]
    fn lex_label_2() {
        assert_eq!(
            vec![Tag::new(Label("a_test_caseForYourMomma".to_owned()), pos(1, 0))],
            lex("a_test_caseForYourMomma").unwrap());
    }

    #[test]
    fn lex_number_panic() {
        let err = lex("1231239213a").err().unwrap();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:10: Invalid end of number `a`",
            err.description());
    }

    #[test]
    fn lex_number() {
        assert_eq!(
            vec![Tag::new(Int(1231239213), pos(1, 0))],
            lex("1231239213").unwrap());
        assert_eq!(
            vec![Tag::new(Long(1231239213123123), pos(1, 0))],
            lex("1231239213123123").unwrap());
    }

    #[test]
    fn lex_empty() {
        assert!(lex("").unwrap().is_empty());
    }

    #[test]
    fn lex_expression_1() {
        assert_eq!(
            vec![Tag::new(Int(123), pos(1, 0)),
                 Tag::new(Comma, pos(1, 3)),
                 Tag::new(Int(456), pos(1, 4))],
            lex("123,456").unwrap());
    }

    #[test]
    fn lex_expression_2() {
        assert_eq!(
            vec![Tag::new(OpenParen, pos(1, 0)),
                 Tag::new(Int(123), pos(1, 1)),
                 Tag::new(CloseParen, pos(1, 4)),
                 Tag::new(Semicolon, pos(1, 5))],
            lex("(123);").unwrap());
        assert_eq!(
            vec![Tag::new(OpenSquare, pos(1, 0)),
                 Tag::new(OpenSquare, pos(1, 1)),
                 Tag::new(Label("aoeu".to_owned()), pos(1, 2)),
                 Tag::new(CloseSquare, pos(1, 6)),
                 Tag::new(Comma, pos(1, 7)),
                 Tag::new(OpenCurly, pos(1, 8)),
                 Tag::new(Int(123), pos(1, 9)),
                 Tag::new(CloseCurly, pos(1, 12)),
                 Tag::new(CloseSquare, pos(1, 13)),
                 Tag::new(Semicolon, pos(1, 14))],
            lex("[[aoeu],{123}];").unwrap());
    }

    #[test]
    fn lex_newline_beginning() {
        assert_eq!(
            vec![Tag::new(OpenSquare, pos(2, 0)),
                 Tag::new(OpenParen, pos(2, 1)),
                 Tag::new(CloseParen, pos(4, 0)),
                 Tag::new(CloseSquare, pos(4, 1))],
            lex("
[(

)]").unwrap());
    }

    #[test]
    fn lex_newline_beginning_2() {
        assert_eq!(
            vec![Tag::new(OpenCurly, pos(1, 0)),
                 Tag::new(Label("if".to_owned()), pos(2, 4)),
                 Tag::new(OpenParen, pos(2, 7)),
                 Tag::new(Int(1), pos(2, 8)),
                 Tag::new(CloseParen, pos(2, 9)),
                 Tag::new(OpenCurly, pos(2, 11)),
                 Tag::new(Int(2), pos(3, 8)),
                 Tag::new(Semicolon, pos(3, 9)),
                 Tag::new(CloseCurly, pos(4, 4)),
                 Tag::new(Label("else".to_owned()), pos(4, 6)),
                 Tag::new(Int(3), pos(5, 8)),
                 Tag::new(CloseCurly, pos(6, 0)),],
            lex("{
    if (1) {
        2;
    } else
        3
}").unwrap());
    }
}
