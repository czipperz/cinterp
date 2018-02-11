use std::io;
use std::os::raw::*;
use std::rc::Rc;
use pos::*;
use preprocess::*;
use prefix::*;

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
}
use self::Token::*;

pub struct Lexer<I: Iterator<Item = char>> {
    iter: PrefixIterator<Preprocessor<I>>,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(file_name: &str, iter: I) -> Self {
        Lexer { iter: PrefixIterator::new(Preprocessor::new(file_name, iter.fuse())) }
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = io::Result<Tag<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut ch = match self.iter.next() {
            Some(Ok(ch)) => ch,
            Some(Err(e)) => return Some(Err(e)),
            None => return None,
        };
        'outer: loop {
            if ch.value.is_whitespace() {
                ch = match self.iter.next() {
                    Some(Ok(ch)) => ch,
                    Some(Err(e)) => return Some(Err(e)),
                    None => return None,
                };
            } else if ch.value == ';' {
                return Some(Ok(Tag::new(Semicolon, ch.pos)));
            } else if ch.value == ',' {
                return Some(Ok(Tag::new(Comma, ch.pos)));
            } else if ch.value == '=' {
                return Some(Ok(Tag::new(Set, ch.pos)));
            } else if ch.value == '(' {
                return Some(Ok(Tag::new(OpenParen, ch.pos)));
            } else if ch.value == ')' {
                return Some(Ok(Tag::new(CloseParen, ch.pos)));
            } else if ch.value == '[' {
                return Some(Ok(Tag::new(OpenSquare, ch.pos)));
            } else if ch.value == ']' {
                return Some(Ok(Tag::new(CloseSquare, ch.pos)));
            } else if ch.value == '{' {
                return Some(Ok(Tag::new(OpenCurly, ch.pos)));
            } else if ch.value == '}' {
                return Some(Ok(Tag::new(CloseCurly, ch.pos)));
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
                                    self.iter.push(Ok(cc));
                                    return Some(Ok(Tag::new(narrow_number(res), start_pos)));
                                } else {
                                    return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                   format!("{} Invalid end of number `{}`", cc.pos, cc.value))));
                                }
                        },
                        Some(Err(e)) => {
                            return Some(Err(e));
                        },
                        None => {
                            return Some(Ok(Tag::new(narrow_number(res), start_pos)));
                        },
                    }
                }
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
                                    self.iter.push(Ok(cc));
                                    return Some(Ok(Tag::new(wrap_label(label), start_pos)));
                                } else {
                                    return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                   format!("{} Invalid end of label `{}`", cc.pos, cc.value))));
                                }
                        },
                        Some(Err(e)) => return Some(Err(e)),
                        None => break,
                    }
                }
                return Some(Ok(Tag::new(wrap_label(label), start_pos)));
            } else {
                return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                               format!("{} Invalid character `{}`", ch.pos, ch.value))));
            }
        }
    }
}

fn narrow_number(x: c_long) -> Token {
    if x >= c_int::min_value() as c_long && x <= c_int::max_value() as c_long {
        Int(x as c_int)
    } else {
        Long(x as c_long)
    }
}

fn wrap_label(s: String) -> Token {
    if s == "if" {
        Token::If
    } else if s == "else" {
        Token::Else
    } else if s == "while" {
        Token::While
    } else if s == "int" {
        Token::KeywordInt
    } else if s == "long" {
        Token::KeywordLong
    } else if s == "const" {
        Token::Const
    } else if s == "volatile" {
        Token::Volatile
    } else if s == "void" {
        Token::Void
    } else {
        Token::Label(s)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::error::Error;

    fn pos(line: usize, column: usize) -> Pos {
        Pos::new(Rc::new("*stdin*".to_owned()), line, column)
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
                 Tag::new(If, pos(2, 4)),
                 Tag::new(OpenParen, pos(2, 7)),
                 Tag::new(Int(1), pos(2, 8)),
                 Tag::new(CloseParen, pos(2, 9)),
                 Tag::new(OpenCurly, pos(2, 11)),
                 Tag::new(Int(2), pos(3, 8)),
                 Tag::new(Semicolon, pos(3, 9)),
                 Tag::new(CloseCurly, pos(4, 4)),
                 Tag::new(Else, pos(4, 6)),
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
