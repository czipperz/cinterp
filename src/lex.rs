use std::io;
use std::os::raw::*;
use pos::*;
use std::rc::Rc;

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
    Semicolon,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    Comma,
}
use self::Token::*;

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
    } else {
        Token::Label(s)
    }
}

pub fn lex<I: Iterator<Item = char>>(file: &str, iter: I) -> io::Result<Vec<Tag<Token>>> {
    let mut tokens = Vec::new();
    let mut iter = iter.fuse();
    let file = Rc::new(file.to_owned());
    let mut pos = Pos::new(file.clone(), 1, 0);
    let mut ch = match iter.next() {
        Some(ch) => ch,
        None => return Ok(tokens),
    };
    'outer: loop {
        if ch.is_whitespace() {
        } else if ch == ';' {
            tokens.push(Tag::new(Semicolon, pos.clone()));
        } else if ch == ',' {
            tokens.push(Tag::new(Comma, pos.clone()));
        } else if ch == '(' {
            tokens.push(Tag::new(OpenParen, pos.clone()));
        } else if ch == ')' {
            tokens.push(Tag::new(CloseParen, pos.clone()));
        } else if ch == '[' {
            tokens.push(Tag::new(OpenSquare, pos.clone()));
        } else if ch == ']' {
            tokens.push(Tag::new(CloseSquare, pos.clone()));
        } else if ch == '{' {
            tokens.push(Tag::new(OpenCurly, pos.clone()));
        } else if ch == '}' {
            tokens.push(Tag::new(CloseCurly, pos.clone()));
        } else if ch.is_digit(10) {
            let mut res: c_long = ch.to_digit(10).unwrap() as c_long;
            let start_pos = pos.clone();
            pos.advance(ch);
            let mut c = iter.next();
            loop {
                match c {
                    Some(cc) => {
                        if cc.is_digit(10) {
                            res *= 10;
                            res += cc.to_digit(10).unwrap() as c_long;
                            pos.advance(cc);
                            c = iter.next();
                        } else if cc.is_whitespace() || cc == ')' || cc == ',' ||
                            cc == '[' || cc == ']' || cc == '{' || cc == '}' || cc == ';' {
                            ch = cc;
                            tokens.push(Tag::new(narrow_number(res), start_pos));
                            continue 'outer;
                        } else {
                            return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                      format!("{} Invalid end of number `{}`", pos, cc)));
                        }
                    },
                    None => {
                        tokens.push(Tag::new(narrow_number(res), start_pos));
                        break 'outer;
                    },
                }
            }
        } else if ch.is_alphabetic() {
            let mut label = String::new();
            label.push(ch);
            let start_pos = pos.clone();
            pos.advance(ch);
            let mut c = iter.next();
            while let Some(cc) = c {
                if cc.is_alphanumeric() {
                    label.push(cc);
                    pos.advance(cc);
                    c = iter.next();
                } else if cc.is_whitespace() || cc == '(' || cc == ')' || cc == ',' ||
                    cc == '[' || cc == ']' || cc == '{' || cc == '}' || cc == ';' {
                    ch = cc;
                    tokens.push(Tag::new(wrap_label(label), start_pos));
                    continue 'outer;
                } else {
                    return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                              format!("{} Invalid end of label `{}`", pos, cc)));
                }
            }
            tokens.push(Tag::new(wrap_label(label), start_pos));
        } else {
            return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                      format!("{} Invalid character `{}`", pos, ch)));
        }

        pos.advance(ch);
        ch = match iter.next() {
            Some(ch) => ch,
            None => return Ok(tokens),
        };
    }
    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::error::Error;

    fn pos(line: usize, column: usize) -> Pos {
        Pos::new(Rc::new("*stdin*".to_owned()), line, column)
    }

    #[test]
    fn lex_label_panic() {
        let err = lex("*stdin*", "heyo'".chars()).err().unwrap();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:4: Invalid end of label `'`",
            err.description());
    }

    #[test]
    fn lex_label() {
        assert_eq!(
            vec![Tag::new(Label("aoeu".to_owned()), pos(1, 0))],
            lex("*stdin*", "aoeu".chars()).unwrap());
    }

    #[test]
    fn lex_number_panic() {
        let err = lex("*stdin*", "1231239213a".chars()).err().unwrap();
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
            lex("*stdin*", "1231239213".chars()).unwrap());
        assert_eq!(
            vec![Tag::new(Long(1231239213123123), pos(1, 0))],
            lex("*stdin*", "1231239213123123".chars()).unwrap());
    }

    #[test]
    fn lex_empty() {
        assert!(lex("*stdin*", "".chars()).unwrap().is_empty());
    }

    #[test]
    fn lex_expression_1() {
        assert_eq!(
            vec![Tag::new(Int(123), pos(1, 0)),
                 Tag::new(Comma, pos(1, 3)),
                 Tag::new(Int(456), pos(1, 4))],
            lex("*stdin*", "123,456".chars()).unwrap());
    }

    #[test]
    fn lex_expression_2() {
        assert_eq!(
            vec![Tag::new(OpenParen, pos(1, 0)),
                 Tag::new(Int(123), pos(1, 1)),
                 Tag::new(CloseParen, pos(1, 4)),
                 Tag::new(Semicolon, pos(1, 5))],
            lex("*stdin*", "(123);".chars()).unwrap());
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
            lex("*stdin*", "[[aoeu],{123}];".chars()).unwrap());
    }

    #[test]
    fn lex_newline_beginning() {
        assert_eq!(
            vec![Tag::new(OpenSquare, pos(2, 0)),
                 Tag::new(OpenParen, pos(2, 1)),
                 Tag::new(CloseParen, pos(4, 0)),
                 Tag::new(CloseSquare, pos(4, 1))],
            lex("*stdin*", "
[(

)]".chars()).unwrap());
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
            lex("*stdin*", "{
    if (1) {
        2;
    } else
        3
}".chars()).unwrap());
    }
}
