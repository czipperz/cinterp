use pos::*;
use lex::*;
use std::io;
use std::collections::HashMap;
use Token::*;

pub struct Preprocessor<I: Iterator<Item = char>> {
    //files: Vec<Lexer<ReaderIterator>>,
    definitions: HashMap<String, Vec<Tag<Token>>>,
    main_lexer: Lexer<I>,
    if_macros: usize,
}

impl<I: Iterator<Item = char>> Preprocessor<I> {
    pub fn new(main_lexer: Lexer<I>) -> Self {
        Preprocessor {
            definitions: HashMap::new(),
            main_lexer,
            if_macros: 0,
        }
    }

    fn skip_over_macros_until_else(&mut self) -> Option<io::Result<Tag<Vec<Tag<Token>>>>> {
        let mut if_macros = 0;
        loop {
            match self.main_lexer.next() {
                Some(Ok(token)) => {
                    match token.value {
                        Macro(vec) => {
                            {
                                let macro_name = match vec.get(0) {
                                    Some(macro_name) => macro_name,
                                    None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                           format!("{} `#` with no macro here", token.pos)))),
                                };
                                match macro_name.value {
                                    Label(ref l) =>
                                        if l == "endif" {
                                            if if_macros != 0 {
                                                if_macros -= 1;
                                                continue;
                                            }
                                        } else if l == "else" {
                                            if if_macros != 0 {
                                                continue;
                                            }
                                        } else if l == "ifdef" {
                                            if_macros += 1;
                                            continue;
                                        } else {
                                            continue;
                                        },
                                    _ => continue,
                                }
                            }
                            return Some(Ok(Tag::new(vec, token.pos)));
                        },
                        _ => (),
                    }
                },
                Some(Err(e)) => return Some(Err(e)),
                None => return None,
            }
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Preprocessor<I> {
    type Item = io::Result<Tag<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.main_lexer.next() {
            Some(Ok(token)) => match token.value {
                Macro(vec) => {
                    let mut vec_iter = vec.into_iter();
                    let macro_name = match vec_iter.next() {
                        Some(macro_name) => macro_name,
                        None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                               format!("{} `#` with no macro here", token.pos)))),
                    };
                    match macro_name.value {
                        Label(ref l) =>
                            if l == "define" {
                                let name = match vec_iter.next() {
                                    Some(name) => name,
                                    None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                           format!("{} What are you going to define?", macro_name.pos)))),
                                };
                                let name = match name.value {
                                    Label(name) => name,
                                    _ => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                        format!("{} Invalid definition name here", name.pos)))),
                                };
                                let value = vec_iter.collect();
                                self.definitions.insert(name, value);
                                self.next()
                            } else if l == "undef" {
                                let name = match vec_iter.next() {
                                    Some(name) => name,
                                    None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                           format!("{} What are you going to define?", macro_name.pos)))),
                                };
                                let name = match name.value {
                                    Label(name) => name,
                                    _ => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                        format!("{} Invalid definition name here", name.pos)))),
                                };
                                self.definitions.remove(&name);
                                self.next()
                            } else if l == "ifdef" || l == "ifndef" {
                                let name = match vec_iter.next() {
                                    Some(name) => name,
                                    None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                        format!("{} Found EOF when expected a macro name after #ifdef here", macro_name.pos)))),
                                };
                                let truth = if l == "ifdef" {
                                    let name = match name.value {
                                        Label(name) => name,
                                        _ => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                            format!("{} Invalid macro name here", name.pos)))),
                                    };
                                    self.definitions.contains_key(&name)
                                } else if l == "ifndef" {
                                    let name = match name.value {
                                        Label(name) => name,
                                        _ => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                            format!("{} Invalid macro name here", name.pos)))),
                                    };
                                    !self.definitions.contains_key(&name)
                                } else {
                                    unreachable!()
                                };
                                self.if_macros += 1;
                                if truth {
                                    self.next()
                                } else {
                                    match self.skip_over_macros_until_else() {
                                        Some(Ok(next)) => {
                                            if (match next.value.get(0) {
                                                Some(&Tag { value: Label(ref l), pos: _ }) => {
                                                    if l == "endif" {
                                                        true
                                                    } else if l == "else" {
                                                        false
                                                    } else {
                                                        unreachable!()
                                                    }
                                                },
                                                Some(_) => unimplemented!(),
                                                None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                                       format!("{} Found EOF when expected a macro name after # here", next.pos)))),
                                            }) {
                                                self.main_lexer.push(Tag::new(Macro(next.value), next.pos));
                                            }
                                            self.next()
                                        },
                                        Some(Err(e)) => Some(Err(e)),
                                        None => Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                        format!("{} No #endif to close #ifdef here", macro_name.pos)))),
                                    }
                                }
                            } else if l == "else" {
                                // skip over body until endif
                                match self.skip_over_macros_until_else() {
                                    Some(Ok(next)) => {
                                        self.main_lexer.push(Tag::new(Macro(next.value), next.pos));
                                        self.next()
                                    },
                                    Some(Err(e)) => Some(Err(e)),
                                    None => Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                    format!("{} No close to #else here", macro_name.pos)))),
                                }
                            } else if l == "endif" {
                                if self.if_macros == 0 {
                                    return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                   format!("{} No #if to end here", macro_name.pos))))
                                }
                                self.if_macros -= 1;
                                self.next()
                            } else {
                                panic!()
                            },
                        x => panic!("{:?}", x)
                    }
                },
                Label(s) => {
                    Some(Ok(Tag::new(
                        if self.definitions.contains_key(&s) {
                            for token in self.definitions.get(&s).as_ref().unwrap().iter().rev() {
                                self.main_lexer.push(token.clone());
                            }
                            return self.next()
                        } else if s == "if" {
                            If
                        } else if s == "else" {
                            Else
                        } else if s == "while" {
                            While
                        } else if s == "int" {
                            KeywordInt
                        } else if s == "long" {
                            KeywordLong
                        } else if s == "const" {
                            Const
                        } else if s == "volatile" {
                            Volatile
                        } else if s == "void" {
                            Void
                        } else if s == "return" {
                            Return
                        } else {
                            Label(s)
                        }, token.pos)))
                },
                _ => Some(Ok(token)),
            },
            Some(Err(e)) => Some(Err(e)),
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;
    use std::rc::Rc;

    fn preprocess(s: &str) -> io::Result<Vec<Tag<Token>>> {
        Preprocessor::new(Lexer::new("*stdin*", s.chars())).collect()
    }

    fn pos(y: usize, x: usize) -> Pos {
        Pos::new(Rc::new("*stdin*".to_owned()), y, x)
    }

    #[test]
    fn preprocess_no_macros() {
        assert_eq!(
            vec![Tag::new(KeywordInt, pos(1, 0)),
                 Tag::new(Label("i".to_owned()), pos(1, 4)),
                 Tag::new(Label("is".to_owned()), pos(2, 0)),
                 Tag::new(Label("babe".to_owned()), pos(3, 0))],
            preprocess("int i
is
babe").unwrap());
    }

    #[test]
    fn preprocess_define_as_int_1() {
        assert_eq!(
            vec![Tag::new(KeywordInt, pos(2, 0)),
                 Tag::new(Label("i".to_owned()), pos(2, 4)),
                 Tag::new(Set, pos(2, 6)),
                 Tag::new(Int(1), pos(1, 14)),
                 Tag::new(Semicolon, pos(2, 13))],
            preprocess("#define VALUE 1
int i = VALUE;").unwrap());
    }

    #[test]
    fn preprocess_define_as_int_2() {
        assert_eq!(
            vec![Tag::new(KeywordInt, pos(2, 0)),
                 Tag::new(Label("i".to_owned()), pos(2, 4)),
                 Tag::new(Set, pos(1, 13)),
                 Tag::new(Int(1), pos(1, 15)),
                 Tag::new(Semicolon, pos(1, 16))],
            preprocess("#define EQ_1 = 1;
int i EQ_1").unwrap());
    }

    #[test]
    fn preprocess_ifdef_else_defined() {
        assert_eq!(
            vec![Tag::new(Int(123), pos(3, 0))],
            preprocess("#define VALUE
#ifdef VALUE
123
#else
456
#endif").unwrap());
    }

    #[test]
    fn preprocess_ifdef_else_not_defined() {
        assert_eq!(
            vec![Tag::new(Int(456), pos(4, 0))],
            preprocess("#ifdef VALUE
123
#else
456
#endif").unwrap());
    }

    #[test]
    fn preprocess_ifdef_not_defined() {
        assert_eq!(
            vec![Tag::new(Int(1), pos(1, 0)),
                 Tag::new(Int(3), pos(5, 0))],
            preprocess("1
#ifdef VALUE
2
#endif
3").unwrap());
    }

    #[test]
    fn preprocess_ifdef_defined() {
        assert_eq!(
            vec![Tag::new(Int(1), pos(2, 0)),
                 Tag::new(Int(2), pos(4, 0)),
                 Tag::new(Int(3), pos(6, 0))],
            preprocess("#define VALUE AOOSEUHT 1230123
1
#ifdef VALUE
2
#endif
3").unwrap());
    }

    #[test]
    fn preprocess_ifdef_not_macro_name_panic() {
        assert_eq!(
            "*stdin*:2:7: Invalid macro name here",
            format!("{}", preprocess("1
#ifdef 123
2
#endif
3").unwrap_err()));
    }

    #[test]
    fn preprocess_ifndef_then_define() {
        assert_eq!(
            vec![Tag::new(If, pos(2, 14))],
            preprocess("#ifndef Value
#define Value if
#endif
Value").unwrap());
    }

    #[test]
    fn preprocess_define_then_redefine_component() {
        assert_eq!(
            vec![Tag::new(Label("f".to_owned()), pos(2, 10)),
                 Tag::new(OpenParen, pos(2, 11)),
                 Tag::new(Int(1), pos(1, 15)),
                 Tag::new(CloseParen, pos(2, 18)),
                 Tag::new(Label("f".to_owned()), pos(2, 10)),
                 Tag::new(OpenParen, pos(2, 11)),
                 Tag::new(Int(2), pos(4, 15)),
                 Tag::new(CloseParen, pos(2, 18))],
            preprocess("#define Intern 1
#define V f(Intern)
V
#define Intern 2
V").unwrap());
    }
}
