use std::io;
use std::os::raw::*;
use std::rc::Rc;
use lex::*;
use pos::*;
use prefix::*;

#[derive(Debug, PartialEq)]
pub struct Type {
    name: Tag<TypeName>,
    is_const: bool,
    is_volatile: bool,
}

#[derive(Debug, PartialEq)]
pub enum TypeName {
    Int,
    Long,
    Custom(String),
    //Pointer(Box<Type>),
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    type_: Type,
    name: Tag<String>,
    value: Option<Tag<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    declarations: Vec<Tag<Declaration>>,
    statements: Vec<Tag<Statement>>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    If(Tag<Expression>, Box<Tag<Statement>>, Option<Box<Tag<Statement>>>),
    While(Tag<Expression>, Box<Tag<Statement>>),
    Block(Block),
    Expression(Expression),
}
use self::Statement::*;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Funcall(String, Vec<Tag<Expression>>),
    Progn(Vec<Tag<Expression>>),
    Label(String),
    Int(c_int),
    Long(c_long),
    Void,
}
use self::Expression::*;

impl Tag<Expression> {
    fn to_progn(self) -> Vec<Tag<Expression>> {
        match self.value {
            Progn(vec) => vec,
            _ => vec![self],
        }
    }
}

fn parse_statement<I: Iterator<Item = Tag<Token>>>(first: Tag<Token>, tokens: &mut PrefixIterator<I>)
                                                   -> io::Result<(Option<Tag<Token>>, Tag<Statement>)> {
    match first.value {
        Token::Semicolon => {
            Ok((tokens.next(), first.with_value(Expression(Void))))
        },
        Token::If => {
            let open_paren = try!(tokens.next().ok_or_else(
                || io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Found EOF after `if` keyword", first.pos))));
            if open_paren.value != Token::OpenParen {
                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Did not find `(` after `if`", open_paren.pos)));
            }
            let (next, cond) = try!(parse_paren(&open_paren.pos, tokens));
            let (next, true_) = try!(parse_statement(try!(next.ok_or_else(
                || io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Found EOF when expected body of if statement here", first.pos)))), tokens));
            let (next, false_) = match next {
                Some(next) =>
                    match next.value {
                        Token::Else => {
                            let (next, else_) = try!(parse_statement(try!(tokens.next().ok_or_else(
                                || io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Found EOF when expected body of else statement here", next.pos)))), tokens));
                            (next, Some(else_))
                        },
                        _ => (Some(next), None),
                    },
                None => (None, None),
            };
            Ok((next, first.with_value(If(cond, Box::new(true_), false_.map(|f| Box::new(f))))))
        },
        Token::While => {
            let open_paren = try!(tokens.next().ok_or_else(
                || io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Found EOF after `while` keyword", first.pos))));
            if open_paren.value != Token::OpenParen {
                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Did not find `(` after `while`", open_paren.pos)));
            }
            let (next, cond) = try!(parse_paren(&open_paren.pos, tokens));
            let (next, body) = try!(parse_statement(try!(next.ok_or_else(
                || io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Found EOF when expected body of while statement here", first.pos)))), tokens));
            Ok((next, first.with_value(While(cond, Box::new(body)))))
        },
        Token::Int(_) | Token::Long(_) => {
            let first_pos = first.pos.clone();
            let (next, expr) = try!(parse_expression(first, tokens));
            match next {
                Some(next) => match next.value {
                    Token::Semicolon => Ok((tokens.next(), expr.map(|expr| Expression(expr)))),
                    _ => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                            format!("{} Expected semicolon here", next.pos))),
                },
                None => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                           format!("{} Expected semicolon at end of statement started here", first_pos))),
            }
        },
        Token::Else =>
            Err(io::Error::new(io::ErrorKind::InvalidInput,
                               format!("{} Unexpected `else`.  No `if` found", first.pos))),
        Token::Label(_) => {
            let first_pos = first.pos.clone();
            let (next, expr) = try!(parse_expression(first, tokens));
            match next {
                Some(next) => match next.value {
                    Token::Semicolon => Ok((tokens.next(), expr.map(Expression))),
                    _ => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                            format!("{} Expected semicolon here", next.pos))),
                },
                None => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                           format!("{} Expected semicolon at end of statement started here", first_pos))),
            }
        },
        Token::OpenParen => {
            let (next, expr) = try!(parse_paren(&first.pos, tokens));
            Ok((next, expr.map(Expression)))
        },
        Token::OpenCurly => {
            let mut declarations = Vec::new();
            let mut statements = Vec::new();
            let mut next = tokens.next();
            loop {
                match next {
                    Some(n) => match n.value {
                        Token::CloseCurly =>
                            return Ok((tokens.next(), first.with_value(Block(Block { declarations, statements })))),
                        Token::Label(_) | Token::Const | Token::Volatile | Token::KeywordInt | Token::KeywordLong => {
                            let maybe = try!(parse_maybe_variable_declaration(n, tokens));
                            next = maybe.0;
                            match maybe.1 {
                                Ok(declaration) => declarations.push(declaration),
                                Err(statement) => {
                                    statements.push(statement);
                                    break;
                                },
                            }
                            continue;
                        },
                        _ => {
                            next = Some(n);
                            break;
                        },
                    },
                    None => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                      format!("{} Found EOF, expected `}}` to close `{{` here", first.pos))),
                }
            }
            loop {
                let parsed = match next {
                    Some(next) => match next.value {
                        Token::CloseCurly => break,
                        _ => try!(parse_statement(next, tokens)),
                    },
                    None => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                      format!("{} Found EOF, expected `}}` to close `{{` here", first.pos))),
                };
                next = parsed.0;
                statements.push(parsed.1);
            }
            Ok((tokens.next(), first.with_value(Block(Block { declarations, statements }))))
        },
        Token::CloseParen => {
            Err(io::Error::new(io::ErrorKind::InvalidInput,
                               format!("{} Unexpected `)` here", first.pos)))
        },
        Token::CloseCurly => {
            Err(io::Error::new(io::ErrorKind::InvalidInput,
                               format!("{} Unexpected `}}` here", first.pos)))
        },
        _ => unimplemented!("{:?}", first),
    }
}

fn parse_maybe_variable_declaration<I: Iterator<Item = Tag<Token>>>(first: Tag<Token>, tokens: &mut PrefixIterator<I>)
                                                                    -> io::Result<(Option<Tag<Token>>, Result<Tag<Declaration>, Tag<Statement>>)> {
    let first_pos = first.pos.clone();
    let mut next = Some(first);
    let mut is_const = false;
    let mut is_volatile = false;
    let mut embedded_type_name = None;
    loop {
        match next {
            Some(n) => match n.value {
                Token::KeywordInt => {
                    if embedded_type_name.is_some() {
                        println!("{:?}", embedded_type_name);
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected keyword `int` after type declaration", n.pos)));
                    } else {
                        embedded_type_name = Some(Tag::new(TypeName::Int, n.pos));
                        next = tokens.next();
                    }
                },
                Token::KeywordLong => {
                    if embedded_type_name.is_some() {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected keyword `long` after type declaration", n.pos)));
                    } else {
                        embedded_type_name = Some(Tag::new(TypeName::Long, n.pos));
                        next = tokens.next();
                    }
                },
                Token::Label(label) => match embedded_type_name {
                    Some(type_name) => {
                        let type_ = Type { name: type_name, is_const, is_volatile };
                        let next = tokens.next();
                        match next {
                            Some(Tag { value: Token::Semicolon, pos: _ }) => {
                                return Ok((tokens.next(),
                                           Ok(Tag::new(Declaration { type_, name: Tag::new(label, n.pos), value: None },
                                                       first_pos))));
                            },
                            Some(Tag { value: _, pos }) => {
                                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                          format!("{} Expected semicolon here", pos)));
                            },
                            None => {
                                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                          format!("{} Found EOF after variable declaration here, expected a `;`", first_pos)));
                            }
                        }
                    },
                    None => {
                        embedded_type_name = Some(Tag::new(TypeName::Custom(label), n.pos));
                        next = tokens.next();
                    },
                },
                Token::Const =>
                    if is_const {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected `const`, variable was already declared `const`", n.pos)));
                    } else {
                        is_const = true;
                        next = tokens.next();
                    },
                Token::Volatile => 
                    if is_volatile {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected `volatile`, variable was already declared `volatile`", n.pos)));
                    } else {
                        is_volatile = true;
                        next = tokens.next();
                    },
                _ => {
                    // Maybe got a type name but no variable name
                    match embedded_type_name {
                        Some(Tag { value: TypeName::Custom(label), pos }) => {
                            if !is_const && !is_volatile {
                                tokens.push(n);
                                let (next, statement) = try!(parse_statement(
                                    Tag::new(Token::Label(label), pos),
                                    tokens));
                                return Ok((next, Err(statement)));
                            }
                        },
                        None => {
                            if !is_const && !is_volatile {
                                return Ok((tokens.next(), Err(Tag::new(Expression(Void), n.pos))));
                            } else {
                                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                          format!("{} Expected a type and then a variable name to finish the declaration", n.pos)));
                            }
                        },
                        _ => (),
                    }
                    return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                              format!("{} Invalid symbol after type descriptors", n.pos)));
                },
            },
            None => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                              format!("{} Unexpected EOF, unclosed statement starting here", first_pos))),
        }
    }
}

fn parse_paren<I: Iterator<Item = Tag<Token>>>(open_paren_pos: &Pos, tokens: &mut PrefixIterator<I>)
                                               -> io::Result<(Option<Tag<Token>>, Tag<Expression>)> {
    let (next, expression) =
        try!(parse_expression(try!(tokens.next().ok_or_else(
            || io::Error::new(io::ErrorKind::InvalidInput,
                              format!("{} Found EOF when expected `)`", open_paren_pos)))), tokens));
    match next {
        Some(other) => if other.value == Token::CloseParen {
            Ok((tokens.next(), expression))
        } else {
            Err(io::Error::new(io::ErrorKind::InvalidInput,
                               format!("{} Expected a `)` here", other.pos)))
        },
        None => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                   format!("{} Found EOF when expected a `)` to close the `(` here", open_paren_pos))),
    }
}

fn parse_expression<I: Iterator<Item = Tag<Token>>>(first: Tag<Token>, tokens: &mut PrefixIterator<I>)
                                                    -> io::Result<(Option<Tag<Token>>, Tag<Expression>)> {
    match first.value {
        Token::Label(first_name) => {
            match tokens.next() {
                Some(second) =>
                    match second.value {
                        Token::OpenParen => {
                            let (next, expression) = try!(parse_paren(&second.pos, tokens));
                            Ok((next, Tag::new(Funcall(first_name, expression.to_progn()), first.pos)))
                        },
                        Token::CloseParen | Token::Semicolon | Token::CloseCurly => {
                            Ok((Some(second), Tag::new(Label(first_name), first.pos)))
                        },
                        Token::Comma => {
                            parse_comma(Tag::new(Label(first_name), first.pos), &second.pos, tokens)
                        },
                        Token::Label(_) | Token::Const | Token::Volatile =>
                            Err(io::Error::new(io::ErrorKind::InvalidInput,
                                               format!("{} Unexpected variable declaration here, only expressions are allowed", first.pos))),
                        _ => unimplemented!("{:?}", second),
                    },
                None => Ok((None, Tag::new(Label(first_name), first.pos))),
            }
        },
        Token::Int(i) => parse_possible_comma(first.with_value(Int(i)), tokens),
        Token::Long(l) => parse_possible_comma(first.with_value(Long(l)), tokens),
        Token::KeywordInt | Token::KeywordLong | Token::Const | Token::Volatile =>
            Err(io::Error::new(io::ErrorKind::InvalidInput,
                               format!("{} Unexpected variable declaration here, only expressions are allowed", first.pos))),
        v => unimplemented!("{:?}", v),
    }
}

fn parse_possible_comma<I: Iterator<Item = Tag<Token>>>(first: Tag<Expression>, tokens: &mut PrefixIterator<I>)
                                                        -> io::Result<(Option<Tag<Token>>, Tag<Expression>)> {
    let next = tokens.next();
    match next {
        Some(next) =>
            match next.value {
                Token::Comma => {
                    parse_comma(first, &next.pos, tokens)
                },
                _ => Ok((Some(next), first)),
            },
        None => Ok((None, first)),
    }
}

fn parse_comma<I: Iterator<Item = Tag<Token>>>(first: Tag<Expression>, comma_pos: &Pos, tokens: &mut PrefixIterator<I>)
                                               -> io::Result<(Option<Tag<Token>>, Tag<Expression>)> {
    let (next, expression) = try!(parse_expression(try!(tokens.next().ok_or_else(
        || io::Error::new(io::ErrorKind::InvalidInput,
                          format!("{} Expected expression after `,` here", comma_pos)))),
                                                   tokens));
    let mut vec = expression.to_progn();
    let first_pos = first.pos.clone();
    vec.insert(0, first);
    Ok((next, Tag::new(Progn(vec), first_pos)))
}

pub fn parse(tokens: Vec<Tag<Token>>) -> io::Result<Vec<Tag<Statement>>> {
    let mut statements = Vec::new();
    let mut tokens = PrefixIterator::new(tokens.into_iter().fuse());
    match tokens.next() {
        Some(token) => {
            let mut next = token;
            loop {
                let (n, statement) = try!(parse_statement(next, &mut tokens));
                statements.push(statement);
                match n.or_else(|| tokens.next()) {
                    Some(n) => next = n,
                    None => break,
                }
            }
        },
        None => (),
    }
    Ok(statements)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::error::Error;

    fn pos(line: usize, column: usize) -> Pos {
        Pos::new(Rc::new("*stdin*".to_owned()), line, column)
    }

    #[test]
    fn parse_expression() {
        assert_eq!(
            vec![Tag::new(Expression(Funcall("hi".to_owned(),
                                             vec![Tag::new(Label("baby".to_owned()), pos(1, 3)),
                                                  Tag::new(Label("cakes".to_owned()), pos(1, 9))])),
                          pos(1, 0))],
            parse(lex("*stdin*", "hi(baby, cakes);".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_if_no_brace() {
        assert_eq!(
            vec![Tag::new(If(Tag::new(Int(1), pos(1, 4)),
                             Box::new(Tag::new(Expression(Int(2)), pos(1, 7))),
                             None),
                          pos(1, 0))],
            parse(lex("*stdin*", "if (1) 2;".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_if_no_brace_no_semicolon_panic() {
        let err = parse(lex("*stdin*", "if (1) 2 else 3;".chars()).unwrap()).unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:9: Expected semicolon here",
            err.description());
    }

    #[test]
    fn parse_if_else_no_braces() {
        assert_eq!(
            vec![Tag::new(If(Tag::new(Int(1), pos(1, 4)),
                             Box::new(Tag::new(Expression(Int(2)), pos(1, 7))),
                             Some(Box::new(Tag::new(Expression(Int(3)), pos(1, 15))))),
                          pos(1, 0))],
            parse(lex("*stdin*", "if (1) 2; else 3;".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_if_else_do_nothing_panic() {
        assert_eq!(
            vec![Tag::new(Token::If, pos(1, 0)),
                 Tag::new(Token::OpenParen, pos(1, 2)),
                 Tag::new(Token::Int(1), pos(1, 3)),
                 Tag::new(Token::CloseParen, pos(1, 4)),
                 Tag::new(Token::Semicolon, pos(1, 5)),
                 Tag::new(Token::Else, pos(1, 6)),
                 Tag::new(Token::Semicolon, pos(1, 10))],
            lex("*stdin*", "if(1);else;".chars()).unwrap());
        assert_eq!(
            vec![Tag::new(If(Tag::new(Int(1), pos(1, 3)),
                             Box::new(Tag::new(Expression(Void), pos(1, 5))),
                             Some(Box::new(Tag::new(Expression(Void), pos(1, 10))))),
                          pos(1, 0))],
            parse(lex("*stdin*", "if(1);else;".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_if_eof_panic() {
        let err = parse(lex("*stdin*", "if;".chars()).unwrap()).unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:2: Did not find `(` after `if`",
            err.description());
    }

    #[test]
    fn parse_if_braces() {
        assert_eq!(
            vec![Tag::new(If(Tag::new(Int(1), pos(1, 4)),
                             Box::new(Tag::new(Block(Block {
                                 declarations: Vec::new(),
                                 statements: vec![Tag::new(Expression(Progn(vec![Tag::new(Int(2), pos(1, 9)),
                                                                                 Tag::new(Int(3), pos(1, 12))])),
                                                           pos(1, 9)),
                                                  Tag::new(Expression(Int(4)), pos(1, 15))]
                             }), pos(1, 7))),
                             None),
                          pos(1, 0))],
            parse(lex("*stdin*", "if (1) { 2, 3; 4; }".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_while_braces() {
        assert_eq!(
            vec![Tag::new(While(Tag::new(Int(1), pos(1, 7)),
                                Box::new(Tag::new(Block(Block {
                                    declarations: Vec::new(),
                                    statements: vec![Tag::new(Expression(Progn(vec![Tag::new(Int(1), pos(1, 12)),
                                                                                    Tag::new(Int(2), pos(1, 15))])),
                                                              pos(1, 12)),
                                                     Tag::new(Expression(Int(3)), pos(1, 18))]
                                }), pos(1, 10)))), pos(1, 0)),
                 Tag::new(Expression(Int(4)), pos(1, 23))],
            parse(lex("*stdin*", "while (1) { 1, 2; 3; } 4;".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_empty_braces() {
        assert_eq!(
            vec![Tag::new(Block(Block {
                declarations: Vec::new(),
                statements: Vec::new()
            }), pos(1, 0))],
            parse(lex("*stdin*", "{}".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_comma_int() {
        assert_eq!(
            vec![Tag::new(Expression(Progn(vec![Tag::new(Int(1), pos(1, 0)),
                                                Tag::new(Int(2), pos(1, 3))])),
                          pos(1, 0))],
            parse(lex("*stdin*", "1, 2;".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_label_semicolon_required_panic() {
        let err = parse(lex("*stdin*", "hi".chars()).unwrap()).unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:0: Expected semicolon at end of statement started here",
            err.description());
    }

    #[test]
    fn parse_3_levels() {
        assert_eq!(
            vec![Tag::new(If(Tag::new(Int(1), pos(2, 4)),
                    Box::new(Tag::new(Block(
                        Block {
                            declarations: Vec::new(),
                            statements: vec![
                                Tag::new(
                                    While(Tag::new(Int(2), pos(3, 11)),
                                          Box::new(Tag::new(Block(Block {
                                              declarations: Vec::new(),
                                              statements: vec![
                                                  Tag::new(If(Tag::new(Int(3), pos(4, 12)),
                                                              Box::new(Tag::new(Block(Block {
                                                                  declarations: Vec::new(),
                                                                  statements: vec![Tag::new(Expression(Label("hi".to_owned())), pos(5, 12))]
                                                              }), pos(4, 15))),
                                                              Some(Box::new(Tag::new(Block(Block {
                                                                  declarations: Vec::new(),
                                                                  statements: vec![Tag::new(Expression(Label("bye".to_owned())), pos(7, 12))]
                                                              }), pos(6, 15))))),
                                                           pos(4, 8))]
                                          }), pos(3, 14)))),
                                    pos(3, 4))]
                        }), pos(2, 7))),
                             Some(Box::new(Tag::new(Expression(Int(4)), pos(11, 4))))),
            pos(2, 0))],
            parse(lex("*stdin*", "
if (1) {
    while (2) {
        if (3) {
            hi;
        } else {
            bye;
        }
    }
} else
    4;".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_no_semicolon_after_else_panic() {
        let err = parse(lex("*stdin*",
        "{
    if (1) {
        2;
    } else
        3
}".chars()).unwrap()).unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:6:0: Expected semicolon here",
            err.description());
    }

    #[test]
    fn parse_variable_decl_panic() {
        let err = parse(lex("*stdin*", "{ int i }".chars()).unwrap()).unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:8: Expected semicolon here",
            err.description());
    }

    #[test]
    fn parse_variable_decl() {
        assert_eq!(
            vec![Tag::new(Block(Block {
                declarations: vec![Tag::new(Declaration {
                    type_: Type {
                        name: Tag::new(TypeName::Int, pos(1, 2)),
                        is_const: false,
                        is_volatile: false,
                    },
                    name: Tag::new("i".to_owned(), pos(1, 6)),
                    value: None,
                }, pos(1, 2))],
                statements: Vec::new(),
            }), pos(1, 0))],
            parse(lex("*stdin*", "{ int i; }".chars()).unwrap()).unwrap());
    }

    #[test]
    fn parse_variable_decl_in_parens_panic() {
        let err = parse(lex("*stdin*", "(type i)".chars()).unwrap()).unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:1: Unexpected variable declaration here, only expressions are allowed",
            err.description());
    }

    #[test]
    fn parse_variable_decl_in_parens_panic_2() {
        let err = parse(lex("*stdin*", "(int i)".chars()).unwrap()).unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:1: Unexpected variable declaration here, only expressions are allowed",
            err.description());
    }
}
