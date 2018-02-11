use std::io;
use std::fmt;
use std::os::raw::*;
use lex::*;
use pos::*;
use prefix::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    pub name: TypeName,
    pub is_const: bool,
    pub is_volatile: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeName {
    Void,
    Int,
    Long,
    Custom(String),
    //Pointer(Box<Type>),
    Array(Box<TypeName>, Option<Tag<Expression>>),
    FunctionPointer(Box<Tag<Type>>, Vec<Tag<Type>>),
}

pub const SIZE_T: TypeName = TypeName::Long;

impl TypeName {
    pub fn can_cast_to(&self, other: &TypeName) -> bool {
        use TypeName::*;
        match (self, other) {
            (_, &Void) => true,
            (&Int, &Long) => true,
            (&Long, &Int) => true,
            (a, b) => a == b,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}
use self::Declaration::*;

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    pub type_: Tag<Type>,
    pub name: Tag<String>,
    pub value: Option<Tag<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub return_type: Tag<Type>,
    pub name: Tag<String>,
    pub params: Vec<(Option<String>, Tag<Type>)>,
    pub body: Option<Tag<Block>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub declarations: Vec<Tag<Declaration>>,
    pub statements: Vec<Tag<Statement>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Funcall(Box<Tag<Expression>>, Vec<Tag<Expression>>),
    Progn(Vec<Tag<Expression>>),
    Label(String),
    Index(Box<Tag<Expression>>, Box<Tag<Expression>>),
    Set(Box<Tag<Expression>>, Box<Tag<Expression>>),
    Cast(Box<Tag<Type>>, Box<Tag<Expression>>),
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

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    If(Tag<Expression>, Box<Tag<Statement>>, Option<Box<Tag<Statement>>>),
    While(Tag<Expression>, Box<Tag<Statement>>),
    Block(Block),
    Expression(Expression),
}
use self::Statement::*;

impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.is_const {
            try!(write!(fmt, "const "));
        }
        if self.is_volatile {
            try!(write!(fmt, "volatile "));
        }
        write!(fmt, "{}", self.name)
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use TypeName::*;
        match self {
            &Void => write!(fmt, "void"),
            &Int => write!(fmt, "int"),
            &Long => write!(fmt, "long"),
            &Custom(ref c) => write!(fmt, "{}", c),
            &Array(ref embedded, Some(ref expr)) => write!(fmt, "{}[{}]", embedded, expr.value),
            &Array(ref embedded, None) => write!(fmt, "{}[]", embedded),
            &FunctionPointer(ref type_, ref params) => {
                try!(write!(fmt, "{}(*)(", type_.value));
                let mut first = true;
                for param in params {
                    if first {
                        first = false;
                    } else {
                        try!(write!(fmt, ", "));
                    }
                    try!(write!(fmt, "{}", param.value));
                }
                write!(fmt, ")")
            }
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &VariableDeclaration(ref v) => write!(fmt, "{}", v),
            &FunctionDeclaration(ref v) => write!(fmt, "{}", v),
        }
    }
}

impl fmt::Display for VariableDeclaration {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{} {}", self.type_.value, self.name.value));
        if let Some(ref expr) = self.value {
            try!(write!(fmt, " = {}", expr.value));
        }
        write!(fmt, ";")
    }
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{} {}(", self.return_type.value, self.name.value));
        let mut first = true;
        for &(ref name, ref type_) in &self.params {
            if first {
                first = false;
            } else {
                try!(write!(fmt, ", "));
            }
            try!(write!(fmt, "{}", type_.value));
            match name {
                &Some(ref name) => try!(write!(fmt, " {}", name)),
                &None => (),
            }
        }
        try!(write!(fmt, ")"));
        match self.body {
            Some(ref body) => write!(fmt, " {}", body.value),
            None => write!(fmt, ";"),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{{"));
        if !self.declarations.is_empty() || !self.statements.is_empty() {
            try!(write!(fmt, " "));
        }
        for declaration in &self.declarations {
            try!(write!(fmt, "{} ", declaration.value));
        }
        for statement in &self.statements {
            try!(write!(fmt, "{} ", statement.value));
        }
        write!(fmt, "}}")
    }
}

fn fmt_list<'a, T: 'a + fmt::Display, I: Iterator<Item = &'a T>>(iter: I, fmt: &mut fmt::Formatter) -> fmt::Result {
    let mut first = true;
    for e in iter {
        if first {
            first = false;
        } else {
            try!(write!(fmt, ", "));
        }
        try!(write!(fmt, "{}", e));
    }
    Ok(())
}

fn parenthesize_progn(expression: &Expression, fmt: &mut fmt::Formatter) -> fmt::Result {
    match expression {
        expression@&Progn(_) => write!(fmt, "({})", expression),
        expression => write!(fmt, "{}", expression),
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Funcall(ref name, ref arguments) => {
                try!(parenthesize_progn(&name.value, fmt));
                try!(write!(fmt, "("));
                try!(fmt_list(arguments.iter().map(|t| &t.value), fmt));
                write!(fmt, ")")
            },
            &Progn(ref expressions) => fmt_list(expressions.iter().map(|t| &t.value), fmt),
            &Label(ref l) => write!(fmt, "{}", l),
            &Index(ref expression, ref index) => {
                try!(parenthesize_progn(&expression.as_ref().value, fmt));
                write!(fmt, "[{}]", index.value)
            },
            &Set(ref name, ref value) => {
                try!(parenthesize_progn(&name.as_ref().value, fmt));
                try!(write!(fmt, " = "));
                parenthesize_progn(&value.as_ref().value, fmt)
            },
            &Cast(ref type_, ref expression) => {
                try!(write!(fmt, "({})", type_.value));
                parenthesize_progn(&expression.as_ref().value, fmt)
            },
            &Int(i) => write!(fmt, "{}", i),
            &Long(l) => write!(fmt, "{}", l),
            &Void => Ok(()),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &If(ref cond, ref true_, ref false_) => {
                try!(write!(fmt, "if ({}) {}", cond.value, true_.value));
                if let &Some(ref false_) = false_ {
                    write!(fmt, "else {}", false_.value)
                } else {
                    Ok(())
                }
            },
            &While(ref cond, ref body) => write!(fmt, "while ({}) {}", cond.value, body.value),
            &Block(ref block) => write!(fmt, "{}", block),
            &Expression(ref expression) => write!(fmt, "{};", expression),
        }
    }
}

fn parse_top_level<I: Iterator<Item = io::Result<Tag<Token>>>>(first: Tag<Token>, tokens: &mut PrefixIterator<I>)
                                                   -> io::Result<(Option<Tag<Token>>, Tag<Declaration>)> {
    let (n, d) = try!(parse_maybe_variable_declaration(first, tokens));
    match d {
        Ok(d) => Ok((n, d)),
        Err(e) => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                     format!("{} Only declarations are allowed at the top level", e.pos))),
    }
}

fn to_result<T, E>(x: Option<Result<T, E>>) -> Result<Option<T>, E> {
    match x {
        Some(Ok(t)) => Ok(Some(t)),
        Some(Err(e)) => Err(e),
        None => Ok(None),
    }
}

fn parse_statement<I: Iterator<Item = io::Result<Tag<Token>>>>(first: Tag<Token>, tokens: &mut PrefixIterator<I>)
                                                   -> io::Result<(Option<Tag<Token>>, Tag<Statement>)> {
    match first.value {
        Token::Semicolon => {
            Ok((try!(to_result(tokens.next())), first.with_value(Expression(Void))))
        },
        Token::If => {
            let open_paren = try!(try!(to_result(tokens.next())).ok_or_else(
                || io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Found EOF after `if` keyword", first.pos))));
            if open_paren.value != Token::OpenParen {
                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Did not find `(` after `if`", open_paren.pos)));
            }
            let (next, cond) = try!(parse_paren(&open_paren.pos, tokens));
            let cond = try!(cond.map_err(|t| io::Error::new(io::ErrorKind::InvalidInput,
                                                            format!("{} Condition of `if` statement cannot be a type name", t.pos))));
            let (next, true_) = try!(parse_statement(try!(next.ok_or_else(
                || io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Found EOF when expected body of if statement here", first.pos)))), tokens));
            let (next, false_) = match next {
                Some(next) =>
                    match next.value {
                        Token::Else => {
                            let (next, else_) = try!(parse_statement(try!(try!(to_result(tokens.next())).ok_or_else(
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
            let open_paren = try!(try!(to_result(tokens.next())).ok_or_else(
                || io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Found EOF after `while` keyword", first.pos))));
            if open_paren.value != Token::OpenParen {
                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Did not find `(` after `while`", open_paren.pos)));
            }
            let (next, cond) = try!(parse_paren(&open_paren.pos, tokens));
            let cond = try!(cond.map_err(|t| io::Error::new(io::ErrorKind::InvalidInput,
                                                            format!("{} Condition of `while` statement cannot be a type name", t.pos))));
            let (next, body) = try!(parse_statement(try!(next.ok_or_else(
                || io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Found EOF when expected body of while statement here", first.pos)))), tokens));
            Ok((next, first.with_value(While(cond, Box::new(body)))))
        },
        Token::Else =>
            Err(io::Error::new(io::ErrorKind::InvalidInput,
                               format!("{} Unexpected `else`.  No `if` found", first.pos))),
        Token::Label(_) | Token::Int(_) | Token::Long(_) | Token::OpenParen => {
            let first_pos = first.pos.clone();
            let (next, expr) = try!(parse_expression(first, tokens, false));
            let expr = expr.unwrap();
            match next {
                Some(next) => match next.value {
                    Token::Semicolon => Ok((try!(to_result(tokens.next())), expr.map(Expression))),
                    _ => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                            format!("{} Expected semicolon here", next.pos))),
                },
                None => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                           format!("{} Expected semicolon at end of statement started here", first_pos))),
            }
        },
        Token::OpenCurly => {
            let (next, block) = try!(parse_block(first.pos.clone(), tokens));
            Ok((next, Tag::new(Block(block.value), block.pos)))
        },
        _ => {
            Err(io::Error::new(io::ErrorKind::InvalidInput,
                               format!("{} Invalid start of a statement", first.pos)))
        },
    }
}

fn parse_params<I: Iterator<Item = io::Result<Tag<Token>>>>(open_paren_pos: &Pos, tokens: &mut PrefixIterator<I>)
                                                -> io::Result<(Option<Tag<Token>>, Vec<(Option<String>, Tag<Type>)>)> {
    let mut params = Vec::new();
    loop {
        match try!(try!(to_result(tokens.next())).ok_or_else(
            || io::Error::new(io::ErrorKind::InvalidInput,
                              format!("{} Found EOF, expected `)` to close `(` here", open_paren_pos)))) {
            Tag { value: Token::CloseParen, pos: _ } => break,
            t => {
                let (next, type_) = try!(parse_type(t, tokens));
                let next = try!(next.ok_or_else(
                    || io::Error::new(io::ErrorKind::InvalidInput,
                                      format!("{} Found EOF, expected `)` to close `(` here", open_paren_pos))));
                match next.value {
                    Token::CloseParen => {
                        if let TypeName::Void = type_.value.name {
                            if params.is_empty() {
                                break
                            }
                        }
                        params.push((None, type_));
                        break;
                    },
                    Token::Comma => params.push((None, type_)),
                    Token::Label(l) => {
                        let next = try!(try!(to_result(tokens.next())).ok_or_else(
                            || io::Error::new(io::ErrorKind::InvalidInput,
                                              format!("{} Found EOF, expected `)` to close `(` here", open_paren_pos))));
                        match next.value {
                            Token::CloseParen => {
                                params.push((Some(l), type_));
                                break;
                            },
                            Token::Comma => params.push((Some(l), type_)),
                            _ => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                           format!("{} Invalid symbol here, must be `,` or `)`", next.pos))),
                        }
                    },
                    _ => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                   format!("{} Invalid symbol here, must be `,`, `)`, or a variable name", next.pos))),
                }
            },
        }
    }
    Ok((try!(to_result(tokens.next())), params))
}

fn parse_block<I: Iterator<Item = io::Result<Tag<Token>>>>(open_curly_pos: Pos, tokens: &mut PrefixIterator<I>)
                                               -> io::Result<(Option<Tag<Token>>, Tag<Block>)> {
    let mut declarations = Vec::new();
    let mut statements = Vec::new();
    let mut next = try!(to_result(tokens.next()));
    loop {
        let n = try!(next.ok_or_else(
            || io::Error::new(io::ErrorKind::InvalidInput,
                              format!("{} Found EOF, expected `}}` to close `{{` here", open_curly_pos))));
        match n.value {
            Token::CloseCurly =>
                return Ok((try!(to_result(tokens.next())),
                           Tag::new(Block { declarations, statements },
                                    open_curly_pos))),
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
        }
    }
    loop {
        let parsed = match next {
            Some(next) => match next.value {
                Token::CloseCurly => break,
                _ => try!(parse_statement(next, tokens)),
            },
            None => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                              format!("{} Found EOF, expected `}}` to close `{{` here", open_curly_pos))),
        };
        next = parsed.0;
        statements.push(parsed.1);
    }
    Ok((try!(to_result(tokens.next())),
        Tag::new(Block { declarations, statements }, open_curly_pos)))
}

fn parse_maybe_variable_declaration<I: Iterator<Item = io::Result<Tag<Token>>>>(first: Tag<Token>, tokens: &mut PrefixIterator<I>)
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
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected keyword `int` after type declaration", n.pos)));
                    } else {
                        embedded_type_name = Some((TypeName::Int, n.pos));
                        next = try!(to_result(tokens.next()));
                    }
                },
                Token::KeywordLong => {
                    if embedded_type_name.is_some() {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected keyword `long` after type declaration", n.pos)));
                    } else {
                        embedded_type_name = Some((TypeName::Long, n.pos));
                        next = try!(to_result(tokens.next()));
                    }
                },
                Token::Void => {
                    if embedded_type_name.is_some() {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected keyword `void` after type declaration", n.pos)));
                    } else {
                        embedded_type_name = Some((TypeName::Void, n.pos));
                        next = try!(to_result(tokens.next()));
                    }
                },
                Token::Label(label) => match embedded_type_name {
                    Some((type_name, type_pos)) => {
                        let type_ = Tag::new(Type { name: type_name, is_const, is_volatile }, type_pos);
                        let next = try!(to_result(tokens.next()));
                        match next {
                            Some(Tag { value: Token::Semicolon, pos: _ }) => {
                                return Ok((try!(to_result(tokens.next())),
                                           Ok(Tag::new(VariableDeclaration(VariableDeclaration { type_, name: Tag::new(label, n.pos), value: None }),
                                                       first_pos))));
                            },
                            Some(Tag { value: Token::Set, pos }) => {
                                let (next, value) = try!(parse_expression(try!(try!(to_result(tokens.next())).ok_or_else(
                                    || io::Error::new(io::ErrorKind::InvalidInput,
                                                      format!("{} Found EOF after `=` here, expected an expression", pos)))), tokens, false));
                                let value = value.unwrap();
                                match next {
                                    Some(Tag { value: Token::Semicolon, pos: _ }) => {
                                        return Ok((try!(to_result(tokens.next())),
                                                   Ok(Tag::new(VariableDeclaration(VariableDeclaration {
                                                       type_,
                                                       name: Tag::new(label, n.pos),
                                                       value: Some(value),
                                                   }), first_pos))));
                                    },
                                    Some(Tag { value: _, pos }) => {
                                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                  format!("{} Expected semicolon here", pos)));
                                    },
                                    None => {
                                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                  format!("{} Found EOF after variable declaration starting here, expected a  `;`", first_pos)));
                                    }
                                }
                            },
                            Some(Tag { value: Token::OpenSquare, pos }) => {
                                let (next, expr) = try!(parse_expression(try!(try!(to_result(tokens.next())).ok_or_else(
                                    || io::Error::new(io::ErrorKind::InvalidInput,
                                                      format!("{} ", pos)))), tokens, false));
                                let expr = expr.unwrap();
                                match next {
                                    Some(Tag { value: Token::CloseSquare, pos: _ }) => {
                                        match try!(to_result(tokens.next())) {
                                            Some(Tag { value: Token::Semicolon, pos: _ }) => {
                                                return Ok((try!(to_result(tokens.next())),
                                                           Ok(Tag::new(VariableDeclaration(VariableDeclaration {
                                                               type_: Tag::new(Type {
                                                                   name: TypeName::Array(Box::new(type_.value.name),
                                                                                         Some(expr)),
                                                                   is_const: type_.value.is_const, is_volatile: type_.value.is_volatile },
                                                                               type_.pos),
                                                               name: Tag::new(label, n.pos), value: None }),
                                                                       first_pos))));
                                            },
                                            Some(Tag { value: _, pos }) => {
                                                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                          format!("{} Expected semicolon here", pos)));
                                            },
                                            None => {
                                                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                          format!("{} Found EOF after variable declaration starting here, expected a  `;`", first_pos)));
                                            }
                                        }
                                    },
                                    Some(Tag { value: _, pos }) => {
                                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                  format!("{} Expected `]` here", pos)));
                                    },
                                    None => {
                                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                  format!("{} Found EOF, expected `]` to close `[` here", pos)));
                                    }
                                }
                            },
                            Some(Tag { value: Token::OpenParen, pos }) => {
                                let (next, params) = try!(parse_params(&pos, tokens));
                                let next = try!(next.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput,
                                                                                  format!("{} Found EOF, expected code block or semicolon at end of function declaration starting here", first_pos))));
                                let (next, body) = match next.value {
                                    Token::OpenCurly => {
                                        let (next, body) = try!(parse_block(next.pos, tokens));
                                        (next, Some(body))
                                    },
                                    Token::Semicolon => (try!(to_result(tokens.next())), None),
                                    _ =>
                                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                  format!("{} Found EOF, expected code block or semicolon at end of function declaration starting here", first_pos))),
                                };
                                return Ok((next,
                                           Ok(Tag::new(FunctionDeclaration(FunctionDeclaration {
                                               return_type: type_,
                                               name: Tag::new(label, n.pos),
                                               params,
                                               body,
                                           }),
                                                       first_pos))));
                            },
                            Some(Tag { value: _, pos }) => {
                                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                          format!("{} Expected semicolon here", pos)));
                            },
                            None => {
                                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                          format!("{} Found EOF after variable declaration starting here, expected a `;`", first_pos)));
                            }
                        }
                    },
                    None => {
                        embedded_type_name = Some((TypeName::Custom(label), n.pos));
                        next = try!(to_result(tokens.next()));
                    },
                },
                Token::Const =>
                    if is_const {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected `const`, variable was already declared `const`", n.pos)));
                    } else {
                        is_const = true;
                        next = try!(to_result(tokens.next()));
                    },
                Token::Volatile => 
                    if is_volatile {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Unexpected `volatile`, variable was already declared `volatile`", n.pos)));
                    } else {
                        is_volatile = true;
                        next = try!(to_result(tokens.next()));
                    },
                _ => {
                    // Maybe got a type name but no variable name
                    match embedded_type_name {
                        Some((TypeName::Custom(label), pos)) => {
                            if !is_const && !is_volatile {
                                tokens.push(Ok(n));
                                let (next, statement) = try!(parse_statement(
                                    Tag::new(Token::Label(label), pos),
                                    tokens));
                                return Ok((next, Err(statement)));
                            }
                        },
                        None => {
                            if !is_const && !is_volatile {
                                let (next, statement) = try!(parse_statement(n, tokens));
                                return Ok((next, Err(statement)));
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

fn parse_paren<I: Iterator<Item = io::Result<Tag<Token>>>>(open_paren_pos: &Pos, tokens: &mut PrefixIterator<I>)
                                               -> io::Result<(Option<Tag<Token>>, Result<Tag<Expression>, Tag<Type>>)> {
    match try!(to_result(tokens.next())) {
        Some(Tag { value: Token::CloseParen, pos: _ }) =>
            Ok((try!(to_result(tokens.next())), Ok(Tag::new(Progn(Vec::new()), open_paren_pos.clone())))),
        Some(first) => {
            let (next, expression) = try!(parse_expression(first, tokens, true));
            match next {
                Some(Tag { value: Token::CloseParen, pos: _ }) => Ok((try!(to_result(tokens.next())), expression)),
                Some(Tag { value: _, pos }) => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                  format!("{} Expected a `)` here", pos))),
                None => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                           format!("{} Found EOF when expected a `)` to close the `(` here", open_paren_pos))),
            }
        },
        None => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                   format!("{} Found EOF when expected `)`", open_paren_pos))),
    }
}

fn parse_type<I: Iterator<Item = io::Result<Tag<Token>>>>(first: Tag<Token>, tokens: &mut PrefixIterator<I>)
                                              -> io::Result<(Option<Tag<Token>>, Tag<Type>)> {
    let mut is_const = false;
    let mut is_volatile = false;
    let first_pos = first.pos;
    let mut next = Some(first.value);
    let mut base = Tag::new(Type { name: loop {
        match next {
            Some(nextu) => match nextu {
                Token::Const => {
                    is_const = true;
                    next = try!(to_result(tokens.next())).map(|x| x.value);
                },
                Token::Volatile => {
                    is_volatile = true;
                    next = try!(to_result(tokens.next())).map(|x| x.value);
                },
                Token::KeywordInt => break TypeName::Int,
                Token::KeywordLong => break TypeName::Long,
                Token::Label(s) => break TypeName::Custom(s),
                Token::Void => break TypeName::Void,
                _ => unimplemented!("{:?}", nextu),
            },
            None => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                              format!("{} Incomplete type started here", first_pos))),
        }
    }, is_const, is_volatile }, first_pos);

    let mut next = try!(to_result(tokens.next()));
    loop {
        match next {
            Some(Tag { value: Token::OpenSquare, pos }) => {
                unimplemented!()
            }
            _ => return Ok((next, base)),
        }
    }
}

fn parse_expression<I: Iterator<Item = io::Result<Tag<Token>>>>(first: Tag<Token>, tokens: &mut PrefixIterator<I>, type_allowed: bool)
                                                    -> io::Result<(Option<Tag<Token>>, Result<Tag<Expression>, Tag<Type>>)> {
    let mut next = None;
    let mut base = match first.value {
        Token::Label(first_name) => Tag::new(Label(first_name), first.pos.clone()),
        Token::Int(i) => Tag::new(Int(i), first.pos.clone()),
        Token::Long(l) => Tag::new(Long(l), first.pos.clone()),
        Token::KeywordInt | Token::KeywordLong | Token::Const | Token::Volatile | Token::Void =>
            if type_allowed {
                let (next, type_) = try!(parse_type(first, tokens));
                return Ok((next, Err(type_)));
            } else {
                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Unexpected variable declaration here, only expressions are allowed", first.pos)));
            },
        Token::Semicolon =>
            return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                      format!("{} Unexpected semicolon here, only expressions are allowed", first.pos))),
        Token::OpenParen => {
            let (n, paren) = try!(parse_paren(&first.pos.clone(), tokens));
            match paren {
                Ok(expr) => {
                    next = Some(n);
                    expr
                },
                Err(type_) => {
                    let (n, expr) = try!(parse_expression(try!(n.ok_or_else(
                        || io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Nothing to type cast", first.pos)))),
                                                          tokens, false));
                    let expr = expr.unwrap();
                    return Ok((n, Ok(Tag::new(Cast(Box::new(type_), Box::new(expr)), first.pos))));
                }
            }
        },
        _ => unimplemented!("{:?}", first),
    };

    let mut next = match next {
        Some(next) => next,
        None => try!(to_result(tokens.next())),
    };
    loop {
        match next {
            Some(nextu) => match nextu.value {
                Token::OpenParen => {
                    let (n, expression) = try!(parse_paren(&nextu.pos, tokens));
                    next = n;
                    let expression = try!(expression.map_err(|t| io::Error::new(io::ErrorKind::InvalidInput,
                                                               format!("{} Nothing to type cast", t.pos))));
                    base = Tag::new(Funcall(Box::new(base), expression.to_progn()), first.pos.clone());
                },
                Token::Semicolon | Token::CloseParen | Token::CloseCurly | Token::CloseSquare =>
                    return Ok((Some(nextu), Ok(base))),
                Token::Comma => {
                    let (n, comma) = try!(parse_comma(base, &nextu.pos, tokens));
                    next = n;
                    base = comma;
                },
                Token::OpenSquare => {
                    let (n, expr) = try!(parse_expression(try!(try!(to_result(tokens.next())).ok_or_else(
                        || io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Found EOF when expected `]` to close `[` here", nextu.pos)))), tokens, false));
                    let expr = expr.unwrap();
                    match n {
                        Some(Tag { value: Token::CloseSquare, pos: _ }) => {
                            next = try!(to_result(tokens.next()));
                            base = Tag::new(Index(Box::new(base), Box::new(expr)), first.pos.clone())
                        },
                        Some(next) => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                format!("{} Expected a `]` here", next.pos))),
                        None => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                          format!("{} Found EOF when expected `]` to close `[` here", nextu.pos))),
                    }
                },
                Token::Set => {
                    let (n, expr) = try!(parse_expression(try!(try!(to_result(tokens.next())).ok_or_else(
                        || io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Found EOF when expecting an expression after `=` here", nextu.pos)))), tokens, false));
                    next = n;
                    base = Tag::new(Set(Box::new(base), Box::new(expr.unwrap())), first.pos.clone());
                },
                _ => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                     format!("{} Unexpected symbol here, expected `(`, `)`, `;`, `}}`, `,`, `[`, `]`, or `=`", nextu.pos))),
            },
            None => return Ok((None, Ok(base))),
        }
    }
}

fn parse_comma<I: Iterator<Item = io::Result<Tag<Token>>>>(first: Tag<Expression>, comma_pos: &Pos, tokens: &mut PrefixIterator<I>)
                                               -> io::Result<(Option<Tag<Token>>, Tag<Expression>)> {
    let (next, expression) = try!(parse_expression(try!(try!(to_result(tokens.next())).ok_or_else(
        || io::Error::new(io::ErrorKind::InvalidInput,
                          format!("{} Expected expression after `,` here", comma_pos)))),
                                                   tokens, false));
    let mut vec = expression.unwrap().to_progn();
    let first_pos = first.pos.clone();
    vec.insert(0, first);
    Ok((next, Tag::new(Progn(vec), first_pos)))
}

pub fn parse<I: Iterator<Item = io::Result<Tag<Token>>>>(tokens: I) -> io::Result<Vec<Tag<Declaration>>> {
    let mut declarations = Vec::new();
    let mut tokens = PrefixIterator::new(tokens.fuse());
    match try!(to_result(tokens.next())) {
        Some(token) => {
            let mut next = token;
            loop {
                let (n, decl) = try!(parse_top_level(next, &mut tokens));
                declarations.push(decl);
                match n {
                    Some(n) => next = n,
                    None => break,
                }
            }
        },
        None => (),
    };
    Ok(declarations)
}

pub fn parse_command_line<I: Iterator<Item = io::Result<Tag<Token>>>>(tokens: I) -> io::Result<Vec<Result<Tag<Declaration>, Tag<Statement>>>> {
    let mut declarations = Vec::new();
    let mut tokens = PrefixIterator::new(tokens.fuse());
    match try!(to_result(tokens.next())) {
        Some(token) => {
            let mut next = token;
            loop {
                let (n, either) = try!(parse_maybe_variable_declaration(next, &mut tokens));
                declarations.push(either);
                match n {
                    Some(n) => next = n,
                    None => break,
                }
            }
        },
        None => (),
    }
    Ok(declarations)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::error::Error;
    use std::rc::Rc;

    fn parse(s: &str) -> io::Result<Vec<Tag<Declaration>>> {
        super::parse(Lexer::new("*stdin*", s.chars()))
    }

    fn parse_command_line(s: &str) -> io::Result<Vec<Result<Tag<Declaration>, Tag<Statement>>>> {
        super::parse_command_line(Lexer::new("*stdin*", s.chars()))
    }

    fn parse_statements(s: &str) -> io::Result<Vec<Tag<Statement>>> {
        let tokens = Lexer::new("*stdin*", s.chars());
        let mut statements = Vec::new();
        let mut tokens = PrefixIterator::new(tokens.into_iter().fuse());
        match try!(to_result(tokens.next())) {
            Some(token) => {
                let mut next = token;
                loop {
                    let (n, statement) = try!(parse_statement(next, &mut tokens));
                    statements.push(statement);
                    match n {
                        Some(n) => next = n,
                        None => break,
                    }
                }
            },
            None => (),
        }
        Ok(statements)
    }

    fn pos(line: usize, column: usize) -> Pos {
        Pos::new(Rc::new("*stdin*".to_owned()), line, column)
    }

    #[test]
    fn parse_funcall_1() {
        assert_eq!(
            vec![Tag::new(Expression(Funcall(Box::new(Tag::new(Label("hi".to_owned()), pos(1, 0))),
                                             vec![Tag::new(Label("baby".to_owned()), pos(1, 3)),
                                                  Tag::new(Label("cakes".to_owned()), pos(1, 9))])),
                          pos(1, 0))],
            parse_statements("hi(baby, cakes);").unwrap());
    }

    #[test]
    fn parse_funcall_2() {
        assert_eq!(
            vec![Tag::new(Expression(Funcall(Box::new(Tag::new(Label("hi".to_owned()), pos(1, 1))),
                                             vec![Tag::new(Label("baby".to_owned()), pos(1, 5)),
                                                  Tag::new(Label("cakes".to_owned()), pos(1, 11))])),
                          pos(1, 0))],
            parse_statements("(hi)(baby, cakes);").unwrap());
    }

    #[test]
    fn parse_if_no_brace() {
        assert_eq!(
            vec![Tag::new(If(Tag::new(Int(1), pos(1, 4)),
                             Box::new(Tag::new(Expression(Int(2)), pos(1, 7))),
                             None),
                          pos(1, 0))],
            parse_statements("if (1) 2;").unwrap());
    }

    #[test]
    fn parse_if_no_brace_no_semicolon_panic() {
        let err = parse_statements("if (1) 2 else 3;").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:9: Unexpected symbol here, expected `(`, `)`, `;`, `}`, `,`, `[`, `]`, or `=`",
            err.description());
    }

    #[test]
    fn parse_if_else_no_braces() {
        assert_eq!(
            vec![Tag::new(If(Tag::new(Int(1), pos(1, 4)),
                             Box::new(Tag::new(Expression(Int(2)), pos(1, 7))),
                             Some(Box::new(Tag::new(Expression(Int(3)), pos(1, 15))))),
                          pos(1, 0))],
            parse_statements("if (1) 2; else 3;").unwrap());
    }

    #[test]
    fn parse_if_else_do_nothing_panic() {
        assert_eq!(
            vec![Tag::new(If(Tag::new(Int(1), pos(1, 3)),
                             Box::new(Tag::new(Expression(Void), pos(1, 5))),
                             Some(Box::new(Tag::new(Expression(Void), pos(1, 10))))),
                          pos(1, 0))],
            parse_statements("if(1);else;").unwrap());
    }

    #[test]
    fn parse_if_eof_panic() {
        let err = parse_statements("if;").unwrap_err();
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
            parse_statements("if (1) { 2, 3; 4; }").unwrap());
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
            parse_statements("while (1) { 1, 2; 3; } 4;").unwrap());
    }

    #[test]
    fn parse_empty_braces() {
        assert_eq!(
            vec![Tag::new(Block(Block {
                declarations: Vec::new(),
                statements: Vec::new()
            }), pos(1, 0))],
            parse_statements("{}").unwrap());
    }

    #[test]
    fn parse_comma_int() {
        assert_eq!(
            vec![Tag::new(Expression(Progn(vec![Tag::new(Int(1), pos(1, 0)),
                                                Tag::new(Int(2), pos(1, 3))])),
                          pos(1, 0))],
            parse_statements("1, 2;").unwrap());
    }

    #[test]
    fn parse_label_semicolon_required_panic() {
        let err = parse_statements("hi").unwrap_err();
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
            parse_statements("
if (1) {
    while (2) {
        if (3) {
            hi;
        } else {
            bye;
        }
    }
} else
    4;").unwrap());
    }

    #[test]
    fn parse_no_semicolon_after_else_panic() {
        let err = parse_statements("{
    if (1) {
        2;
    } else
        3
}").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:6:0: Expected semicolon here",
            err.description());
    }

    #[test]
    fn parse_variable_decl_panic_no_semicolon() {
        let err = parse_statements("{ int i }").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:8: Expected semicolon here",
            err.description());
    }

    #[test]
    fn parse_variable_decl_three_tokens_in_a_row_panic() {
        let err = parse("int i 2;").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:6: Expected semicolon here",
            err.description());
    }

    #[test]
    fn parse_variable_decl() {
        assert_eq!(
            vec![Tag::new(Block(Block {
                declarations: vec![Tag::new(VariableDeclaration(VariableDeclaration {
                    type_: Tag::new(Type {
                        name: TypeName::Int,
                        is_const: false,
                        is_volatile: false,
                    }, pos(1, 2)),
                    name: Tag::new("i".to_owned(), pos(1, 6)),
                    value: None,
                }), pos(1, 2))],
                statements: Vec::new(),
            }), pos(1, 0))],
            parse_statements("{ int i; }").unwrap());
    }

    #[test]
    fn parse_variable_decl_in_parens_panic() {
        let err = parse_statements("(type i)").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:6: Unexpected symbol here, expected `(`, `)`, `;`, `}`, `,`, `[`, `]`, or `=`",
            err.description());
    }

    #[test]
    fn parse_variable_decl_in_parens_panic_2() {
        let err = parse_statements("(int i)").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:5: Expected a `)` here",
            err.description());
    }

    #[test]
    fn parse_semicolon_in_paren_panic() {
        let err = parse_statements("(;)").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:1: Unexpected semicolon here, only expressions are allowed",
            err.description());
    }

    #[test]
    fn parse_index_label_label() {
        assert_eq!(
            vec![Tag::new(Expression(Index(Box::new(Tag::new(Label("a".to_owned()), pos(1, 0))), Box::new(Tag::new(Label("b".to_owned()), pos(1, 2))))), pos(1, 0))],
            parse_statements("a[b];").unwrap());
    }

    #[test]
    fn parse_close_paren_panic() {
        let err = parse_statements(")").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:0: Invalid start of a statement",
            err.description());
    }

    #[test]
    fn parse_close_square_panic() {
        let err = parse_statements("]").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:0: Invalid start of a statement",
            err.description());
    }

    #[test]
    fn parse_close_curly_panic() {
        let err = parse_statements("}").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:0: Invalid start of a statement",
            err.description());
    }

    #[test]
    fn parse_declare_set_variable() {
        assert_eq!(
            vec![Tag::new(VariableDeclaration(VariableDeclaration {
                type_: Tag::new(Type {
                    name: TypeName::Int,
                    is_const: false,
                    is_volatile: false,
                }, pos(1, 0)),
                name: Tag::new("i".to_owned(), pos(1, 4)),
                value: Some(Tag::new(Int(3), pos(1, 8))),
            }), pos(1, 0))],
            parse("int i = 3;").unwrap());
    }


    #[test]
    fn parse_set_variable() {
        assert_eq!(
            vec![Ok(Tag::new(VariableDeclaration(VariableDeclaration {
                type_: Tag::new(Type {
                    name: TypeName::Int,
                    is_const: false,
                    is_volatile: false,
                }, pos(1, 0)),
                name: Tag::new("i".to_owned(), pos(1, 4)),
                value: None,
            }), pos(1, 0))),
                 Err(Tag::new(Expression(Set(Box::new(Tag::new(Label("i".to_owned()),
                                                               pos(1, 7))),
                                             Box::new(Tag::new(Int(3),
                                                               pos(1, 11))))),
                              pos(1, 7))),
                 Err(Tag::new(If(Tag::new(Label("i".to_string()), pos(1, 18)),
                                 Box::new(Tag::new(Block(Block {
                                     declarations: vec![],
                                     statements: vec![
                                         Tag::new(
                                             Expression(Set(Box::new(Tag::new(Label("i".to_owned()),
                                                                              pos(1, 25))),
                                                            Box::new(Tag::new(Int(5),
                                                                              pos(1, 31))))),
                                             pos(1, 24)),
                                     ],
                                 }), pos(1, 21))),
                                 None),
                              pos(1, 14)))],
            parse_command_line("int i; i = 3; if (i) { ((i) = (5)); }").unwrap());
    }

    #[test]
    fn parse_top_level_statement_1_panic() {
        let err = parse("{
    if (1) {
        2;
    } else
        3;
}").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:0: Only declarations are allowed at the top level",
            err.description());
    }

    #[test]
    fn parse_top_level_statement_2_panic() {
        let err = parse("3;").unwrap_err();
        assert_eq!(
            io::ErrorKind::InvalidInput,
            err.kind());
        assert_eq!(
            "*stdin*:1:0: Only declarations are allowed at the top level",
            err.description());
    }

    #[test]
    fn parse_array() {
        assert_eq!(
            vec![Ok(Tag::new(VariableDeclaration(VariableDeclaration {
                type_: Tag::new(
                    Type {
                        name: TypeName::Array(Box::new(TypeName::Int),
                                              Some(Tag::new(Int(3),
                                                            pos(1, 6)))),
                        is_const: false,
                        is_volatile: false,
                    }, pos(1, 0)),
                name: Tag::new("i".to_owned(), pos(1, 4)),
                value: None, }),
                          pos(1, 0)))],
            parse_command_line("int i[3];").unwrap());
    }

    #[test]
    fn parse_cast_void_int() {
        assert_eq!(
            vec![Err(Tag::new(Expression(Cast(Box::new(Tag::new(Type { name: TypeName::Void,
                                                                       is_const: false,
                                                                       is_volatile: false, },
                                                                pos(1, 1))),
                                              Box::new(Tag::new(Int(0),
                                                                pos(1, 7))))),
                              pos(1, 0)))],
            parse_command_line("(void) 0;").unwrap());
    }

    #[test]
    fn parse_cast_long_int() {
        assert_eq!(
            vec![Err(Tag::new(Expression(Cast(Box::new(Tag::new(Type { name: TypeName::Long,
                                                                       is_const: false,
                                                                       is_volatile: false, },
                                                                pos(1, 1))),
                                              Box::new(Tag::new(Int(0),
                                                                pos(1, 7))))),
                              pos(1, 0)))],
            parse_command_line("(long) 0;").unwrap());
    }

    #[test]
    fn parse_cast_void_int_in_paren() {
        assert_eq!(
            vec![Err(Tag::new(Expression(Cast(Box::new(Tag::new(Type { name: TypeName::Void,
                                                                       is_const: false,
                                                                       is_volatile: false, },
                                                                pos(1, 2))),
                                              Box::new(Tag::new(Int(0),
                                                                pos(1, 8))))),
                              pos(1, 1)))],
            parse_command_line("((void) 0);").unwrap());
    }

    #[test]
    fn parse_if_void() {
        assert_eq!(
            vec![Err(Tag::new(If(Tag::new(Cast(Box::new(Tag::new(Type { name: TypeName::Void,
                                                                        is_const: false,
                                                                        is_volatile: false },
                                                                 pos(1, 5))),
                                               Box::new(Tag::new(Int(0), pos(1, 11)))),
                                          pos(1, 4)),
                                 Box::new(Tag::new(Expression(Void),
                                                   pos(1, 13))),
                                 None),
                              pos(1, 0)))],
            parse_command_line("if ((void) 0);").unwrap());
    }

    #[test]
    fn parse_defun_1() {
        assert_eq!(
            vec![Tag::new(FunctionDeclaration(FunctionDeclaration {
                return_type: Tag::new(Type { name: TypeName::Int,
                                             is_const: false,
                                             is_volatile: false },
                                      pos(1, 0)),
                name: Tag::new("f".to_owned(), pos(1, 4)),
                params: vec![],
                body: Some(Tag::new(Block { declarations: Vec::new(),
                                            statements: Vec::new() },
                                    pos(1, 8))) }),
                          pos(1, 0))],
            parse("int f() {}").unwrap());
    }

    #[test]
    fn parse_array_declaration() {
        assert_eq!(
            vec![Tag::new(VariableDeclaration(VariableDeclaration {
                type_: Tag::new(Type { name: TypeName::Array(Box::new(TypeName::Int), Some(Tag::new(Int(3), pos(1, 6)))),
                                       is_const: false,
                                       is_volatile: false }, pos(1, 0)),
                name: Tag::new("f".to_owned(), pos(1, 4)),
                value: None,
            }),
                          pos(1, 0))],
            parse("int f[3];").unwrap());
    }

    #[test]
    fn display_block() {
        let mut v = parse_command_line("{ int i = 13; int j = f(123); g(i, j, j); }").unwrap();
        assert_eq!(v.len(), 1);
        let b = v.pop().unwrap().unwrap_err().value;
        assert_eq!(
            "{ int i = 13; int j = f(123); g(i, j, j); }",
            format!("{}", b));
    }

    #[test]
    fn parse_defun_if_void() {
        assert_eq!(
            vec![Tag::new(FunctionDeclaration(FunctionDeclaration {
                return_type: Tag::new(Type { name: TypeName::Int,
                                             is_const: false,
                                             is_volatile: false },
                                      pos(1, 0)),
                name: Tag::new("function_name".to_owned(), pos(1, 4)),
                params: vec![],
                body: Some(Tag::new(Block { declarations: Vec::new(),
                                            statements: vec![
                                                Tag::new(If(Tag::new(Cast(Box::new(Tag::new(
                                                    Type { name: TypeName::Void, is_const: false, is_volatile: false }, pos(1, 31))),
                                                                          Box::new(Tag::new(Int(0), pos(1, 37)))),
                                                                     pos(1, 30)),
                                                            Box::new(Tag::new(Expression(Void), pos(1, 39))),
                                                            None),
                                                         pos(1, 26))] },
                                    pos(1, 24))) }),
                          pos(1, 0))],
            parse("int function_name(void) { if ((void) 0); }").unwrap());
    }

    #[test]
    fn parse_defun_declaration() {
        assert_eq!(
            vec![Tag::new(FunctionDeclaration(FunctionDeclaration {
                return_type: Tag::new(Type { name: TypeName::Void,
                                             is_const: false,
                                             is_volatile: false },
                                      pos(1, 0)),
                name: Tag::new("function_name".to_owned(), pos(1, 5)),
                params: vec![],
                body: None }),
                          pos(1, 0))],
            parse("void function_name(void);").unwrap());
    }
}
