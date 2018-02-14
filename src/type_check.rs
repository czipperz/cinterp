use parse::*;
use pos::*;
use std::io;
use parse::Expression::*;
use parse::Statement::*;
use parse::Declaration::*;
use parse::Expression;
use parse::Block;
use std::collections::HashMap;

fn type_check_statement(statement: &Statement, statement_pos: &Pos,
                        global_definitions: &mut HashMap<String, Tag<Type>>,
                        local_definitions: &mut Vec<HashMap<String, Tag<Type>>>) -> io::Result<()> {
    match statement {
        &If(ref cond, ref true_, ref false_) => {
            if try!(type_check_expression(&cond.value, &cond.pos, global_definitions, local_definitions)).value.name == TypeName::Void {
                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Condition of an if statement must be a number", cond.pos)));
            }
            try!(type_check_statement(&true_.value, &true_.pos, global_definitions, local_definitions));
            match false_ {
                &Some(ref false_) => { try!(type_check_statement(&false_.value, &false_.pos, global_definitions, local_definitions)); },
                &None => (),
            }
        },
        &While(ref cond, ref body) => {
            if try!(type_check_expression(&cond.value, &cond.pos, global_definitions, local_definitions)).value.name == TypeName::Void {
                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Condition of a while statement must not have type void", cond.pos)));
            }
            try!(type_check_statement(&body.value, &body.pos, global_definitions, local_definitions));
        },
        &Block(ref block) => {
            local_definitions.push(HashMap::new());
            try!(type_check_block(block, global_definitions, local_definitions));
            local_definitions.pop();
        },
        &Expression(ref expression) => {
            try!(type_check_expression(expression, statement_pos, global_definitions, local_definitions));
        },
    }
    Ok(())
}

fn type_check_block(block: &Block,
                    global_definitions: &mut HashMap<String, Tag<Type>>,
                    local_definitions: &mut Vec<HashMap<String, Tag<Type>>>) -> io::Result<()> {
    for declaration in &block.declarations {
        try!(define_variable(&declaration.value, &declaration.pos, local_definitions.last_mut().unwrap()));
        try!(type_check_declaration(&declaration.value, &declaration.pos, global_definitions, local_definitions));
    }
    for statement in &block.statements {
        try!(type_check_statement(&statement.value, &statement.pos, global_definitions, local_definitions));
    }
    Ok(())
}

fn lookup_label(label: &String, pos: &Pos,
                global_definitions: &mut HashMap<String, Tag<Type>>,
                local_definitions: &mut Vec<HashMap<String, Tag<Type>>>) -> io::Result<Tag<Type>> {
    for def in local_definitions {
        if let Some(def) = def.get(label) {
            return Ok(def.clone());
        }
    }
    match global_definitions.get(label) {
        Some(def) => Ok(def.clone()),
        None => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                   format!("{} Undeclared variable used here", pos))),
    }
}

fn type_check_expression(expression: &Expression, expression_pos: &Pos,
                         global_definitions: &mut HashMap<String, Tag<Type>>,
                         local_definitions: &mut Vec<HashMap<String, Tag<Type>>>) -> io::Result<Tag<Type>> {
    match expression {
        &Funcall(ref name, ref arguments) => {
            let type_ = try!(type_check_expression(&name.value, &name.pos, global_definitions, local_definitions));
            if let TypeName::FunctionPointer(return_type, parameter_types) = type_.value.name {
                if arguments.len() != parameter_types.len() {
                    return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                              format!("{} Wrong number of arguments to function here", type_.pos)));
                }
                for (arg, param) in arguments.iter().zip(parameter_types.iter()) {
                    let arg_type = try!(type_check_expression(&arg.value, &arg.pos, global_definitions, local_definitions));
                    if !arg_type.value.name.can_cast_to(&param.value.name) {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Invalid cast of {} to {} here", arg.pos, arg_type.value.name, param.value.name)));
                    }
                }
                Ok(*return_type)
            } else {
                Err(io::Error::new(io::ErrorKind::InvalidInput,
                                   format!("{} Expected a function or function pointer here", type_.pos)))
            }
        },
        &Progn(ref expressions) => {
            let mut type_ = Tag::new(Type { name: TypeName::Void,
                                            is_const: true, is_volatile: false },
                                     expression_pos.clone());
            for expression in expressions {
                type_ = try!(type_check_expression(&expression.value, &expression.pos, global_definitions, local_definitions));
            }
            Ok(type_)
        },
        &Label(ref label) => {
            lookup_label(label, expression_pos, global_definitions, local_definitions)
        },
        &Index(ref name, ref value) => {
            let name_type = try!(type_check_expression(&name.value, &name.pos, global_definitions, local_definitions));
            let value_type = try!(type_check_expression(&value.value, &value.pos, global_definitions, local_definitions));
            if let TypeName::Array(array_type, _) = name_type.value.name {
                if value_type.value.name.can_cast_to(&SIZE_T) {
                    Ok(Tag::new(Type { name: *array_type,
                                       is_const: name_type.value.is_const,
                                       is_volatile: name_type.value.is_volatile },
                                name_type.pos))
                } else {
                    Err(io::Error::new(io::ErrorKind::InvalidInput,
                                       format!("{} Array must be indexed by a number", value_type.pos)))
                }
            } else if let TypeName::Array(array_type, _) = value_type.value.name {
                if name_type.value.name.can_cast_to(&SIZE_T) {
                    Ok(Tag::new(Type { name: *array_type,
                                       is_const: value_type.value.is_const,
                                       is_volatile: value_type.value.is_volatile },
                                value_type.pos))
                } else {
                    Err(io::Error::new(io::ErrorKind::InvalidInput,
                                       format!("{} Array must be indexed by a number", name_type.pos)))
                }
            } else {
                Err(io::Error::new(io::ErrorKind::InvalidInput,
                                   format!("{} Expected an array here", name.pos)))
            }
        },
        &Set(ref name, ref value) => {
            let name_type = try!(type_check_expression(&name.value, &name.pos, global_definitions, local_definitions));
            let value_type = try!(type_check_expression(&value.value, &value.pos, global_definitions, local_definitions));
            if value_type.value.name.can_cast_to(&name_type.value.name) {
                Ok(name_type)
            } else {
                Err(io::Error::new(io::ErrorKind::InvalidInput,
                                   format!("{} Invalid cast of {} to {} here", expression_pos, value_type.value.name, name_type.value.name)))
            }
        },
        &Cast(ref casted_type, ref expression) => {
            let expression_type = try!(type_check_expression(&expression.value, &expression.pos, global_definitions, local_definitions));
            if expression_type.value.name.can_cast_to(&casted_type.value.name) {
                Ok(casted_type.as_ref().clone())
            } else {
                Err(io::Error::new(io::ErrorKind::InvalidInput,
                                   format!("{} Invalid cast of {} to {} here", expression_pos, expression_type.value.name, casted_type.value.name)))
            }
        },
        &Int(_) => Ok(Tag::new(Type { name: TypeName::Int,
                                      is_const: true, is_volatile: false },
                               expression_pos.clone())),
        &Long(_) => Ok(Tag::new(Type { name: TypeName::Int,
                                       is_const: true, is_volatile: false },
                                expression_pos.clone())),
        &Void => Ok(Tag::new(Type { name: TypeName::Void,
                                    is_const: true, is_volatile: false },
                             expression_pos.clone())),
    }
}

fn type_check_declaration(declaration: &Declaration, declaration_pos: &Pos,
                          global_definitions: &mut HashMap<String, Tag<Type>>,
                          local_definitions: &mut Vec<HashMap<String, Tag<Type>>>) -> io::Result<()> {
    match declaration {
        &VariableDeclaration(ref var) => {
            match var.type_.value.name {
                TypeName::Array(_, Some(ref expression)) => {
                    let expression_type = try!(type_check_expression(&expression.value, &expression.pos, global_definitions, local_definitions));
                    if !expression_type.value.name.can_cast_to(&SIZE_T) {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Array size must be a number", expression.pos)));
                    }
                },
                _ => (),
            }
            if let Some(ref expression) = var.value {
                let expression_type = try!(type_check_expression(&expression.value, &expression.pos, global_definitions, local_definitions));
                if expression_type.value.name.can_cast_to(&var.type_.value.name) {
                    Ok(())
                } else {
                    Err(io::Error::new(io::ErrorKind::InvalidInput,
                                       format!("{} Expression here does not match type of variable", expression.pos)))
                }
            } else {
                Ok(())
            }
        },
        &FunctionDeclaration(ref fun) => {
            let mut local_scope = HashMap::new();
            for &(ref name, ref param_type) in &fun.params {
                if let &Some(ref name) = name {
                    if let Some(prev) = local_scope.insert(name.clone(), param_type.clone()) {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                  format!("{} Variable declaration here is invalid, it's already been declared here\n{}", param_type.pos, prev.pos)));
                    }
                }
            }
            local_definitions.push(local_scope);
            if let Some(ref block) = fun.body {
                try!(type_check_block(&block.value, global_definitions, local_definitions));
                local_definitions.pop();
            }
            Ok(())
        },
    }
}

fn define_variable(declaration: &Declaration, declaration_pos: &Pos, definitions: &mut HashMap<String, Tag<Type>>) -> io::Result<()> {
    let (name, type_) = match declaration {
        &VariableDeclaration(ref var) =>
            (var.name.value.clone(), var.type_.clone()),
        &FunctionDeclaration(ref fun) =>
            (fun.name.value.clone(),
             Tag::new(Type {
                 name: TypeName::FunctionPointer(Box::new(fun.return_type.clone()),
                                                 fun.params.iter().map(|&(_, ref t)| t.clone()).collect()),
                 is_const: true,
                 is_volatile: false,
             },
                      declaration_pos.clone()))
    };
    if let Some(prev) = definitions.insert(name, type_) {
        return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                  format!("{} Variable declaration here is invalid, it's already been declared here\n{}", declaration_pos, prev.pos)));
    }
    Ok(())
}

pub fn type_check<'a, I: Iterator<Item = &'a Tag<Declaration>>>(mut iter: I) -> io::Result<()> {
    let mut global_definitions = HashMap::new();
    while let Some(declaration) = iter.next() {
        try!(define_variable(&declaration.value, &declaration.pos, &mut global_definitions));
        try!(type_check_declaration(&declaration.value, &declaration.pos, &mut global_definitions, &mut Vec::new()));
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use lex::*;
    use preprocess::*;

    fn x(input: &str) -> io::Result<()> {
        type_check(parse(Preprocessor::new(Lexer::new("*stdin*", input.chars()))).unwrap().iter())
    }

    #[test]
    fn type_check_declaration_no_value() {
        x("int i;").unwrap();
    }

    #[test]
    fn type_check_declaration_value_1() {
        x("int i = 3;").unwrap();
    }

    #[test]
    fn type_check_declaration_value_2() {
        x("long l = 3;").unwrap();
    }

    // #[test]
    // fn type_check_declaration_array_value() {
    //     x("int x[31] = {0};").unwrap();
    // }

    #[test]
    fn type_check_declaration_array() {
        x("int x[31];").unwrap();
    }

    #[test]
    fn type_check_declaration_array_bad_index_panic() {
        x("a b; int x[b];").unwrap_err();
    }

    #[test]
    fn type_check_declaration_value_label() {
        x("long l = 3; int i = l;").unwrap();
    }

    #[test]
    fn type_check_two_declarations_same_name_panic() {
        x("long l = 3; long l = 5;").unwrap_err();
    }

    #[test]
    fn type_check_declaration_value_label_incompatible_types_panic() {
        x("custom_type l; int i = l;").unwrap_err();
    }

    #[test]
    fn type_check_variable_basic_usage() {
        x("int f(void) { int i; i; }").unwrap();
    }

    #[test]
    fn type_check_multiple_variables_same_scope_panic() {
        assert_eq!(
            "*stdin*:1:21: Variable declaration here is invalid, it's already been declared here
*stdin*:1:14:",
            format!("{}", x("int f(void) { int i; int i; }").unwrap_err()));
    }

    #[test]
    fn type_check_multiple_variables_different_scope_1() {
        x("int i; int f(void) { int i; i = f(); { int i; { int i; }} }").unwrap();
    }

    #[test]
    fn type_check_multiple_variables_different_scope_2() {
        x("a i; int f(void) { int i = i; }").unwrap();
    }

    #[test]
    fn type_check_function_parameter_name_collision_panic() {
        assert_eq!(
            "*stdin*:1:16: Variable declaration here is invalid, it's already been declared here
*stdin*:1:7:",
            format!("{}", x("void f(int i) { int i; }").unwrap_err()));
    }

    #[test]
    fn type_check_function_parameter_defined() {
        x("void f(int i) { f(i); }").unwrap();
    }

    #[test]
    fn type_check_function_parameter_wrong_type_when_used() {
        assert_eq!(
            "*stdin*:1:22: Expression here does not match type of variable",
            format!("{}", x("void f(a i) { int j = i; }").unwrap_err()));
    }

    #[test]
    fn type_check_use_outside_scope_panic_1() {
        assert_eq!(
            "*stdin*:1:45: Undeclared variable used here",
            format!("{}", x("int f(void) { int i; } int g(void) { int j = i; }").unwrap_err()));
    }

    #[test]
    fn type_check_use_outside_scope_panic_2() {
        assert_eq!(
            "*stdin*:1:35: Undeclared variable used here",
            format!("{}", x("int f(void) { { int i; } { int j = i; } }").unwrap_err()));
    }

    #[test]
    fn type_check_cast_void_to_int_panic() {
        assert_eq!(
            "*stdin*:1:36: Invalid cast of void to int here",
            format!("{}", x("void g(void); int f(void) { int i = (int) g(); }").unwrap_err()));
    }

    #[test]
    fn type_check_if_void_panic() {
        assert_eq!(
            "*stdin*:1:18: Condition of an if statement must be a number",
            format!("{}", x("int f(void) { if ((void) 0); }").unwrap_err()));
    }

    #[test]
    fn type_check_array_lookup_not_array_panic() {
        assert_eq!(
            "*stdin*:1:14: Expected an array here",
            format!("{}", x("int f(void) { 3[0]; }").unwrap_err()));
    }

    #[test]
    fn type_check_array_reverse_index() {
        x("int f(void) { int x[3]; 0[x]; }").unwrap();
    }

    #[test]
    fn type_check_array_lookup_wrong_type_panic() {
        assert_eq!(
            "*stdin*:1:37: Invalid cast of int to a here",
            format!("{}", x("void g(a); int f(void) { int x[3]; g(x[0]); }").unwrap_err()));
    }

    #[test]
    fn type_check_array_lookup() {
        x("void g(int); int f(void) { int x[3]; g(x[0]); }").unwrap();
    }
}
