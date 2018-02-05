use parse::*;
use pos::*;
use std::io;
use parse::Expression::*;
use parse::Statement::*;
use parse::Expression;
use std::collections::HashMap;

fn type_check_statement(statement: &Statement, statement_pos: &Pos,
                        global_definitions: &mut HashMap<String, Tag<Type>>,
                        local_definitions: &mut Vec<HashMap<String, Tag<Type>>>) -> io::Result<()> {
    match statement {
        &If(ref cond, ref true_, ref false_) => {
            if try!(type_check_expression(&cond.value, &cond.pos, global_definitions, local_definitions)).value.name == TypeName::Void {
                return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                          format!("{} Condition of an if statement must not have type void", cond.pos)));
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
            for declaration in &block.declarations {
                try!(type_check_declaration(&declaration.value, &declaration.pos, global_definitions, local_definitions));
            }
            for statement in &block.statements {
                try!(type_check_statement(&statement.value, &statement.pos, global_definitions, local_definitions));
            }
        },
        &Expression(ref expression) => {
            try!(type_check_expression(expression, statement_pos, global_definitions, local_definitions));
        },
    }
    Ok(())
}

fn type_check_expression(expression: &Expression, expression_pos: &Pos,
                         global_definitions: &mut HashMap<String, Tag<Type>>,
                         local_definitions: &mut Vec<HashMap<String, Tag<Type>>>) -> io::Result<Tag<Type>> {
    match expression {
        &Funcall(ref name, ref arguments) => {
            unimplemented!()
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
            for def in local_definitions {
                if let Some(def) = def.get(label) {
                    return Ok(def.clone());
                }
            }
            match global_definitions.get(label) {
                Some(def) => Ok(def.clone()),
                None => Err(io::Error::new(io::ErrorKind::InvalidInput,
                                           format!("{} Undeclared variable used here", expression_pos))),
            }
        },
        &Index(ref name, ref value) => {
            unimplemented!()
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
            if casted_type.value.name.can_cast_to(&expression_type.value.name) {
                Ok(expression_type)
            } else {
                Err(io::Error::new(io::ErrorKind::InvalidInput,
                                   format!("{} Invalid cast of {} to {} here", expression_pos, casted_type.value.name, expression_type.value.name)))
            }
        },
        &Int(i) => Ok(Tag::new(Type { name: TypeName::Int,
                                      is_const: true, is_volatile: false },
                               expression_pos.clone())),
        &Long(l) => Ok(Tag::new(Type { name: TypeName::Int,
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
    if let Some(ref expression) = declaration.value {
        let expression_type = try!(type_check_expression(&expression.value, &expression.pos, global_definitions, local_definitions));
        if expression_type.value.name.can_cast_to(&declaration.type_.value.name) {
            Ok(())
        } else {
            Err(io::Error::new(io::ErrorKind::InvalidInput,
                               format!("{} Expression here does not match type of variable", expression.pos)))
        }
    } else {
        Ok(())
    }
}

pub fn type_check<'a, I: Iterator<Item = &'a Tag<Declaration>>>(mut iter: I) -> io::Result<()> {
    let mut global_definitions = HashMap::new();
    while let Some(declaration) = iter.next() {
        if let Some(prev) = global_definitions.insert(declaration.value.name.value.clone(),
                                                      declaration.value.type_.clone()) {
            return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                      format!("{} Variable declaration here is invalid, it's already been declared here\n{}", declaration.pos, prev.pos)));
        }
        try!(type_check_declaration(&declaration.value, &declaration.pos, &mut global_definitions, &mut Vec::new()));
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use lex::*;
    use parse::*;

    fn x(input: &str) -> io::Result<()> {
        type_check(parse(lex("*stdin*", input.chars()).unwrap()).unwrap().iter())
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
    fn type_check_if_void() {
        x("if ((void) 0);").unwrap_err();
    }
}
