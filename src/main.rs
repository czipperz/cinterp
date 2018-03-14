#[cfg(test)] #[macro_use] extern crate pretty_assertions;
#[macro_use] extern crate structopt;
use structopt::StructOpt;

mod preprocess;
mod lex;
mod parse;
mod pos;
mod prefix;
mod type_check;

use std::io;
use std::fs;
use std::process;
use std::error::Error;
use std::collections::HashMap;
use lex::*;
use preprocess::*;
use parse::*;
use type_check::*;
use pos::*;

fn exec_command_line<I: Iterator<Item = io::Result<char>>>(input: I, globals: &mut HashMap<String, Tag<Type>>) -> io::Result<Vec<Result<Tag<Declaration>, Tag<Statement>>>> {
    let tokens = Lexer::new("*stdin*", input);
    let tokens = Preprocessor::new(tokens);
    let syntax_tree = try!(parse_command_line(tokens));
    try!(type_check_command_line(syntax_tree.iter(), globals));
    Ok(syntax_tree)
}

fn exec<I: Iterator<Item = io::Result<char>>>(input: I) -> io::Result<Vec<Tag<Declaration>>> {
    let tokens = Lexer::new("*stdin*", input);
    let tokens = Preprocessor::new(tokens);
    let syntax_tree = try!(parse(tokens));
    try!(type_check(syntax_tree.iter()));
    Ok(syntax_tree)
}

#[derive(StructOpt, Debug)]
#[structopt(name = "cinterp")]
struct Options {
    rest: Vec<String>,
}

fn command_line() {
    let mut globals = HashMap::new();
    let mut input = String::new();
    loop {
        use std::io::Write;
        input.clear();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();
        if input == ":exit\n" {
            return;
        }
        match exec_command_line(input.chars().map(Ok), &mut globals) {
            Ok(syntax_trees) => {
                let mut first = true;
                for syntax_tree in syntax_trees {
                    if first {
                        first = false;
                    } else {
                        print!(" ");
                    }
                    match syntax_tree {
                        Ok(declaration) => print!("{}", declaration.value),
                        Err(statement) => print!("{}", statement.value),
                    }
                }
                println!("");
            },
            Err(e) => println!("{}", e.description()),
        }
    }
}

fn main() {
    let options = Options::from_args();
    if options.rest.is_empty() {
        command_line();
    } else {
        for file in options.rest {
            use std::io::Read;
            let file = io::BufReader::new(fs::File::open(file).unwrap());
            match exec(file.bytes().map(|b| b.map(|b| b as char))) {
                Ok(_) => (),
                Err(e) => {
                    eprintln!("{}", e);
                    process::exit(1);
                },
            }
        }
    }
}
