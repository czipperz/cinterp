#[cfg(test)] #[macro_use] extern crate pretty_assertions;

mod preprocess;
mod lex;
mod parse;
mod pos;
mod prefix;
mod type_check;

use std::io;
use std::io::Write;
use std::error::Error;
use lex::*;
use parse::*;
use type_check::*;
use pos::*;

fn exec(input: &String) -> io::Result<Vec<Tag<Declaration>>> {
    let tokens = Lexer::new("*stdin*", input.chars());
    let syntax_tree = try!(parse(tokens));
    try!(type_check(syntax_tree.iter()));
    Ok(syntax_tree)
}

fn main() {
    let mut input = String::new();
    loop {
        input.clear();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();
        if input == ":exit\n" {
            return;
        }
        match exec(&input) {
            Ok(syntax_trees) => {
                let mut first = true;
                for syntax_tree in syntax_trees {
                    if first {
                        first = false;
                    } else {
                        print!(" ");
                    }
                    print!("{}", syntax_tree.value);
                }
                println!("");
            },
            Err(e) => println!("{}", e.description()),
        }
    }
}
