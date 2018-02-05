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
        match lex("*stdin*", input.chars()).and_then(parse)
            .and_then(|bst| type_check(bst.iter()).map(|()| bst)) {
            Ok(bst) => println!("{:?}", bst),
            Err(e) => println!("{}", e.description()),
        }
    }
}
