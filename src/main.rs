mod lex;
mod parse;
mod pos;
mod prefix;

use std::io;
use std::io::Write;
use std::error::Error;
use lex::*;
use parse::*;

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
        match lex("*stdin*", input.chars()).and_then(parse) {
            Ok(bst) => println!("{:?}", bst),
            Err(e) => println!("{}", e.description()),
        }
    }
}
