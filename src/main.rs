// Citations:
//  https://doc.rust-lang.org
//  https://stackoverflow.com/questions/34969902/how-to-write-a-rust-function-that-takes-an-iterator
//  https://stackoverflow.com/questions/28370126/how-can-i-test-stdin-and-stdout
//  http://lalrpop.github.io/lalrpop/

#![cfg_attr(feature = "ci", deny(warnings))]

extern crate clap;
extern crate lalrpop_util;
mod ast;
mod grammar;
mod util;
mod parse;
mod typecheck;
mod eval;
mod globals;

use std::io::prelude::*;
use std::process::exit;
use util::Error;
use globals::get_globals;

fn evaluate_source(input: &str, small_step: bool) -> Result<String, Error> {
    let ast = parse::source_to_ast(input)?;

    let globals = get_globals();

    typecheck::typecheck_ast(
        &ast,
        &globals
            .iter()
            .map(|(name, &(_, ref value_type))| (name.clone(), value_type.clone()))
            .collect(),
    )?;

    if small_step {
        let steps = eval::get_eval_steps(&ast, &globals)?;

        let mut output = String::new();
        for step in &steps[0..(steps.len() - 1)] {
            output += &format!("--> {}\n", step)
        }

        output += &format!("--> {}", steps[steps.len() - 1]);

        Ok(output)
    } else {
        let value = eval::evaluate_ast(&ast, &globals)?;

        Ok(format!("{}", value))
    }
}

fn evaluate_file(file_name: &str, small_step: bool) -> Result<String, String> {
    let mut input_file = std::fs::File::open(file_name).map_err(|_| "Couldn't open file.")?;

    let mut input_string = String::new();
    input_file
        .read_to_string(&mut input_string)
        .map_err(|_| "Couldn't read input.")?;

    evaluate_source(&input_string, small_step).map_err(|err| err.as_string(&input_string))
}

#[cfg(test)]
mod evaluate_file {
    use super::*;

    #[test]
    fn basic_expressions() {
        assert_eq!(
            evaluate_file("examples/basic_expressions.aro", false).unwrap(),
            "-5"
        )
    }

    #[test]
    fn eval_error() {
        assert_eq!(
            evaluate_file("examples/eval_error.aro", false).unwrap_err(),
            "As usual, you can\'t get head.\
             \nEspecially not from an empty list.\
             \n      |\
             \n    4 | [] |> (head <| type Any)\
             \n      | ^^"
        )
    }

    #[test]
    fn function_calls() {
        assert_eq!(
            evaluate_file("examples/function_calls.aro", false).unwrap(),
            "2.5"
        )
    }

    #[test]
    fn generics_expressions() {
        assert_eq!(
            evaluate_file("examples/generics.aro", false).unwrap(),
            "[5 5 5]"
        )
    }

    #[test]
    fn let_expressions() {
        assert_eq!(evaluate_file("examples/let.aro", false).unwrap(), "2.5")
    }

    #[test]
    fn list() {
        assert_eq!(
            evaluate_file("examples/list.aro", false).unwrap(),
            "[1 2 3 4]"
        )
    }

    #[test]
    fn parse_error() {
        assert_eq!(
            evaluate_file("examples/parse_error.aro", false).unwrap_err(),
            "Bitch, do I look like I speak perl?\
             \n      |\
             \n    4 |     -10 & -5\
             \n      |         ^"
        )
    }

    #[test]
    fn recursion() {
        assert_eq!(
            evaluate_file("examples/recursion.aro", false).unwrap(),
            "120"
        )
    }

    #[test]
    fn right_pipe() {
        assert_eq!(
            evaluate_file("examples/right_pipe.aro", false).unwrap(),
            "7"
        )
    }

    #[test]
    fn tuple() {
        assert_eq!(evaluate_file("examples/tuple.aro", false).unwrap(), "12.29")
    }
}

fn main() {
    let options = clap::App::new("The aro-> Compiler")
        .version("0.4.0")
        .author("Zander Otavka <otavkaal@grinnell.edu>")
        .arg(
            clap::Arg::with_name("infile")
                .help("The input file of aro code.")
                .required(true),
        )
        .arg(
            clap::Arg::with_name("small-step")
                .help("Prints each step of execution")
                .short("s")
                .long("small-step"),
        )
        .get_matches();

    exit(match evaluate_file(
        options.value_of("infile").unwrap(),
        options.is_present("small-step"),
    ) {
        Ok(output) => {
            println!("{}", output);
            0
        }
        Err(message) => {
            eprintln!("{}", message);
            1
        }
    })
}
