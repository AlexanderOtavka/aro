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

use std::io::prelude::*;
use std::process::exit;
use std::collections::HashMap;
use util::Error;

fn evaluate_source(input: &str, small_step: bool) -> Result<String, Error> {
    let ast = parse::source_to_ast(input)?;

    typecheck::typecheck_ast(&ast, &HashMap::new())?;

    if small_step {
        let steps = eval::get_eval_steps(&ast)?;

        let mut output = String::new();
        for step in &steps[0..(steps.len() - 1)] {
            output += &format!("--> {}\n", step)
        }

        output += &format!("--> {}", steps[steps.len() - 1]);

        Ok(output)
    } else {
        let value = eval::evaluate_expression(&ast)?;

        Ok(format!("{}", value))
    }
}

#[cfg(test)]
mod evaluate_source {
    use super::*;

    #[test]
    fn spits_out_a_result() {
        assert_eq!(evaluate_source("5", false).unwrap(), "5");
        assert_eq!(evaluate_source("-51", false).unwrap(), "-51");
        assert_eq!(evaluate_source("#false ()", false).unwrap(), "#false ()");
        assert_eq!(evaluate_source("#true ()", false).unwrap(), "#true ()");
    }

    #[test]
    fn evaluates_an_ast() {
        assert_eq!(
            evaluate_source(
                "
                -20.2 +
                (5 + 10 + 15)
                ",
                false
            ).unwrap(),
            "9.8"
        );
        assert_eq!(
            evaluate_source(
                "
                -20.2 +
                (5 + 10 + 15)
                ",
                true
            ).unwrap(),
            "--> (-20.2 + ((5 + 10) + 15))\
             \n--> (-20.2 + (15 + 15))\
             \n--> (-20.2 + 30)\
             \n--> 9.8"
        );
    }

    #[test]
    fn evaluates_an_if_expression() {
        assert_eq!(
            evaluate_source(
                "
                if 20 <= 10 then
                    2 * (0 / 0)
                else
                    -10 - -5
                ",
                false
            ).unwrap(),
            "-5"
        );
    }

    #[test]
    fn does_zero_division_with_floats() {
        assert_eq!(
            evaluate_source(
                "
                1 / 0.0 -
                    100000000000000000000000000000000000000000000000000000.0
                ",
                false
            ).unwrap(),
            "inf"
        );
    }

    #[test]
    fn reports_an_error() {
        assert!(
            evaluate_source(
                "
                20 + (5 + 10
                ",
                false
            ).is_err()
        );
    }

    #[test]
    fn complains_about_extra_tokens_in_file() {
        assert!(evaluate_source("5 7", false).is_err());
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
