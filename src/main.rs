// Citations:
//  https://doc.rust-lang.org
//  https://stackoverflow.com/questions/34969902/how-to-write-a-rust-function-that-takes-an-iterator
//  https://stackoverflow.com/questions/28370126/how-can-i-test-stdin-and-stdout

#![cfg_attr(feature = "ci", deny(warnings))]

extern crate clap;
mod lex;
mod parse;
mod eval;

use std::io::prelude::*;
use std::process::exit;
use eval::Value;

fn evaluate_source(input: &str) -> Result<String, String> {
    let tokens = lex::source_to_tokens(&input)?;

    let (ast, unprocessed) = parse::tokens_to_ast(tokens.as_slice()).map_err(String::from)?;

    if unprocessed.len() > 0 {
        return Err(String::from(
            "One expression per file!  Get that shit out'a here.",
        ));
    }

    Ok(match eval::evaluate_expression(ast)? {
        Value::Int(value) => format!("{}", value),
        Value::Float(value) => format!("{}", value),
        Value::Bool(true) => String::from("true"),
        Value::Bool(false) => String::from("false"),
    })
}

#[cfg(test)]
mod test_evaluate_source {
    use super::*;

    #[test]
    fn it_spits_out_a_result() {
        assert_eq!(evaluate_source("5").unwrap(), "5");
        assert_eq!(evaluate_source("-51").unwrap(), "-51");
        assert_eq!(evaluate_source("false").unwrap(), "false");
        assert_eq!(evaluate_source("true").unwrap(), "true");
    }

    #[test]
    fn it_evaluates_an_ast() {
        assert_eq!(
            evaluate_source(
                "
                (+ -20.2
                   (+ (+ 5 10)
                      15))
                "
            ).unwrap(),
            "9.8"
        );
    }

    #[test]
    fn it_evaluates_an_if_expression() {
        assert_eq!(
            evaluate_source(
                "
                (if (<= 20 10)
                    (* 2 (/ 0 0))
                    (- -10 -5))
                "
            ).unwrap(),
            "-5"
        );
    }

    #[test]
    fn it_does_zero_division_with_floats() {
        assert_eq!(
            evaluate_source(
                "
                (- (/ 1 0.0)
                   100000000000000000000000000000000000000000000000000000.0)
                "
            ).unwrap(),
            "inf"
        );
    }

    #[test]
    fn it_reports_an_error() {
        assert_eq!(
            evaluate_source(
                "
                (+ 20
                   (+ (+ 5 10)
                "
            ).unwrap_err(),
            "You ass goblin!  You can't end the file there."
        );
    }

    #[test]
    fn it_complains_about_extra_tokens_in_file() {
        assert_eq!(
            evaluate_source("5 7").unwrap_err(),
            "One expression per file!  Get that shit out'a here.",
        );
    }
}

fn evaluate_file(file_name: &str) -> Result<String, String> {
    let mut input_file = std::fs::File::open(file_name).map_err(|_| "Couldn't open file.")?;

    let mut input_string = String::new();
    input_file
        .read_to_string(&mut input_string)
        .map_err(|_| "Couldn't read input.")?;

    evaluate_source(&input_string)
}

fn main() {
    let options = clap::App::new("The aro-> Compiler")
        .version("0.2.0")
        .author("Zander Otavka <otavkaal@grinnell.edu>")
        .arg(
            clap::Arg::with_name("infile")
                .help("The input file of aro code.")
                .required(true),
        )
        .get_matches();

    exit(match evaluate_file(options.value_of("infile").unwrap()) {
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
