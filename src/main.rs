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
mod c_compile;
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
             \n    1 | [] |> (head <| type Any)\
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
    fn mutability() {
        assert_eq!(
            evaluate_file("examples/mutability.aro", false).unwrap(),
            "(2 16 7 (ref <| 7))"
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
    fn records() {
        assert_eq!(
            evaluate_file("examples/records.aro", false).unwrap(),
            "{is_forward <- #false () speed <- 25 with_sass <- #true ()}"
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

fn c_compile_source(input: &str) -> Result<String, Error> {
    let ast = parse::source_to_ast(input)?;

    let globals = get_globals();

    typecheck::typecheck_ast(
        &ast,
        &globals
            .iter()
            .map(|(name, &(_, ref value_type))| (name.clone(), value_type.clone()))
            .collect(),
    )?;

    let mut scope = Vec::new();
    let mut expr_index = 0;
    let mut functions = Vec::new();
    let mut function_index = 0;
    let expr = c_compile::lift_expr(
        &ast,
        &mut scope,
        &mut expr_index,
        &mut functions,
        &mut function_index,
    );

    Ok(format!(
        "#include <stdio.h>\
         \n#include <stdbool.h>\
         \n#include <stdlib.h>\
         \n\
         \ntypedef union _Aro_Any {{\
         \n  bool Bool;\
         \n  int Int;\
         \n  double Float;\
         \n  void* Void_Ptr;\
         \n  union _Aro_Any* Any_Ptr;\
         \n}} _Aro_Any;\
         \n\
         \n{}\
         \n\
         \n{}\
         \n\
         \nint main(void) {{\
         \n  {}\
         \n\
         \n  printf(\"%f\\n\", (double) {});\
         \n  return 0;\
         \n}}",
        functions
            .iter()
            .map(|function| format!("{};", function.expr.get_signature_string()))
            .collect::<Vec<String>>()
            .join("\n"),
        functions
            .into_iter()
            .map(|function| format!("{}", function))
            .collect::<Vec<String>>()
            .join("\n\n"),
        scope
            .into_iter()
            .map(|statement| format!("{}", statement))
            .collect::<Vec<String>>()
            .join(" "),
        expr
    ))
}

fn c_compile_file(file_name: &str) -> Result<String, String> {
    let mut input_file = std::fs::File::open(file_name).map_err(|_| "Couldn't open file.")?;

    let mut input_string = String::new();
    input_file
        .read_to_string(&mut input_string)
        .map_err(|_| "Couldn't read input.")?;

    c_compile_source(&input_string).map_err(|err| err.as_string(&input_string))
}

#[cfg(test)]
mod c_compile_file {
    use super::*;

    #[test]
    fn add() {
        assert_eq!(
            c_compile_file("examples/add.aro").unwrap(),
            "#include <stdio.h>\
             \n#include <stdbool.h>\
             \n#include <stdlib.h>\
             \n\
             \ntypedef union _Aro_Any {\
             \n  bool Bool;\
             \n  int Int;\
             \n  double Float;\
             \n  void* Void_Ptr;\
             \n  union _Aro_Any* Any_Ptr;\
             \n} _Aro_Any;\
             \n\
             \n\
             \n\
             \n\
             \n\
             \nint main(void) {\
             \n  double _aro_expr_0; _aro_expr_0 = (1 + 1);\
             \n\
             \n  printf(\"%f\\n\", (double) _aro_expr_0);\
             \n  return 0;\
             \n}",
        )
    }
}

fn main() {
    let options = clap::App::new("The -aro-> Compiler")
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
        .arg(
            clap::Arg::with_name("target-c")
                .help("Compile to C")
                .short("c")
                .long("target-c"),
        )
        .get_matches();

    let infile = options.value_of("infile").unwrap();
    let result = if options.is_present("target-c") {
        c_compile_file(infile)
    } else {
        evaluate_file(infile, options.is_present("small-step"))
    };

    exit(match result {
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
