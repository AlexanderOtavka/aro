// Citations:
//  https://doc.rust-lang.org
//  https://stackoverflow.com/questions/34969902/how-to-write-a-rust-function-that-takes-an-iterator
//  https://stackoverflow.com/questions/28370126/how-can-i-test-stdin-and-stdout
//  http://lalrpop.github.io/lalrpop/

#![cfg_attr(feature = "ci", deny(warnings))]

extern crate clap;
extern crate lalrpop_util;
mod c_ast;
mod c_compile;
mod eval;
mod globals;
mod grammar;
mod parse;
mod typecheck;
mod typed_ast;
mod untyped_ast;
mod util;
mod wasm_ast;
mod wasm_compile;

use globals::{get_globals, get_globals_c_files};
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::process::exit;
use util::Error;

fn evaluate_source(input: &str, small_step: bool) -> Result<String, Error> {
    let ast = parse::source_to_ast(input)?;

    let globals = get_globals(&mut Vec::new());

    typecheck::typecheck_ast(
        &ast,
        &globals
            .iter()
            .map(|(name, &(_, ref typechecked))| (name.clone(), *typechecked.expr_type.clone()))
            .collect(),
        &mut Vec::new(),
    )?;

    if small_step {
        let steps = eval::get_eval_steps(
            &ast,
            &globals
                .iter()
                .map(|(name, &(ref value, _))| (name.clone(), value.clone()))
                .collect(),
        )?;

        let mut output = String::new();
        for step in &steps[0..(steps.len() - 1)] {
            output += &format!("--> {}\n", step)
        }

        output += &format!("--> {}", steps[steps.len() - 1]);

        Ok(output)
    } else {
        let value = eval::evaluate_ast(
            &ast,
            &globals
                .iter()
                .map(|(name, &(ref value, _))| (name.clone(), value.clone()))
                .collect(),
        )?;

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
            "-5.1"
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
        assert_eq!(evaluate_file("examples/generics.aro", false).unwrap(), "6")
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
            "(2 16 7 (&!7))"
        )
    }

    #[test]
    fn parse_error() {
        assert_eq!(
            evaluate_file("examples/parse_error.aro", false).unwrap_err(),
            "Bitch, do I look like I speak perl?\
             \n      |\
             \n    4 |     -10 $ -5\
             \n      |         ^"
        )
    }

    #[test]
    fn records() {
        assert_eq!(
            evaluate_file("examples/records.aro", false).unwrap(),
            "{is_forward <- #false speed <- 25 with_sass <- #true}"
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
        assert_eq!(evaluate_file("examples/tuple.aro", false).unwrap(), "11")
    }
}

fn c_compile_source(input: &str) -> Result<String, Error> {
    let ast = parse::source_to_ast(input)?;

    let mut real_types = Vec::new();
    let globals = get_globals(&mut real_types);

    let typechecked_ast = typecheck::typecheck_ast(
        &ast,
        &globals
            .iter()
            .map(|(name, &(_, ref typechecked))| (name.clone(), *typechecked.expr_type.clone()))
            .collect(),
        &mut real_types,
    )?;

    let record_layouts = c_compile::layout_records(real_types.clone());

    let mut declarations = Vec::new();
    let mut statements = Vec::new();
    let mut expr_index = 0;
    let mut functions = Vec::new();
    let mut function_index = 0;
    let mut externs = Vec::new();
    let expr = c_compile::lift_expr(
        &typechecked_ast,
        &mut declarations,
        &mut statements,
        &mut expr_index,
        &mut functions,
        &mut function_index,
        &mut externs,
        &real_types,
        &record_layouts,
    );

    c_compile::print_value(
        expr,
        &typechecked_ast.expr_type,
        &mut declarations,
        &mut statements,
        &mut expr_index,
        &real_types,
        &record_layouts,
    );

    Ok(format!(
        "// Generated by {} {}\
         \n\
         \n#include <arostd.h>\
         \n\
         \n{}\
         \n\
         \n{}\
         \n\
         \n{}\
         \n\
         \nint main(void) {{\
         \n  _aro_std_init();\
         \n\
         \n  {}\
         \n  {}\
         \n\
         \n  printf(\"\\n\");\
         \n  return 0;\
         \n}}",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        externs
            .iter()
            .map(|extern_declaration| format!("{}", extern_declaration))
            .collect::<Vec<String>>()
            .join("\n"),
        functions
            .iter()
            .map(|function| format!("static {};", function.expr.get_signature_string()))
            .collect::<Vec<String>>()
            .join("\n"),
        functions
            .into_iter()
            .map(|function| format!("static {}", function))
            .collect::<Vec<String>>()
            .join("\n\n"),
        declarations
            .into_iter()
            .map(|statement| format!("{}", statement))
            .collect::<Vec<String>>()
            .join(" "),
        statements
            .into_iter()
            .map(|statement| format!("{}", statement))
            .collect::<Vec<String>>()
            .join(" "),
    ))
}

fn c_compile_globals(path: &Path) -> Result<(), String> {
    let mut real_types = Vec::new();
    let (std_h_code, std_c_code) = get_globals_c_files(&get_globals(&mut real_types), &real_types);

    let mut std_h_file = File::create(path.join("arostd.h"))
        .map_err(|err| format!("Couldn't create std file: {}", err))?;
    let mut std_c_file = File::create(path.join("arostd.c"))
        .map_err(|err| format!("Couldn't create std file: {}", err))?;
    write!(std_h_file, "{}", std_h_code)
        .map_err(|err| format!("Couldn't write to std file: {}", err))?;
    write!(std_c_file, "{}", std_c_code)
        .map_err(|err| format!("Couldn't write to std file: {}", err))?;

    Ok(())
}

fn c_compile_file(file_name: &str) -> Result<String, String> {
    let mut input_file = std::fs::File::open(file_name).map_err(|_| "Couldn't open file.")?;

    let mut input_string = String::new();
    input_file
        .read_to_string(&mut input_string)
        .map_err(|_| "Couldn't read input.")?;

    c_compile_source(&input_string).map_err(|err| err.as_string(&input_string))
}

fn wasm_compile_source(input: &str) -> Result<String, Error> {
    let ast = parse::source_to_ast(input)?;

    let mut real_types = Vec::new();
    let globals = get_globals(&mut real_types);

    let typechecked_ast = typecheck::typecheck_ast(
        &ast,
        &globals
            .iter()
            .map(|(name, &(_, ref typechecked))| (name.clone(), *typechecked.expr_type.clone()))
            .collect(),
        &mut real_types,
    )?;

    let record_layouts = c_compile::layout_records(real_types.clone());

    let mut c_declarations = Vec::new();
    let mut c_statements = Vec::new();
    let mut c_expr_index = 0;
    let mut c_functions = Vec::new();
    let mut c_function_index = 0;
    let mut c_externs = Vec::new();
    let value = c_compile::lift_expr(
        &typechecked_ast,
        &mut c_declarations,
        &mut c_statements,
        &mut c_expr_index,
        &mut c_functions,
        &mut c_function_index,
        &mut c_externs,
        &real_types,
        &record_layouts,
    );

    Ok(format!(
        "{}",
        wasm_ast::WAsmModule::from_c(
            &c_externs,
            0,
            &c_functions,
            &c_declarations,
            &c_statements,
            Some(&value),
        )
    ))
}

fn wasm_compile_file(file_name: &str) -> Result<String, String> {
    let mut input_file = std::fs::File::open(file_name).map_err(|_| "Couldn't open file.")?;

    let mut input_string = String::new();
    input_file
        .read_to_string(&mut input_string)
        .map_err(|_| "Couldn't read input.")?;

    wasm_compile_source(&input_string).map_err(|err| err.as_string(&input_string))
}

#[cfg(test)]
mod c_compile_file {
    use super::*;

    #[test]
    fn add() {
        assert_eq!(
            c_compile_file("examples/add.aro").unwrap(),
            "// Generated by aro 0.1.0\
             \n\
             \n#include <arostd.h>\
             \n\
             \n\
             \n\
             \n\
             \n\
             \n\
             \n\
             \nint main(void) {\
             \n  _aro_std_init();\
             \n\
             \n  double _aro_expr_op_result_0;\
             \n  _aro_expr_op_result_0 = (1 + 1); printf(\"%lf\", _aro_expr_op_result_0);\
             \n\
             \n  printf(\"\\n\");\
             \n  return 0;\
             \n}",
        )
    }
}

fn main() {
    let options = clap::App::new("The -aro-> Compiler")
        .version(env!("CARGO_PKG_VERSION"))
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
            clap::Arg::with_name("target-wat")
                .help("Compile to WebAssembly Text Format")
                .short("t")
                .long("target-wat"),
        )
        .arg(
            clap::Arg::with_name("target-c")
                .help("Compile to C")
                .short("c")
                .long("target-c"),
        )
        .arg(
            clap::Arg::with_name("build-std")
                .help("When targeting C, build standard library in given folder")
                .takes_value(true)
                .value_name("path")
                .long("build-std"),
        )
        .get_matches();

    let infile = options.value_of("infile").unwrap();
    let result = if options.is_present("target-wat") {
        wasm_compile_file(infile)
    } else if options.is_present("target-c") {
        let file_result = c_compile_file(infile);

        if options.is_present("build-std") {
            match c_compile_globals(Path::new(options.value_of("build-std").unwrap())) {
                Ok(_) => file_result,
                Err(error) => Err(error),
            }
        } else {
            file_result
        }
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
