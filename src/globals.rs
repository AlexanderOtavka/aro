use parse::source_to_ast;
use eval::evaluate_ast;
use typecheck::typecheck_ast;
use ast::{Type, Value};
use std::collections::HashMap;

pub fn get_globals() -> HashMap<String, (Value, Type)> {
    let mut sources = HashMap::new();

    sources.insert("inf", "1/0");
    sources.insert("nan", "0/0");

    let mut globals = HashMap::new();
    for (name, source) in sources {
        let ast = source_to_ast(source).unwrap();
        globals.insert(
            String::from(name),
            (
                evaluate_ast(&ast, &HashMap::new()).unwrap(),
                typecheck_ast(&ast, &HashMap::new()).unwrap(),
            ),
        );
    }

    globals
}
