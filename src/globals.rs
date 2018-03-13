use parse::source_to_ast;
use eval::evaluate_ast;
use typecheck::typecheck_ast;
use ast::{Type, Value};
use std::collections::HashMap;

pub fn get_globals() -> HashMap<String, (Value, Type)> {
    let mut sources = HashMap::new();

    sources.insert("inf", "1/0");
    sources.insert("nan", "0/0");
    sources.insert(
        "floordiv",
        r#"
            a: Num -(Num -> Int)->
            b: Num -Int->
                (a b) |> @hook("std.math.floordiv"  (Num Num) -> Int)
        "#,
    );
    sources.insert(
        "push",
        r#"
            T: Any -(T -> [T..] -> [T..])->
            el: T -([T..] -> [T..])->
            list: [T..] -[T..]->
                (el list) |> @hook("std.list.push"  ((T  [T..]) -> [T..]))
        "#,
    );
    sources.insert(
        "is_empty",
        r#"
            @hook("std.list.is_empty" (T: Any -> [T..] -> Bool))
        "#,
    );
    sources.insert(
        "head",
        r#"
            @hook("std.list.head" (T: Any -> [T..] -> T))
        "#,
    );
    sources.insert(
        "tail",
        r#"
            @hook("std.list.tail" (T: Any -> [T..] -> [T..]))
        "#,
    );

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
