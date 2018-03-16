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
            @hook("std.list.is_empty"  (T: Any -> [T..] -> Bool))
        "#,
    );
    sources.insert(
        "head",
        r#"
            @hook("std.list.head"  (T: Any -> [T..] -> T))
        "#,
    );
    sources.insert(
        "tail",
        r#"
            @hook("std.list.tail"  (T: Any -> [T..] -> [T..]))
        "#,
    );
    sources.insert(
        "ref",
        r#"
            @hook("std.ref.new"  (T: Any -> T -> (Ref <| T)))
        "#,
    );
    sources.insert(
        "get!",
        r#"
            @hook("std.ref.get!"  (T: Any -> (Ref <| T) -> T))
        "#,
    );
    sources.insert(
        "set!",
        r#"
            T: Any -(T -> (Ref <| T) -> (Ref <| T))->
            new_value: T -((Ref <| T) -> (Ref <| T))->
            reference: (Ref <| T) -(Ref <| T)->
                (reference new_value)
                    |> @hook("std.ref.set!"  (((Ref <| T)  T) -> (Ref <| T)))
        "#,
    );
    sources.insert(
        "while",
        r#"
            let while_internal: ((() -> Bool) -> (() -> Any) -> ()) <==
                condition: (() -> Bool) -((() -> Any) -> ())->
                body: (() -> Any) -()->
                    if condition <| () then
                        body <| ();
                        while_internal <| condition <| body
                    else
                        ()
            while_internal
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
