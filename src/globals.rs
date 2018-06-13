use ast::{CDeclaration, CName, CType, EvaluatedType, TypedAst, TypedExpression, Value};
use c_compile::{bind_global, layout_records, lift_expr, type_to_ctype};
use eval::evaluate_ast;
use parse::source_to_ast;
use std::collections::HashMap;
use typecheck::typecheck_ast;

pub fn get_globals(
    real_types: &mut Vec<EvaluatedType>,
) -> HashMap<String, (Value, TypedAst<TypedExpression>)> {
    let mut sources = HashMap::new();

    sources.insert("inf", "1/0");
    sources.insert("nan", "0/0");
    sources.insert(
        "floordiv",
        r#"
            a: Num =(Num => Int)=>
            b: Num =Int=>
                (a b) |> @hook("std.math.floordiv"  (Num Num) => Int)
        "#,
    );
    sources.insert(
        "push",
        r#"
            T: Any =([T] => T => [T])=>
            list: [T] =(T => [T])=>
            element: T =[T]=>
                (element list) |> @hook("std.list.push"  ((T [T]) => [T]))
        "#,
    );
    sources.insert(
        "is_empty",
        r#"
            @hook("std.list.is_empty"  ([Any] => Bool))
        "#,
    );
    sources.insert(
        "head",
        r#"
            @hook("std.list.head"  (T: Any => [T] => T))
        "#,
    );
    sources.insert(
        "tail",
        r#"
            @hook("std.list.tail"  (T: Any => [T] => [T]))
        "#,
    );
    sources.insert(
        "while",
        r#"
            let while_internal: ((() => Bool) => (() => Any) => ()) <-
                condition: (() => Bool) =((() => Any) => ())=>
                body: (() => Any) =()=>
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
                typecheck_ast(&ast, &HashMap::new(), real_types).unwrap(),
            ),
        );
    }

    globals
}

pub fn get_globals_c_files(
    globals: &HashMap<String, (Value, TypedAst<TypedExpression>)>,
    real_types: &Vec<EvaluatedType>,
) -> (String, String) {
    let mut declarations = Vec::new();

    let mut local_declarations = Vec::new();
    let mut local_statements = Vec::new();
    let mut expr_index = 0;
    let mut functions = Vec::new();
    let mut function_index = 0;
    let mut externs = Vec::new();

    for (name, (_, typechecked)) in globals {
        declarations.push(CDeclaration(
            CType::Ref(Box::new(CType::Ref(Box::new(type_to_ctype(
                &typechecked.expr_type,
            ))))),
            CName::Ident(name.clone()),
        ));

        let value = lift_expr(
            typechecked,
            &mut local_declarations,
            &mut local_statements,
            &mut expr_index,
            &mut functions,
            &mut function_index,
            &mut externs,
            real_types,
            &layout_records(real_types.clone()),
        );
        bind_global(
            name,
            &value,
            &typechecked.expr_type,
            &mut local_declarations,
            &mut local_statements,
            &mut expr_index,
        );
    }

    let h_file = format!(
        "// Generated by {} {}\
         \n\
         \n// Required POSIX libraries\
         \n#include <stdio.h>\
         \n#include <stdbool.h>\
         \n#include <stdlib.h>\
         \n\
         \n// Critical typedefs\
         \ntypedef union _Aro_Any {{\
         \n  bool Bool;\
         \n  int Int;\
         \n  double Float;\
         \n  union _Aro_Any* Object;\
         \n  union _Aro_Any* Closure;\
         \n  void* Ref;\
         \n  void* Void_Ptr;\
         \n}} _Aro_Any, *_Aro_Object, *_Aro_Closure;\
         \n\
         \n// Useful macros\
         \n#define ARO_DEFINE_CLOSURE_HOOK(NAME, RETURN_TYPE, ARGUMENT) \\\
         \n  _Aro_Closure _aro_hook__##NAME;                            \\\
         \n  static RETURN_TYPE fn__##NAME(ARGUMENT, _Aro_Object _captures)\
         \n\
         \n#define ARO_BIND_CLOSURE_HOOK(NAME)             \\\
         \n  _aro_hook__##NAME = malloc(sizeof(_Aro_Any)); \\\
         \n _aro_hook__##NAME->Void_Ptr = fn__##NAME;\
         \n\
         \n// Lifecycle hooks for linked libraries\
         \nvoid _aro_std_init(void);\
         \nvoid _aro_std_ext_init(void);\
         \nvoid _aro_ext_init(void);\
         \n\
         \n// Global names\
         \n{}\
         \n",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        declarations
            .iter()
            .map(|declaration| format!("extern {}", declaration))
            .collect::<Vec<String>>()
            .join("\n"),
    );

    let c_file = format!(
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
         \n{}\
         \n\
         \nvoid _aro_ext_init(void) {{}}\
         \n\
         \nvoid _aro_std_init(void) {{\
         \n  _aro_std_ext_init();\
         \n\
         \n  {}\
         \n  {}\
         \n\
         \n  _aro_ext_init();\
         \n}}\
         \n",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        externs
            .iter()
            .map(|declaration| format!("extern {}", declaration))
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
            .join("\n"),
        declarations
            .iter()
            .map(|declaration| format!("{}", declaration))
            .collect::<Vec<String>>()
            .join("\n"),
        local_declarations
            .iter()
            .map(|declaration| format!("{}", declaration))
            .collect::<Vec<String>>()
            .join(" "),
        local_statements
            .iter()
            .map(|statement| format!("{}", statement))
            .collect::<Vec<String>>()
            .join(" "),
    );

    (h_file, c_file)
}
