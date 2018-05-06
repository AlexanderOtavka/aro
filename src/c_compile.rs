use ast::{Ast, CExpr, CFunc, CStatement, CType, CValue, Expression, Pattern, Type, Value};
use std::collections::{HashMap, HashSet};

fn get_expr_name(expr_index: &mut i32) -> String {
    let old_i = *expr_index;
    *expr_index += 1;
    format!("_aro_expr_{}", old_i)
}

fn get_func_name(func_index: &mut i32) -> String {
    let old_i = *func_index;
    *func_index += 1;
    format!("_aro_func_{}", old_i)
}

fn type_to_ctype(t: &Type) -> CType {
    match t {
        &Type::Num => CType::Float,
        &Type::Int => CType::Int,
        &Type::Bool => CType::Bool,
        &Type::Func(ref param, ref ret) => CType::Closure {
            param: param.replace_expr(type_to_ctype(&param.expr)),
            ret: ret.replace_expr(type_to_ctype(&ret.expr)),
        },
        &Type::Tuple(_) => CType::Object,
        _ => panic!("Unhandled type: {}", t),
    }
}

fn pattern_to_ctype(pattern: &Pattern) -> CType {
    match pattern {
        &Pattern::Ident(_, ref ident_type) => type_to_ctype(&ident_type.expr),
        &Pattern::Tuple(_) => CType::Object,
    }
}

fn get_expr_ctype(ast: &Ast<Expression>) -> CType {
    match &*ast.expr_type.borrow() {
        &Some(ref expr_type) => type_to_ctype(expr_type),
        &None => panic!("Ast must be typechecked before compilation"),
    }
}

fn get_ident_name(name: &str) -> String {
    format!("aro_{}", name)
}

fn find_names_in_pattern(pattern: &Ast<Pattern>, names: &mut HashSet<String>) {
    match &*pattern.expr {
        &Pattern::Ident(ref name, _) => {
            names.insert(name.clone());
        }
        &Pattern::Tuple(ref vec) => for element in vec {
            find_names_in_pattern(element, names);
        },
    }
}

fn find_captures(
    ast: &Ast<Expression>,
    locals: &HashSet<String>,
    captures: &mut HashMap<String, CType>,
) {
    match &*ast.expr {
        &Expression::BinOp(_, ref left, ref right) => {
            find_captures(left, locals, captures);
            find_captures(right, locals, captures);
        }
        &Expression::GenericCall(ref expr, _) => find_captures(expr, locals, captures),
        &Expression::Ident(ref name) => {
            if !locals.contains(name) {
                captures.insert(name.clone(), get_expr_ctype(ast));
            }
        }
        &Expression::If(ref condition, ref consequent, ref alternate) => {
            find_captures(condition, locals, captures);
            find_captures(consequent, locals, captures);
            find_captures(alternate, locals, captures);
        }
        &Expression::Let(ref pattern, ref value, ref body) => {
            let mut locals = locals.clone();
            find_names_in_pattern(pattern, &mut locals);
            find_captures(value, &locals, captures);
            find_captures(body, &locals, captures);
        }
        &Expression::RecordAccess(ref record, _) => {
            find_captures(record, locals, captures);
        }
        &Expression::Sequence(ref side_effect, ref result) => {
            find_captures(side_effect, locals, captures);
            find_captures(result, locals, captures);
        }
        &Expression::TypeLet(_, _, ref body) => {
            find_captures(body, locals, captures);
        }
        &Expression::Value(ref value) => match value {
            &Value::Bool(_) | &Value::Hook(_, _) | &Value::Int(_) | &Value::Num(_) => {}
            &Value::Func(ref pattern, _, ref body) => {
                let mut locals = locals.clone();
                find_names_in_pattern(pattern, &mut locals);
                find_captures(body, &locals, captures);
            }
            &Value::GenericFunc(_, _, _, ref body) => {
                find_captures(body, locals, captures);
            }
            &Value::List(ref vec) => for element in vec {
                find_captures(element, locals, captures);
            },
            &Value::Tuple(ref vec) => for element in vec {
                find_captures(element, locals, captures);
            },
            &Value::Record(ref map) => for (_, element) in map {
                find_captures(element, locals, captures);
            },
            &Value::Ref(ref value) => {
                find_captures(
                    &ast.replace_expr(Expression::Value((&*value.borrow()).clone())),
                    locals,
                    captures,
                );
            }
        },
    }
}

fn make_declarations(pattern: &Ast<Pattern>, scope: &mut Vec<Ast<CStatement>>) {
    match &*pattern.expr {
        &Pattern::Ident(ref name, ref value_type) => {
            let value_name = get_ident_name(name);
            let value_ctype = type_to_ctype(&value_type.expr);
            let value_ref_type = CType::Ref(Box::new(value_ctype.clone()));

            scope.push(
                pattern.replace_expr(CStatement::VarDecl(value_ref_type, value_name.clone())),
            );
            scope.push(pattern.replace_expr(CStatement::RefAlloc(value_name.clone(), value_ctype)));
        }
        &Pattern::Tuple(ref vec) => for element in vec {
            make_declarations(element, scope);
        },
    }
}

fn bind_declarations(pattern: &Ast<Pattern>, value: Ast<CExpr>, scope: &mut Vec<Ast<CStatement>>) {
    match &*pattern.expr {
        &Pattern::Ident(ref name, _) => {
            let value_name = get_ident_name(name);
            scope.push(pattern.replace_expr(CStatement::RefAssign(value_name, value)));
        }
        &Pattern::Tuple(ref vec) => for (index, element) in vec.iter().enumerate() {
            let inner_value = value.replace_expr(CExpr::ObjectAccess {
                object: value.clone(),
                index,
                field_type: element.replace_expr(pattern_to_ctype(&element.expr)),
            });
            bind_declarations(element, inner_value, scope);
        },
    }
}

pub fn lift_expr(
    ast: &Ast<Expression>,
    scope: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut i32,
) -> Ast<CValue> {
    match &*ast.expr {
        &Expression::Value(ref v) => match v {
            &Value::Num(value) => ast.replace_expr(CValue::Float(value)),
            &Value::Bool(value) => ast.replace_expr(CValue::Bool(value)),
            &Value::Int(value) => ast.replace_expr(CValue::Int(value)),
            &Value::Func(ref pattern, _, ref body) => {
                let mut captures_map = HashMap::new();
                let mut locals = HashSet::new();
                find_names_in_pattern(pattern, &mut locals);
                find_captures(body, &locals, &mut captures_map);

                let func_name = get_func_name(function_index);
                let mut func_scope = Vec::new();
                let mut func_expr_index = 0;

                let param_ctype = pattern_to_ctype(&pattern.expr);
                let param_value = pattern.replace_expr(CExpr::Value(CValue::Ident(
                    String::from("_aro_arg"),
                    param_ctype.clone(),
                )));
                make_declarations(pattern, &mut func_scope);
                bind_declarations(pattern, param_value, &mut func_scope);

                for (index, (name, var_type)) in captures_map.iter().enumerate() {
                    let ident_name = get_ident_name(name);
                    func_scope.push(ast.replace_expr(CStatement::VarDecl(
                        CType::Ref(Box::new(var_type.clone())),
                        ident_name.clone(),
                    )));

                    let captures_object = ast.replace_expr(CExpr::Value(CValue::Ident(
                        String::from("_aro_captures"),
                        CType::Object,
                    )));
                    func_scope.push(ast.replace_expr(CStatement::VarAssign(
                        ident_name,
                        ast.replace_expr(CExpr::ObjectAccess {
                            object: captures_object,
                            index,
                            field_type: ast.replace_expr(CType::Ref(Box::new(var_type.clone()))),
                        }),
                    )))
                }

                let ret = lift_expr(
                    body,
                    &mut func_scope,
                    &mut func_expr_index,
                    functions,
                    function_index,
                );

                let closure_name = get_expr_name(expr_index);
                let closure_type = get_expr_ctype(ast);
                scope.push(ast.replace_expr(CStatement::VarDecl(
                    closure_type.clone(),
                    closure_name.clone(),
                )));
                scope.push(
                    ast.replace_expr(CStatement::ClosureInit {
                        name: closure_name.clone(),
                        function: ast.replace_expr(CValue::Ident(
                            func_name.clone(),
                            CType::VoidPtr,
                        )),
                        captures: captures_map
                            .into_iter()
                            .map(|(name, var_type)| {
                                ast.replace_expr(CValue::Ident(
                                    get_ident_name(&name),
                                    CType::Ref(Box::new(var_type.clone())),
                                ))
                            })
                            .collect(),
                    }),
                );

                functions.push(ast.replace_expr(CFunc {
                    name: func_name,
                    param: pattern.replace_expr(param_ctype),
                    body: func_scope,
                    ret,
                }));

                ast.replace_expr(CValue::Ident(closure_name, closure_type))
            }
            &Value::Tuple(ref vec) => {
                let mut data = Vec::new();
                for element in vec {
                    data.push(lift_expr(
                        element,
                        scope,
                        expr_index,
                        functions,
                        function_index,
                    ));
                }

                let expr_name = get_expr_name(expr_index);
                scope.push(ast.replace_expr(CStatement::VarDecl(CType::Object, expr_name.clone())));
                scope.push(ast.replace_expr(CStatement::ObjectInit {
                    name: expr_name.clone(),
                    data,
                }));

                ast.replace_expr(CValue::Ident(expr_name, CType::Object))
            }
            _ => panic!(),
        },
        &Expression::Ident(ref name) => {
            ast.replace_expr(CValue::Deref(get_ident_name(name), get_expr_ctype(ast)))
        }
        &Expression::BinOp(ref op, ref left, ref right) => {
            let left_c_ast = lift_expr(left, scope, expr_index, functions, function_index);
            let right_c_ast = lift_expr(right, scope, expr_index, functions, function_index);

            let expr_name = get_expr_name(expr_index);
            let expr_type = get_expr_ctype(ast);
            scope.push(ast.replace_expr(CStatement::VarDecl(expr_type.clone(), expr_name.clone())));
            scope.push(ast.replace_expr(CStatement::VarAssign(
                expr_name.clone(),
                ast.replace_expr(CExpr::BinOp(op.clone(), left_c_ast, right_c_ast)),
            )));

            ast.replace_expr(CValue::Ident(expr_name, expr_type))
        }
        &Expression::If(ref condition, ref consequent, ref alternate) => {
            let condition_c_ast =
                lift_expr(condition, scope, expr_index, functions, function_index);

            let expr_name = get_expr_name(expr_index);
            let expr_type = get_expr_ctype(ast);

            let mut consequent_scope = Vec::new();
            let consequent_c_ast = lift_expr(
                consequent,
                &mut consequent_scope,
                expr_index,
                functions,
                function_index,
            );
            consequent_scope.push(ast.replace_expr(CStatement::VarAssign(
                expr_name.clone(),
                consequent_c_ast.to_c_expr(),
            )));
            let consequent_block = consequent.replace_expr(CStatement::Block(consequent_scope));

            let mut alternate_scope = Vec::new();
            let alternate_c_ast = lift_expr(
                alternate,
                &mut alternate_scope,
                expr_index,
                functions,
                function_index,
            );
            alternate_scope.push(ast.replace_expr(CStatement::VarAssign(
                expr_name.clone(),
                alternate_c_ast.to_c_expr(),
            )));
            let alternate_block = alternate.replace_expr(CStatement::Block(alternate_scope));

            scope.push(ast.replace_expr(CStatement::VarDecl(expr_type.clone(), expr_name.clone())));
            scope.push(ast.replace_expr(CStatement::If(
                condition_c_ast.to_c_expr(),
                consequent_block,
                alternate_block,
            )));

            ast.replace_expr(CValue::Ident(expr_name, expr_type))
        }
        &Expression::Let(ref pattern, ref value, ref body) => {
            let body_name = get_expr_name(expr_index);
            let body_type = get_expr_ctype(body);
            scope
                .push(body.replace_expr(CStatement::VarDecl(body_type.clone(), body_name.clone())));

            let mut body_scope = Vec::new();
            make_declarations(pattern, &mut body_scope);
            let value_c_ast = lift_expr(
                value,
                &mut body_scope,
                expr_index,
                functions,
                function_index,
            );
            bind_declarations(pattern, value_c_ast.to_c_expr(), &mut body_scope);

            let body_c_ast =
                lift_expr(body, &mut body_scope, expr_index, functions, function_index);
            body_scope.push(body.replace_expr(CStatement::VarAssign(
                body_name.clone(),
                body_c_ast.to_c_expr(),
            )));

            scope.push(ast.replace_expr(CStatement::Block(body_scope)));

            ast.replace_expr(CValue::Ident(body_name, body_type))
        }
        _ => panic!(),
    }
}

#[cfg(test)]
mod lift_expr {
    use super::*;
    use parse::source_to_ast;
    use typecheck::typecheck_ast;
    use std::collections::HashMap;

    fn assert_lift(
        aro_code: &str,
        expected_scope: &str,
        expected_functions: &str,
        expected_expr: &str,
    ) {
        let ast = source_to_ast(aro_code).unwrap();
        typecheck_ast(&ast, &HashMap::new()).unwrap();

        let mut scope = Vec::new();
        let mut expr_index = 0;
        let mut functions = Vec::new();
        let mut function_index = 0;
        let actual_expr = lift_expr(
            &ast,
            &mut scope,
            &mut expr_index,
            &mut functions,
            &mut function_index,
        );

        let mut actual_scope = String::new();
        for statement in scope {
            actual_scope += &format!("{} ", statement);
        }

        let mut actual_functions = String::new();
        for function in functions {
            actual_functions += &format!("{} ", function);
        }

        assert_eq!(actual_scope, expected_scope);
        assert_eq!(actual_functions, expected_functions);
        assert_eq!(format!("{}", actual_expr), expected_expr);
    }

    #[test]
    fn handles_arithmatic() {
        assert_lift(
            "1 + 1",
            "int _aro_expr_0; \
             _aro_expr_0 = (1 + 1); ",
            "",
            "_aro_expr_0",
        );
        assert_lift(
            "1.0 + 1.0",
            "double _aro_expr_0; \
             _aro_expr_0 = (1 + 1); ",
            "",
            "_aro_expr_0",
        );
        assert_lift(
            "1.1 - 1",
            "double _aro_expr_0; \
             _aro_expr_0 = (1.1 - 1); ",
            "",
            "_aro_expr_0",
        );
        assert_lift(
            "1 * 1.1",
            "double _aro_expr_0; \
             _aro_expr_0 = (1 * 1.1); ",
            "",
            "_aro_expr_0",
        );
        assert_lift(
            "1.3 / 1.1",
            "double _aro_expr_0; \
             _aro_expr_0 = ((double ) 1.3 / 1.1); ",
            "",
            "_aro_expr_0",
        );
        assert_lift(
            "1 / 1",
            "double _aro_expr_0; \
             _aro_expr_0 = ((double ) 1 / 1); ",
            "",
            "_aro_expr_0",
        );
    }

    #[test]
    fn handles_comparison() {
        assert_lift(
            "1.0 <= 1.0",
            "bool _aro_expr_0; \
             _aro_expr_0 = (1 <= 1); ",
            "",
            "_aro_expr_0",
        );
    }

    #[test]
    fn supports_nested_operations() {
        assert_lift(
            "1 - 1 <= 1.3 + 8 * 3",
            "int _aro_expr_0; \
             _aro_expr_0 = (1 - 1); \
             int _aro_expr_1; \
             _aro_expr_1 = (8 * 3); \
             double _aro_expr_2; \
             _aro_expr_2 = (1.3 + _aro_expr_1); \
             bool _aro_expr_3; \
             _aro_expr_3 = (_aro_expr_0 <= _aro_expr_2); ",
            "",
            "_aro_expr_3",
        );
    }

    #[test]
    fn handles_if_expressions() {
        assert_lift(
            "
            if 1 <= 2 then
                5.3
            else
                8 * 2
            ",
            "bool _aro_expr_0; \
             _aro_expr_0 = (1 <= 2); \
             double _aro_expr_1; \
             if (_aro_expr_0) { \
             _aro_expr_1 = 5.3; \
             } else { \
             int _aro_expr_2; \
             _aro_expr_2 = (8 * 2); \
             _aro_expr_1 = _aro_expr_2; \
             } ",
            "",
            "_aro_expr_1",
        );
    }

    #[test]
    fn handles_let_expressions() {
        assert_lift(
            "
            let x: Num <- 5 + 2
            x + 3
            ",
            "double _aro_expr_0; \
             { \
             double * aro_x; \
             aro_x = malloc(sizeof(double )); \
             int _aro_expr_1; \
             _aro_expr_1 = (5 + 2); \
             *aro_x = _aro_expr_1; \
             double _aro_expr_2; \
             _aro_expr_2 = ((*aro_x) + 3); \
             _aro_expr_0 = _aro_expr_2; \
             } ",
            "",
            "_aro_expr_0",
        );
    }

    #[test]
    fn handles_let_shadowing() {
        assert_lift(
            "
            let x: Num <- 5
            let x: Int <- 4
            x + 3
            ",
            "int _aro_expr_0; \
             { \
             double * aro_x; \
             aro_x = malloc(sizeof(double )); \
             *aro_x = 5; \
             int _aro_expr_1; \
             { \
             int * aro_x; \
             aro_x = malloc(sizeof(int )); \
             *aro_x = 4; \
             int _aro_expr_2; \
             _aro_expr_2 = ((*aro_x) + 3); \
             _aro_expr_1 = _aro_expr_2; \
             } \
             _aro_expr_0 = _aro_expr_1; \
             } ",
            "",
            "_aro_expr_0",
        );
    }

    #[test]
    fn handles_functions() {
        assert_lift(
            "2 |> (fn x: Int =Int=> x + 1)",
            "_Aro_Any* _aro_expr_0; \
             _aro_expr_0 = malloc(sizeof(_Aro_Any) * 1); \
             _aro_expr_0[0].Void_Ptr = _aro_func_0;  \
             int _aro_expr_1; \
             _aro_expr_1 = (*(int (*)(int , _Aro_Any* ))_aro_expr_0[0].Void_Ptr)\
             (2, &_aro_expr_0[1]); ",
            "int _aro_func_0(int _aro_arg, _Aro_Any* _aro_captures) { \
             int * aro_x; \
             aro_x = malloc(sizeof(int )); \
             *aro_x = _aro_arg; \
             int _aro_expr_0; \
             _aro_expr_0 = ((*aro_x) + 1); \
             return _aro_expr_0; \
             } ",
            "_aro_expr_1",
        );
    }

    #[test]
    fn unpacks_tuples() {
        assert_lift(
            "
            let (a: Int  b: Num) <- (1 2.3)
            a + b
            ",
            "double _aro_expr_0; \
             { \
             int * aro_a; \
             aro_a = malloc(sizeof(int )); \
             double * aro_b; \
             aro_b = malloc(sizeof(double )); \
             _Aro_Any* _aro_expr_1; \
             _aro_expr_1 = malloc(sizeof(_Aro_Any) * 2); \
             _aro_expr_1[0].Int = 1; \
             _aro_expr_1[1].Float = 2.3; \
             *aro_a = ((int )_aro_expr_1[0].Int); \
             *aro_b = ((double )_aro_expr_1[1].Float); \
             double _aro_expr_2; \
             _aro_expr_2 = ((*aro_a) + (*aro_b)); \
             _aro_expr_0 = _aro_expr_2; \
             } ",
            "",
            "_aro_expr_0",
        );
        assert_lift(
            "
            (1 2.3) |> (fn (a: Int  b: Num) =Num=> a + b)
            ",
            "_Aro_Any* _aro_expr_0; \
             _aro_expr_0 = malloc(sizeof(_Aro_Any) * 1); \
             _aro_expr_0[0].Void_Ptr = _aro_func_0;  \
             _Aro_Any* _aro_expr_1; \
             _aro_expr_1 = malloc(sizeof(_Aro_Any) * 2); \
             _aro_expr_1[0].Int = 1; \
             _aro_expr_1[1].Float = 2.3; \
             double _aro_expr_2; \
             _aro_expr_2 = (*(double (*)(_Aro_Any* , _Aro_Any* ))_aro_expr_0[0].Void_Ptr)\
             (_aro_expr_1, &_aro_expr_0[1]); ",
            "double _aro_func_0(_Aro_Any* _aro_arg, _Aro_Any* _aro_captures) { \
             int * aro_a; \
             aro_a = malloc(sizeof(int )); \
             double * aro_b; \
             aro_b = malloc(sizeof(double )); \
             *aro_a = ((int )_aro_arg[0].Int); \
             *aro_b = ((double )_aro_arg[1].Float); \
             double _aro_expr_0; \
             _aro_expr_0 = ((*aro_a) + (*aro_b)); \
             return _aro_expr_0; \
             } ",
            "_aro_expr_2",
        );
    }
}
