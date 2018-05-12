use ast::{Ast, BinOp, CExpr, CFunc, CStatement, CType, CValue, Type, TypedAst, TypedExpression,
          TypedPattern, TypedValue};
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

fn get_ident_name(name: &str) -> String {
    format!("aro_{}", name)
}

fn type_to_ctype(t: &Type) -> CType {
    match t {
        &Type::Any => CType::Any,
        &Type::Num => CType::Float,
        &Type::Int => CType::Int,
        &Type::Bool => CType::Bool,
        &Type::Func(ref param, ref ret) => CType::Closure {
            param: param.replace_expr(type_to_ctype(&param.expr)),
            ret: ret.replace_expr(type_to_ctype(&ret.expr)),
        },
        &Type::Tuple(_) => CType::Object,
        &Type::GenericFunc(_, _, ref body) => type_to_ctype(&body.expr),
        &Type::Ident(ref name, ref supertype) => type_to_ctype(
            &supertype
                .as_ref()
                .expect(&format!("Ident `{}` not typechecked", name))
                .expr,
        ),
        _ => panic!("Unhandled type: {}", t),
    }
}

fn find_names_in_pattern(pattern: &TypedAst<TypedPattern, Type>, names: &mut HashSet<String>) {
    match &*pattern.expr {
        &TypedPattern::Ident(ref name) => {
            names.insert(name.clone());
        }
        &TypedPattern::Tuple(ref vec) => for element in vec {
            find_names_in_pattern(element, names);
        },
    }
}

fn find_captures(
    ast: &TypedAst<TypedExpression, Type>,
    locals: &HashSet<String>,
    captures: &mut HashMap<String, CType>,
) {
    match &*ast.expr {
        &TypedExpression::BinOp(_, ref left, ref right) => {
            find_captures(left, locals, captures);
            find_captures(right, locals, captures);
        }
        &TypedExpression::Ident(ref name) => {
            if !locals.contains(name) {
                captures.insert(name.clone(), type_to_ctype(&ast.expr_type));
            }
        }
        &TypedExpression::If(ref condition, ref consequent, ref alternate) => {
            find_captures(condition, locals, captures);
            find_captures(consequent, locals, captures);
            find_captures(alternate, locals, captures);
        }
        &TypedExpression::Let(ref pattern, ref value, ref body) => {
            let mut locals = locals.clone();
            find_names_in_pattern(pattern, &mut locals);
            find_captures(value, &locals, captures);
            find_captures(body, &locals, captures);
        }
        &TypedExpression::RecordAccess(ref record, _) => {
            find_captures(record, locals, captures);
        }
        &TypedExpression::Sequence(ref side_effect, ref result) => {
            find_captures(side_effect, locals, captures);
            find_captures(result, locals, captures);
        }
        &TypedExpression::Value(ref value) => match value {
            &TypedValue::Bool(_)
            | &TypedValue::Hook(_)
            | &TypedValue::Int(_)
            | &TypedValue::Num(_) => {}
            &TypedValue::Func(ref pattern, ref body) => {
                let mut locals = locals.clone();
                find_names_in_pattern(pattern, &mut locals);
                find_captures(body, &locals, captures);
            }
            &TypedValue::List(ref vec) => for element in vec {
                find_captures(element, locals, captures);
            },
            &TypedValue::Tuple(ref vec) => for element in vec {
                find_captures(element, locals, captures);
            },
            &TypedValue::Record(ref map) => for (_, element) in map {
                find_captures(element, locals, captures);
            },
        },
    }
}

fn make_declarations(pattern: &TypedAst<TypedPattern, Type>, scope: &mut Vec<Ast<CStatement>>) {
    match &*pattern.expr {
        &TypedPattern::Ident(ref name) => {
            let value_name = get_ident_name(name);
            let value_ctype = type_to_ctype(&pattern.expr_type);
            let value_ref_type = CType::Ref(Box::new(value_ctype.clone()));

            scope.push(
                pattern.replace_untyped(CStatement::VarDecl(value_ref_type, value_name.clone())),
            );
            scope.push(
                pattern.replace_untyped(CStatement::RefAlloc(value_name.clone(), value_ctype)),
            );
        }
        &TypedPattern::Tuple(ref vec) => for element in vec {
            make_declarations(element, scope);
        },
    }
}

fn bind_declarations(
    pattern: &TypedAst<TypedPattern, Type>,
    value: Ast<CExpr>,
    scope: &mut Vec<Ast<CStatement>>,
) {
    match &*pattern.expr {
        &TypedPattern::Ident(ref name) => {
            let value_name = get_ident_name(name);
            scope.push(pattern.replace_untyped(CStatement::RefAssign(value_name, value)));
        }
        &TypedPattern::Tuple(ref vec) => for (index, element) in vec.iter().enumerate() {
            let inner_value = value.replace_expr(CExpr::ObjectAccess {
                object: value.clone(),
                index,
                field_type: element.replace_untyped(type_to_ctype(&element.expr_type)),
            });
            bind_declarations(element, inner_value, scope);
        },
    }
}

pub fn lift_expr(
    ast: &TypedAst<TypedExpression, Type>,
    scope: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut i32,
) -> Ast<CValue> {
    match &*ast.expr {
        &TypedExpression::Value(ref v) => match v {
            &TypedValue::Num(value) => ast.replace_untyped(CValue::Float(value)),
            &TypedValue::Bool(value) => ast.replace_untyped(CValue::Bool(value)),
            &TypedValue::Int(value) => ast.replace_untyped(CValue::Int(value)),
            &TypedValue::Func(ref pattern, ref body) => {
                let mut captures_map = HashMap::new();
                let mut locals = HashSet::new();
                find_names_in_pattern(pattern, &mut locals);
                find_captures(body, &locals, &mut captures_map);

                let func_name = get_func_name(function_index);
                let mut func_scope = Vec::new();
                let mut func_expr_index = 0;

                let param_ctype = type_to_ctype(&pattern.expr_type);
                let param_value = pattern.replace_untyped(CExpr::Value(CValue::Ident(
                    String::from("_aro_arg"),
                    param_ctype.clone(),
                )));
                make_declarations(pattern, &mut func_scope);
                bind_declarations(pattern, param_value, &mut func_scope);

                for (index, (name, var_type)) in captures_map.iter().enumerate() {
                    let ident_name = get_ident_name(name);
                    func_scope.push(ast.replace_untyped(CStatement::VarDecl(
                        CType::Ref(Box::new(var_type.clone())),
                        ident_name.clone(),
                    )));

                    let captures_object = ast.replace_untyped(CExpr::Value(CValue::Ident(
                        String::from("_aro_captures"),
                        CType::Object,
                    )));
                    func_scope.push(ast.replace_untyped(CStatement::VarAssign(
                        ident_name,
                        ast.replace_untyped(CExpr::ObjectAccess {
                            object: captures_object,
                            index,
                            field_type: ast.replace_untyped(CType::Ref(Box::new(var_type.clone()))),
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
                let closure_type = type_to_ctype(&ast.expr_type);
                scope.push(ast.replace_untyped(CStatement::VarDecl(
                    closure_type.clone(),
                    closure_name.clone(),
                )));
                scope.push(
                    ast.replace_untyped(CStatement::ClosureInit {
                        name: closure_name.clone(),
                        function: ast.replace_untyped(CValue::Ident(
                            func_name.clone(),
                            CType::VoidPtr,
                        )),
                        captures: captures_map
                            .into_iter()
                            .map(|(name, var_type)| {
                                ast.replace_untyped(CValue::Ident(
                                    get_ident_name(&name),
                                    CType::Ref(Box::new(var_type.clone())),
                                ))
                            })
                            .collect(),
                    }),
                );

                functions.push(ast.replace_untyped(CFunc {
                    name: func_name,
                    param: pattern.replace_untyped(param_ctype),
                    body: func_scope,
                    ret,
                }));

                ast.replace_untyped(CValue::Ident(closure_name, closure_type))
            }
            &TypedValue::Tuple(ref vec) => {
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
                scope.push(ast.replace_untyped(CStatement::VarDecl(
                    CType::Object,
                    expr_name.clone(),
                )));
                scope.push(ast.replace_untyped(CStatement::ObjectInit {
                    name: expr_name.clone(),
                    data,
                }));

                ast.replace_untyped(CValue::Ident(expr_name, CType::Object))
            }
            _ => panic!(),
        },
        &TypedExpression::Ident(ref name) => ast.replace_untyped(CValue::Deref(
            get_ident_name(name),
            type_to_ctype(&ast.expr_type),
        )),
        &TypedExpression::BinOp(ref op, ref left, ref right) => {
            let left_c_ast = lift_expr(left, scope, expr_index, functions, function_index);
            let right_c_ast = {
                let right_c_ast = lift_expr(right, scope, expr_index, functions, function_index);
                match op {
                    &BinOp::Call => {
                        if let CType::Closure { param, .. } = left_c_ast.expr.get_ctype() {
                            let right_ctype = type_to_ctype(&right.expr_type);
                            if *param.expr == CType::Any && right_ctype != CType::Any {
                                let any_name = get_expr_name(expr_index);
                                scope.push(right.replace_untyped(CStatement::VarDecl(
                                    CType::Any,
                                    any_name.clone(),
                                )));
                                scope.push(right.replace_untyped(CStatement::AnyAssign {
                                    name: any_name.clone(),
                                    value: right_c_ast.to_c_expr(),
                                    value_type: right_ctype,
                                }));

                                right.replace_untyped(CValue::Ident(any_name, CType::Any))
                            } else {
                                right_c_ast
                            }
                        } else {
                            panic!()
                        }
                    }
                    _ => right_c_ast,
                }
            };

            let expr_name = get_expr_name(expr_index);
            let expr_type = type_to_ctype(&ast.expr_type);
            scope.push(ast.replace_untyped(CStatement::VarDecl(
                expr_type.clone(),
                expr_name.clone(),
            )));
            scope.push(ast.replace_untyped(CStatement::VarAssign(
                expr_name.clone(),
                ast.replace_untyped({
                    let bin_op_ast = CExpr::BinOp(op.clone(), left_c_ast.clone(), right_c_ast);
                    match op {
                        &BinOp::Call => {
                            if let CType::Closure { ret, .. } = left_c_ast.expr.get_ctype() {
                                if *ret.expr == CType::Any && expr_type != CType::Any {
                                    CExpr::AnyAccess {
                                        value: ast.replace_untyped(bin_op_ast),
                                        value_type: expr_type.clone(),
                                    }
                                } else {
                                    bin_op_ast
                                }
                            } else {
                                panic!()
                            }
                        }
                        _ => bin_op_ast,
                    }
                }),
            )));

            ast.replace_untyped(CValue::Ident(expr_name, expr_type))
        }
        &TypedExpression::If(ref condition, ref consequent, ref alternate) => {
            let condition_c_ast =
                lift_expr(condition, scope, expr_index, functions, function_index);

            let expr_name = get_expr_name(expr_index);
            let expr_type = type_to_ctype(&ast.expr_type);

            let mut consequent_scope = Vec::new();
            let consequent_c_ast = lift_expr(
                consequent,
                &mut consequent_scope,
                expr_index,
                functions,
                function_index,
            );
            consequent_scope.push(ast.replace_untyped(CStatement::VarAssign(
                expr_name.clone(),
                consequent_c_ast.to_c_expr(),
            )));
            let consequent_block = consequent.replace_untyped(CStatement::Block(consequent_scope));

            let mut alternate_scope = Vec::new();
            let alternate_c_ast = lift_expr(
                alternate,
                &mut alternate_scope,
                expr_index,
                functions,
                function_index,
            );
            alternate_scope.push(ast.replace_untyped(CStatement::VarAssign(
                expr_name.clone(),
                alternate_c_ast.to_c_expr(),
            )));
            let alternate_block = alternate.replace_untyped(CStatement::Block(alternate_scope));

            scope.push(ast.replace_untyped(CStatement::VarDecl(
                expr_type.clone(),
                expr_name.clone(),
            )));
            scope.push(ast.replace_untyped(CStatement::If(
                condition_c_ast.to_c_expr(),
                consequent_block,
                alternate_block,
            )));

            ast.replace_untyped(CValue::Ident(expr_name, expr_type))
        }
        &TypedExpression::Let(ref pattern, ref value, ref body) => {
            let body_name = get_expr_name(expr_index);
            let body_type = type_to_ctype(&body.expr_type);
            scope.push(body.replace_untyped(CStatement::VarDecl(
                body_type.clone(),
                body_name.clone(),
            )));

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
            body_scope.push(body.replace_untyped(CStatement::VarAssign(
                body_name.clone(),
                body_c_ast.to_c_expr(),
            )));

            scope.push(ast.replace_untyped(CStatement::Block(body_scope)));

            ast.replace_untyped(CValue::Ident(body_name, body_type))
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
        let typechecked_ast = typecheck_ast(&ast, &HashMap::new()).unwrap();

        let mut scope = Vec::new();
        let mut expr_index = 0;
        let mut functions = Vec::new();
        let mut function_index = 0;
        let actual_expr = lift_expr(
            &typechecked_ast,
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
