use ast::{Ast, BinOp, CDeclaration, CExpr, CFunc, CStatement, CType, CValue, EvaluatedType,
          TypedAst, TypedExpression, TypedPattern, TypedValue, WellCTyped};
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

fn get_adaptor_func_name(func_index: &mut i32) -> String {
    let old_i = *func_index;
    *func_index += 1;
    format!("_aro_adaptor_func_{}", old_i)
}

fn get_ident_name(name: &str) -> String {
    format!("aro_{}", name)
}

fn type_to_ctype(t: &EvaluatedType) -> CType {
    match t {
        &EvaluatedType::Any => CType::Any,
        &EvaluatedType::Num => CType::Float,
        &EvaluatedType::Int => CType::Int,
        &EvaluatedType::Bool => CType::Bool,
        &EvaluatedType::Func(ref param, ref ret) => CType::Closure {
            param: param.replace_expr(type_to_ctype(&param.expr)),
            ret: ret.replace_expr(type_to_ctype(&ret.expr)),
        },
        &EvaluatedType::Tuple(_) => CType::Object,
        &EvaluatedType::GenericFunc(_, _, ref body) => type_to_ctype(&body.expr),
        &EvaluatedType::Ident(_, ref supertype) => type_to_ctype(&supertype.expr),
        _ => panic!("Unhandled type: {}", t),
    }
}

fn maybe_cast_representation(
    from: Ast<CExpr>,
    from_type: &EvaluatedType,
    to_type: &EvaluatedType,
    declarations: &mut Vec<CDeclaration>,
    scope: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut i32,
) -> (Ast<CExpr>, bool) {
    match (from_type, to_type) {
        (&EvaluatedType::Int, &EvaluatedType::Num) => (
            from.replace_expr(CExpr::Cast {
                value: from.clone(),
                to_type: CType::Float,
            }),
            true,
        ),
        (&EvaluatedType::Num, &EvaluatedType::Int) => (
            from.replace_expr(CExpr::Cast {
                value: from.clone(),
                to_type: CType::Int,
            }),
            true,
        ),
        (&EvaluatedType::Any, to_type) => (
            from.replace_expr(CExpr::AnyAccess {
                value: from.clone(),
                value_type: type_to_ctype(to_type),
            }),
            true,
        ),
        (ref from_type, &EvaluatedType::Any) => (
            from.replace_expr(CExpr::Cast {
                value: from.clone(),
                to_type: CType::Any,
            }),
            true,
        ),
        (&EvaluatedType::Tuple(ref from_elements), &EvaluatedType::Tuple(ref to_elements)) => {
            let mut casted_elements = Vec::new();
            let mut something_was_casted = false;
            for (index, (from_element, to_element)) in from_elements
                .into_iter()
                .zip(to_elements.into_iter())
                .enumerate()
            {
                let (c_element, was_casted) = maybe_cast_representation(
                    from.replace_expr(CExpr::ObjectAccess {
                        object: from.clone(),
                        index,
                        field_type: type_to_ctype(&from_element.expr),
                    }),
                    &from_element.expr,
                    &to_element.expr,
                    declarations,
                    scope,
                    expr_index,
                    functions,
                    function_index,
                );
                casted_elements.push(c_element);
                if was_casted {
                    something_was_casted = true;
                }
            }

            if something_was_casted {
                let copy_name = get_expr_name(expr_index);
                declarations.push(CDeclaration(CType::Object, copy_name.clone()));
                scope.push(from.replace_expr(CStatement::ObjectInit {
                    name: copy_name.clone(),
                    data: casted_elements,
                }));
                (
                    from.replace_expr(CExpr::Value(CValue::Ident(copy_name, CType::Object))),
                    true,
                )
            } else {
                (from.clone(), false)
            }
        }
        (
            &EvaluatedType::Func(ref from_in, ref from_out),
            &EvaluatedType::Func(ref to_in, ref to_out),
        ) => {
            let mut adaptor_func_declarations = Vec::new();
            let mut adaptor_func_scope = Vec::new();
            let mut adaptor_func_expr_index = 0;

            let arg = to_in.replace_expr(CExpr::Value(CValue::Ident(
                String::from("_aro_arg"),
                type_to_ctype(&to_in.expr),
            )));
            let (arg, arg_was_casted) = maybe_cast_representation(
                arg,
                &to_in.expr,
                &from_in.expr,
                &mut adaptor_func_declarations,
                &mut adaptor_func_scope,
                &mut adaptor_func_expr_index,
                functions,
                function_index,
            );

            let arg_name = get_expr_name(&mut adaptor_func_expr_index);
            let arg_ctype = type_to_ctype(&from_in.expr);
            adaptor_func_declarations.push(CDeclaration(arg_ctype.clone(), arg_name.clone()));
            adaptor_func_scope
                .push(arg.replace_expr(CStatement::VarAssign(arg_name.clone(), arg.clone())));

            let inner_func_name = get_expr_name(&mut adaptor_func_expr_index);
            let inner_func_ctype = type_to_ctype(from_type);
            adaptor_func_declarations.push(CDeclaration(
                inner_func_ctype.clone(),
                inner_func_name.clone(),
            ));
            adaptor_func_scope.push(from.replace_expr(CStatement::VarAssign(
                inner_func_name.clone(),
                from.replace_expr(CExpr::ObjectAccess {
                    index: 0,
                    object: from.replace_expr(CExpr::Value(CValue::Ident(
                        String::from("_aro_captures"),
                        CType::Object,
                    ))),
                    field_type: inner_func_ctype.clone(),
                }),
            )));

            let ret = from_out.replace_expr(CExpr::BinOp(
                BinOp::Call,
                from.replace_expr(CValue::Ident(inner_func_name, inner_func_ctype)),
                arg.replace_expr(CValue::Ident(arg_name, arg_ctype)),
                type_to_ctype(&from_out.expr),
            ));
            let (ret, ret_was_casted) = maybe_cast_representation(
                ret,
                &from_out.expr,
                &to_out.expr,
                &mut adaptor_func_declarations,
                &mut adaptor_func_scope,
                &mut adaptor_func_expr_index,
                functions,
                function_index,
            );

            if arg_was_casted || ret_was_casted {
                let adaptor_func_name = get_adaptor_func_name(function_index);
                functions.push(from.replace_expr(CFunc {
                    name: adaptor_func_name.clone(),
                    param: to_in.replace_expr(type_to_ctype(&to_in.expr)),
                    declarations: adaptor_func_declarations,
                    body: adaptor_func_scope,
                    ret,
                }));

                let adaptor_closure_name = get_expr_name(expr_index);
                let adaptor_closure_type = type_to_ctype(to_type);
                declarations.push(CDeclaration(
                    adaptor_closure_type.clone(),
                    adaptor_closure_name.clone(),
                ));
                scope.push(from.replace_expr(CStatement::ClosureInit {
                    name: adaptor_closure_name.clone(),
                    function: from.replace_expr(CValue::Ident(adaptor_func_name, CType::VoidPtr)),
                    captures: vec![from.clone()],
                }));

                (
                    from.replace_expr(CExpr::Value(CValue::Ident(
                        adaptor_closure_name,
                        adaptor_closure_type,
                    ))),
                    true,
                )
            } else {
                (from, false)
            }
        }
        _ => (from, false),
    }
}

fn find_names_in_pattern(pattern: &TypedAst<TypedPattern>, names: &mut HashSet<String>) {
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
    ast: &TypedAst<TypedExpression>,
    locals: &HashSet<String>,
    captures: &mut HashMap<String, CType>,
) {
    match &*ast.expr {
        &TypedExpression::BinOp(_, ref left, ref right) => {
            find_captures(left, locals, captures);
            find_captures(right, locals, captures);
        }
        &TypedExpression::GenericCall((_, _, _, ref generic_func), _) => {
            find_captures(generic_func, locals, captures);
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
            &TypedValue::GenericFunc(_, _, _, ref body) => {
                find_captures(body, locals, captures);
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

fn make_declarations(
    pattern: &TypedAst<TypedPattern>,
    declarations: &mut Vec<CDeclaration>,
    scope: &mut Vec<Ast<CStatement>>,
) {
    match &*pattern.expr {
        &TypedPattern::Ident(ref name) => {
            let value_name = get_ident_name(name);
            let value_ctype = type_to_ctype(&pattern.expr_type);
            let value_ref_type = CType::Ref(Box::new(value_ctype.clone()));

            declarations.push(CDeclaration(value_ref_type, value_name.clone()));
            scope.push(
                pattern.replace_untyped(CStatement::RefAlloc(value_name.clone(), value_ctype)),
            );
        }
        &TypedPattern::Tuple(ref vec) => for element in vec {
            make_declarations(element, declarations, scope);
        },
    }
}

fn bind_declarations(
    pattern: &TypedAst<TypedPattern>,
    value: &TypedAst<CExpr>,
    declarations: &mut Vec<CDeclaration>,
    scope: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut i32,
) {
    match &*pattern.expr {
        &TypedPattern::Ident(ref name) => {
            let value_name = get_ident_name(name);
            let (casted_value, _) = maybe_cast_representation(
                value.to_untyped_ast(),
                &value.expr_type,
                &pattern.expr_type,
                declarations,
                scope,
                expr_index,
                functions,
                function_index,
            );
            scope.push(pattern.replace_untyped(CStatement::RefAssign(value_name, casted_value)));
        }
        &TypedPattern::Tuple(ref vec) => for (index, element) in vec.iter().enumerate() {
            let inner_value_type =
                if let &EvaluatedType::Tuple(ref value_element_types) = &*value.expr_type {
                    *value_element_types[index].expr.clone()
                } else {
                    panic!("Cannot unpack non-tuple `{}`", value.expr_type)
                };
            let inner_value = value.replace_expr_and_type(
                CExpr::ObjectAccess {
                    object: value.to_untyped_ast(),
                    index,
                    field_type: type_to_ctype(&inner_value_type),
                },
                inner_value_type,
            );

            bind_declarations(
                element,
                &inner_value,
                declarations,
                scope,
                expr_index,
                functions,
                function_index,
            );
        },
    }
}

pub fn lift_expr(
    ast: &TypedAst<TypedExpression>,
    declarations: &mut Vec<CDeclaration>,
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
                let mut func_declarations = Vec::new();
                let mut func_scope = Vec::new();
                let mut func_expr_index = 0;

                let param_ctype = type_to_ctype(&pattern.expr_type);
                let param_value = pattern.replace_expr(CExpr::Value(CValue::Ident(
                    String::from("_aro_arg"),
                    param_ctype.clone(),
                )));
                make_declarations(pattern, &mut func_declarations, &mut func_scope);
                bind_declarations(
                    pattern,
                    &param_value,
                    &mut func_declarations,
                    &mut func_scope,
                    &mut func_expr_index,
                    functions,
                    function_index,
                );

                for (index, (name, var_type)) in captures_map.iter().enumerate() {
                    let ident_name = get_ident_name(name);
                    func_declarations.push(CDeclaration(
                        CType::Ref(Box::new(var_type.clone())),
                        ident_name.clone(),
                    ));

                    let captures_object = ast.replace_untyped(CExpr::Value(CValue::Ident(
                        String::from("_aro_captures"),
                        CType::Object,
                    )));
                    func_scope.push(ast.replace_untyped(CStatement::VarAssign(
                        ident_name,
                        ast.replace_untyped(CExpr::ObjectAccess {
                            object: captures_object,
                            index,
                            field_type: CType::Ref(Box::new(var_type.clone())),
                        }),
                    )))
                }

                let ret = lift_expr(
                    body,
                    &mut func_declarations,
                    &mut func_scope,
                    &mut func_expr_index,
                    functions,
                    function_index,
                );

                let closure_name = get_expr_name(expr_index);
                let closure_type = type_to_ctype(&ast.expr_type);
                declarations.push(CDeclaration(closure_type.clone(), closure_name.clone()));
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
                                ast.replace_untyped(CExpr::Value(CValue::Ident(
                                    get_ident_name(&name),
                                    CType::Ref(Box::new(var_type.clone())),
                                )))
                            })
                            .collect(),
                    }),
                );

                functions.push(ast.replace_untyped(CFunc {
                    name: func_name,
                    param: pattern.replace_untyped(param_ctype),
                    declarations: func_declarations,
                    body: func_scope,
                    ret: ret.to_c_expr(),
                }));

                ast.replace_untyped(CValue::Ident(closure_name, closure_type))
            }
            &TypedValue::Tuple(ref vec) => {
                let mut data = Vec::new();
                for element in vec {
                    data.push(
                        lift_expr(
                            element,
                            declarations,
                            scope,
                            expr_index,
                            functions,
                            function_index,
                        ).to_c_expr(),
                    );
                }

                let expr_name = get_expr_name(expr_index);
                declarations.push(CDeclaration(CType::Object, expr_name.clone()));
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
            let left_c_ast = lift_expr(
                left,
                declarations,
                scope,
                expr_index,
                functions,
                function_index,
            );
            let right_c_ast = {
                let right_c_ast = lift_expr(
                    right,
                    declarations,
                    scope,
                    expr_index,
                    functions,
                    function_index,
                );
                match op {
                    &BinOp::Call => {
                        if let &EvaluatedType::Func(ref param_type, _) = &*left.expr_type {
                            let (casted_ast, was_casted) = maybe_cast_representation(
                                right.replace_untyped(CExpr::Value(*right_c_ast.expr.clone())),
                                &right.expr_type,
                                &param_type.expr,
                                declarations,
                                scope,
                                expr_index,
                                functions,
                                function_index,
                            );

                            if was_casted {
                                let expr_name = get_expr_name(expr_index);
                                let expr_ctype = type_to_ctype(&param_type.expr);
                                declarations
                                    .push(CDeclaration(expr_ctype.clone(), expr_name.clone()));
                                scope.push(right.replace_untyped(CStatement::VarAssign(
                                    expr_name.clone(),
                                    casted_ast,
                                )));
                                right.replace_untyped(CValue::Ident(expr_name, expr_ctype))
                            } else {
                                right_c_ast
                            }
                        } else {
                            panic!("Calling non-function `{}`", left.expr_type)
                        }
                    }
                    _ => right_c_ast,
                }
            };

            let result_ast = {
                let bin_op_expr = CExpr::BinOp(
                    op.clone(),
                    left_c_ast.clone(),
                    right_c_ast,
                    type_to_ctype(&ast.expr_type),
                );
                match op {
                    &BinOp::Call => {
                        if let &EvaluatedType::Func(_, ref ret_type) = &*left.expr_type {
                            let (casted, _) = maybe_cast_representation(
                                ast.replace_untyped(bin_op_expr),
                                &ret_type.expr,
                                &ast.expr_type,
                                declarations,
                                scope,
                                expr_index,
                                functions,
                                function_index,
                            );
                            casted
                        } else {
                            panic!("Calling non-function `{}`", left.expr_type)
                        }
                    }
                    _ => ast.replace_untyped(bin_op_expr),
                }
            };

            let expr_name = get_expr_name(expr_index);
            let expr_type = type_to_ctype(&ast.expr_type);
            declarations.push(CDeclaration(expr_type.clone(), expr_name.clone()));
            scope.push(ast.replace_untyped(CStatement::VarAssign(expr_name.clone(), result_ast)));

            ast.replace_untyped(CValue::Ident(expr_name, expr_type))
        }
        &TypedExpression::If(ref condition, ref consequent, ref alternate) => {
            let condition_c_ast = lift_expr(
                condition,
                declarations,
                scope,
                expr_index,
                functions,
                function_index,
            );

            let expr_name = get_expr_name(expr_index);
            let expr_type = type_to_ctype(&ast.expr_type);

            let mut consequent_declarations = Vec::new();
            let mut consequent_scope = Vec::new();
            let consequent_c_ast = lift_expr(
                consequent,
                &mut consequent_declarations,
                &mut consequent_scope,
                expr_index,
                functions,
                function_index,
            );
            consequent_scope.push(ast.replace_untyped(CStatement::VarAssign(
                expr_name.clone(),
                consequent_c_ast.to_c_expr(),
            )));
            let consequent_block = consequent
                .replace_untyped(CStatement::Block(consequent_declarations, consequent_scope));

            let mut alternate_declarations = Vec::new();
            let mut alternate_scope = Vec::new();
            let alternate_c_ast = lift_expr(
                alternate,
                &mut alternate_declarations,
                &mut alternate_scope,
                expr_index,
                functions,
                function_index,
            );
            alternate_scope.push(ast.replace_untyped(CStatement::VarAssign(
                expr_name.clone(),
                alternate_c_ast.to_c_expr(),
            )));
            let alternate_block = alternate
                .replace_untyped(CStatement::Block(alternate_declarations, alternate_scope));

            declarations.push(CDeclaration(expr_type.clone(), expr_name.clone()));
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
            declarations.push(CDeclaration(body_type.clone(), body_name.clone()));

            let mut body_declarations = Vec::new();
            let mut body_scope = Vec::new();
            make_declarations(pattern, &mut body_declarations, &mut body_scope);
            let value_c_ast = lift_expr(
                value,
                &mut body_declarations,
                &mut body_scope,
                expr_index,
                functions,
                function_index,
            );
            bind_declarations(
                pattern,
                &value.replace_expr(CExpr::Value(*value_c_ast.expr)),
                &mut body_declarations,
                &mut body_scope,
                expr_index,
                functions,
                function_index,
            );

            let body_c_ast = lift_expr(
                body,
                &mut body_declarations,
                &mut body_scope,
                expr_index,
                functions,
                function_index,
            );
            body_scope.push(body.replace_untyped(CStatement::VarAssign(
                body_name.clone(),
                body_c_ast.to_c_expr(),
            )));

            scope.push(ast.replace_untyped(CStatement::Block(body_declarations, body_scope)));

            ast.replace_untyped(CValue::Ident(body_name, body_type))
        }
        &TypedExpression::GenericCall(
            (ref name, ref supertype, ref body, ref generic_func),
            ref type_arg,
        ) => {
            // TODO: generate adaptor code and adaptor functions
            panic!()
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

        let mut declarations = Vec::new();
        let mut scope = Vec::new();
        let mut expr_index = 0;
        let mut functions = Vec::new();
        let mut function_index = 0;
        let actual_expr = lift_expr(
            &typechecked_ast,
            &mut declarations,
            &mut scope,
            &mut expr_index,
            &mut functions,
            &mut function_index,
        );

        let mut actual_scope = String::new();
        for statement in declarations {
            actual_scope += &format!("{} ", statement);
        }
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
             int _aro_expr_1; \
             double _aro_expr_2; \
             bool _aro_expr_3; \
             _aro_expr_0 = (1 - 1); \
             _aro_expr_1 = (8 * 3); \
             _aro_expr_2 = (1.3 + _aro_expr_1); \
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
             double _aro_expr_1; \
             _aro_expr_0 = (1 <= 2); \
             if (_aro_expr_0) {  \
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
             int _aro_expr_1; \
             double _aro_expr_2; \
             aro_x = malloc(sizeof(double )); \
             _aro_expr_1 = (5 + 2); \
             *aro_x = _aro_expr_1; \
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
             int _aro_expr_1; \
             aro_x = malloc(sizeof(double )); \
             *aro_x = 5; \
             { \
             int * aro_x; \
             int _aro_expr_2; \
             aro_x = malloc(sizeof(int )); \
             *aro_x = 4; \
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
            let my_tuple: (Int Num) <- (1 2.3)
            let (a: Num  b: Num) <- my_tuple
            a + b
            ",
            "double _aro_expr_0; \
             { \
             _Aro_Any* * aro_my_tuple; \
             aro_my_tuple = malloc(sizeof(_Aro_Any* )); \
             _Aro_Any* _aro_expr_1; \
             _aro_expr_1 = malloc(sizeof(_Aro_Any) * 2); \
             _aro_expr_1[0].Int = 1; \
             _aro_expr_1[1].Float = 2.3; \
             *aro_my_tuple = _aro_expr_1; \
             double _aro_expr_2; \
             { \
             double * aro_a; \
             aro_a = malloc(sizeof(double )); \
             double * aro_b; \
             aro_b = malloc(sizeof(double )); \
             *aro_a = ((double )(*aro_my_tuple)[0].Int); \
             *aro_b = ((double )(*aro_my_tuple)[1].Float); \
             double _aro_expr_3; \
             _aro_expr_3 = ((*aro_a) + (*aro_b)); \
             _aro_expr_2 = _aro_expr_3; \
             } \
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
