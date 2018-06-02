use ast::{
    Ast, BinOp, CDeclaration, CExpr, CFunc, CStatement, CType, CValue, EvaluatedType, TypedAst,
    TypedExpression, TypedPattern, TypedValue,
};
use std::collections::{HashMap, HashSet};

fn get_expr_name(name: &str, expr_index: &mut i32) -> String {
    let old_i = *expr_index;
    *expr_index += 1;
    format!("_aro_expr_{}_{}", name, old_i)
}

fn get_func_name(func_index: &mut i32) -> String {
    let old_i = *func_index;
    *func_index += 1;
    format!("_aro_func_{}", old_i)
}

fn get_adaptor_func_name(func_index: &mut i32) -> String {
    let old_i = *func_index;
    *func_index += 1;
    format!("_aro_func_adaptor_{}", old_i)
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
        &EvaluatedType::GenericFunc { ref output, .. } => type_to_ctype(&output.expr),
        &EvaluatedType::Ident(_, ref supertype) => type_to_ctype(&supertype.expr),
        _ => panic!("Unhandled type: {}", t),
    }
}

fn maybe_cast_representation(
    from: Ast<CValue>,
    from_type: &EvaluatedType,
    to_type: &EvaluatedType,
    declarations: &mut Vec<CDeclaration>,
    statements: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut i32,
) -> (Ast<CValue>, bool) {
    match (from_type, to_type) {
        (&EvaluatedType::Int, &EvaluatedType::Num) => {
            let expr_name = get_expr_name("casted", expr_index);
            let expr_ctype = CType::Float;
            declarations.push(CDeclaration(expr_ctype.clone(), expr_name.clone()));
            statements.push(from.replace_expr(CStatement::VarAssign(
                expr_name.clone(),
                from.replace_expr(CExpr::Cast {
                    value: from.clone(),
                    to_type: expr_ctype.clone(),
                }),
            )));
            (
                from.replace_expr(CValue::Ident(expr_name, expr_ctype)),
                true,
            )
        }
        (&EvaluatedType::Num, &EvaluatedType::Int) => {
            let expr_name = get_expr_name("casted", expr_index);
            let expr_ctype = CType::Int;
            declarations.push(CDeclaration(expr_ctype.clone(), expr_name.clone()));
            statements.push(from.replace_expr(CStatement::VarAssign(
                expr_name.clone(),
                from.replace_expr(CExpr::Cast {
                    value: from.clone(),
                    to_type: expr_ctype.clone(),
                }),
            )));
            (
                from.replace_expr(CValue::Ident(expr_name, expr_ctype)),
                true,
            )
        }
        (&EvaluatedType::Any, &EvaluatedType::Any) => (from, false),
        (&EvaluatedType::Any, to_type) => {
            let expr_name = get_expr_name("casted", expr_index);
            let expr_ctype = type_to_ctype(to_type);
            declarations.push(CDeclaration(expr_ctype.clone(), expr_name.clone()));
            statements.push(from.replace_expr(CStatement::VarAssign(
                expr_name.clone(),
                from.replace_expr(CExpr::AnyAccess {
                    value: from.clone(),
                    value_type: expr_ctype.clone(),
                }),
            )));
            (
                from.replace_expr(CValue::Ident(expr_name, expr_ctype)),
                true,
            )
        }
        (_, &EvaluatedType::Any) => {
            let expr_name = get_expr_name("casted", expr_index);
            let expr_ctype = CType::Any;
            declarations.push(CDeclaration(expr_ctype.clone(), expr_name.clone()));
            statements.push(from.replace_expr(CStatement::VarAssign(
                expr_name.clone(),
                from.replace_expr(CExpr::Cast {
                    value: from.clone(),
                    to_type: expr_ctype.clone(),
                }),
            )));
            (
                from.replace_expr(CValue::Ident(expr_name, expr_ctype)),
                true,
            )
        }
        (&EvaluatedType::Tuple(ref from_elements), &EvaluatedType::Tuple(ref to_elements)) => {
            let mut casted_elements = Vec::new();
            let mut something_was_casted = false;
            let mut temp_declarations = Vec::new();
            let mut temp_statements = Vec::new();
            for (index, (from_element, to_element)) in from_elements
                .into_iter()
                .zip(to_elements.into_iter())
                .enumerate()
            {
                let expr_name = get_expr_name("object_access", expr_index);
                let expr_ctype = type_to_ctype(&from_element.expr);
                temp_declarations.push(CDeclaration(expr_ctype.clone(), expr_name.clone()));
                temp_statements.push(from.replace_expr(CStatement::VarAssign(
                    expr_name.clone(),
                    from.replace_expr(CExpr::ObjectAccess {
                        object: from.clone(),
                        index,
                        field_type: expr_ctype.clone(),
                    }),
                )));
                let ident = from.replace_expr(CValue::Ident(expr_name, expr_ctype));

                let (c_element, was_casted) = maybe_cast_representation(
                    ident,
                    &from_element.expr,
                    &to_element.expr,
                    &mut temp_declarations,
                    &mut temp_statements,
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
                declarations.append(&mut temp_declarations);
                statements.append(&mut temp_statements);

                let copy_name = get_expr_name("casted_tuple", expr_index);
                declarations.push(CDeclaration(CType::Object, copy_name.clone()));
                statements.push(from.replace_expr(CStatement::ObjectInit {
                    name: copy_name.clone(),
                    data: casted_elements,
                }));
                (
                    from.replace_expr(CValue::Ident(copy_name, CType::Object)),
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
            let mut adaptor_func_statements = Vec::new();
            let mut adaptor_func_expr_index = 0;

            let arg = to_in.replace_expr(CValue::Ident(
                String::from("_aro_arg"),
                type_to_ctype(&to_in.expr),
            ));
            let (arg, arg_was_casted) = maybe_cast_representation(
                arg,
                &to_in.expr,
                &from_in.expr,
                &mut adaptor_func_declarations,
                &mut adaptor_func_statements,
                &mut adaptor_func_expr_index,
                functions,
                function_index,
            );

            let inner_func_name = get_expr_name("inner_closure", &mut adaptor_func_expr_index);
            let inner_func_ctype = type_to_ctype(from_type);
            adaptor_func_declarations.push(CDeclaration(
                inner_func_ctype.clone(),
                inner_func_name.clone(),
            ));
            adaptor_func_statements.push(from.replace_expr(CStatement::VarAssign(
                inner_func_name.clone(),
                from.replace_expr(CExpr::ObjectAccess {
                    index: 0,
                    object: from.replace_expr(CValue::Ident(
                        String::from("_aro_captures"),
                        CType::Object,
                    )),
                    field_type: inner_func_ctype.clone(),
                }),
            )));

            let ret = from_out.replace_expr(CExpr::BinOp(
                BinOp::Call,
                from.replace_expr(CValue::Ident(inner_func_name, inner_func_ctype)),
                arg,
                type_to_ctype(&from_out.expr),
            ));
            let ret_name = get_expr_name("ret", &mut adaptor_func_expr_index);
            let ret_ctype = type_to_ctype(&from_out.expr);
            adaptor_func_declarations.push(CDeclaration(ret_ctype.clone(), ret_name.clone()));
            adaptor_func_statements
                .push(from_out.replace_expr(CStatement::VarAssign(ret_name.clone(), ret)));
            let (ret, ret_was_casted) = maybe_cast_representation(
                from_out.replace_expr(CValue::Ident(ret_name, ret_ctype)),
                &from_out.expr,
                &to_out.expr,
                &mut adaptor_func_declarations,
                &mut adaptor_func_statements,
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
                    body: adaptor_func_statements,
                    ret,
                }));

                let adaptor_closure_name = get_expr_name("adaptor_closure", expr_index);
                let adaptor_closure_type = type_to_ctype(to_type);
                declarations.push(CDeclaration(
                    adaptor_closure_type.clone(),
                    adaptor_closure_name.clone(),
                ));
                statements.push(from.replace_expr(CStatement::ClosureInit {
                    name: adaptor_closure_name.clone(),
                    function: from.replace_expr(CValue::Ident(adaptor_func_name, CType::VoidPtr)),
                    captures: vec![from.clone()],
                }));

                (
                    from.replace_expr(CValue::Ident(adaptor_closure_name, adaptor_closure_type)),
                    true,
                )
            } else {
                (from, false)
            }
        }
        (
            &EvaluatedType::GenericFunc {
                substituted_output: ref from_type_ast,
                ..
            },
            &EvaluatedType::GenericFunc {
                substituted_output: ref to_type_ast,
                ..
            },
        ) => maybe_cast_representation(
            from,
            &from_type_ast.expr,
            &to_type_ast.expr,
            declarations,
            statements,
            expr_index,
            functions,
            function_index,
        ),
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
        &TypedExpression::Cast(ref from, _) => {
            find_captures(from, locals, captures);
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
            &TypedValue::Func(ref pattern, _, ref body) => {
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

fn make_declarations(
    pattern: &TypedAst<TypedPattern>,
    declarations: &mut Vec<CDeclaration>,
    statements: &mut Vec<Ast<CStatement>>,
) {
    match &*pattern.expr {
        &TypedPattern::Ident(ref name) => {
            let value_name = get_ident_name(name);
            let wrapper_type = CType::Ref(Box::new(type_to_ctype(&pattern.expr_type)));
            let wrapper_ref_type = CType::Ref(Box::new(wrapper_type.clone()));

            declarations.push(CDeclaration(wrapper_ref_type, value_name.clone()));
            statements.push(
                pattern.replace_untyped(CStatement::RefAlloc(value_name.clone(), wrapper_type)),
            );
            statements.push(pattern.replace_untyped(CStatement::RefAssign(
                value_name.clone(),
                pattern.replace_untyped(CExpr::Value(CValue::Int(0))),
            )));
        }
        &TypedPattern::Tuple(ref vec) => for element in vec {
            make_declarations(element, declarations, statements);
        },
    }
}

fn bind_declarations(
    pattern: &TypedAst<TypedPattern>,
    value: &Ast<CValue>,
    value_type: &EvaluatedType,
    declarations: &mut Vec<CDeclaration>,
    statements: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut i32,
) {
    match &*pattern.expr {
        &TypedPattern::Ident(ref name) => {
            let (casted_value, _) = maybe_cast_representation(
                value.clone(),
                value_type,
                &pattern.expr_type,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
            );

            let value_name = get_ident_name(name);
            let value_ctype = type_to_ctype(&pattern.expr_type);

            let wrapper_name = get_expr_name(&format!("{}_wrapper", name), expr_index);
            let wrapper_type = CType::Ref(Box::new(value_ctype.clone()));

            declarations.push(CDeclaration(wrapper_type.clone(), wrapper_name.clone()));
            statements.push(
                pattern.replace_untyped(CStatement::RefAlloc(wrapper_name.clone(), value_ctype)),
            );
            statements.push(pattern.replace_untyped(CStatement::RefAssign(
                wrapper_name.clone(),
                casted_value.to_c_expr(),
            )));
            statements.push(pattern.replace_untyped(CStatement::RefAssign(
                value_name,
                pattern.replace_untyped(CExpr::Value(CValue::Ident(wrapper_name, wrapper_type))),
            )));
        }
        &TypedPattern::Tuple(ref vec) => for (index, element) in vec.iter().enumerate() {
            let inner_value_type =
                if let &EvaluatedType::Tuple(ref value_element_types) = value_type {
                    *value_element_types[index].expr.clone()
                } else {
                    panic!("Cannot unpack non-tuple `{}`", value_type)
                };
            let inner_value = value.replace_expr(CExpr::ObjectAccess {
                object: value.clone(),
                index,
                field_type: type_to_ctype(&inner_value_type),
            });
            let inner_value_name = get_expr_name("unpacked", expr_index);
            let inner_value_ctype = type_to_ctype(&inner_value_type);
            declarations.push(CDeclaration(
                inner_value_ctype.clone(),
                inner_value_name.clone(),
            ));
            statements.push(
                value.replace_expr(CStatement::VarAssign(inner_value_name.clone(), inner_value)),
            );

            bind_declarations(
                element,
                &value.replace_expr(CValue::Ident(inner_value_name, inner_value_ctype)),
                &inner_value_type,
                declarations,
                statements,
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
    statements: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut i32,
) -> Ast<CValue> {
    match &*ast.expr {
        &TypedExpression::Value(ref v) => match v {
            &TypedValue::Num(value) => ast.replace_untyped(CValue::Float(value)),
            &TypedValue::Bool(value) => ast.replace_untyped(CValue::Bool(value)),
            &TypedValue::Int(value) => ast.replace_untyped(CValue::Int(value)),
            &TypedValue::Func(ref pattern, ref declared_body_type, ref body) => {
                let mut captures_map = HashMap::new();
                let mut locals = HashSet::new();
                find_names_in_pattern(pattern, &mut locals);
                find_captures(body, &locals, &mut captures_map);

                let func_name = get_func_name(function_index);
                let mut func_declarations = Vec::new();
                let mut func_statements = Vec::new();
                let mut func_expr_index = 0;

                let param_ctype = type_to_ctype(&pattern.expr_type);
                let param_value = pattern
                    .replace_untyped(CValue::Ident(String::from("_aro_arg"), param_ctype.clone()));
                make_declarations(pattern, &mut func_declarations, &mut func_statements);
                bind_declarations(
                    pattern,
                    &param_value,
                    &pattern.expr_type,
                    &mut func_declarations,
                    &mut func_statements,
                    &mut func_expr_index,
                    functions,
                    function_index,
                );

                for (index, (name, var_type)) in captures_map.iter().enumerate() {
                    let ident_name = get_ident_name(name);
                    let ident_type = CType::Ref(Box::new(CType::Ref(Box::new(var_type.clone()))));
                    func_declarations.push(CDeclaration(ident_type.clone(), ident_name.clone()));

                    let captures_object = ast.replace_untyped(CValue::Ident(
                        String::from("_aro_captures"),
                        CType::Object,
                    ));
                    func_statements.push(ast.replace_untyped(CStatement::VarAssign(
                        ident_name,
                        ast.replace_untyped(CExpr::ObjectAccess {
                            object: captures_object,
                            index,
                            field_type: ident_type,
                        }),
                    )))
                }

                let ret = lift_expr(
                    body,
                    &mut func_declarations,
                    &mut func_statements,
                    &mut func_expr_index,
                    functions,
                    function_index,
                );

                let (ret, _) = maybe_cast_representation(
                    ret,
                    &body.expr_type,
                    &declared_body_type.expr,
                    &mut func_declarations,
                    &mut func_statements,
                    &mut func_expr_index,
                    functions,
                    function_index,
                );

                let closure_name = get_expr_name("closure", expr_index);
                let closure_type = type_to_ctype(&ast.expr_type);
                declarations.push(CDeclaration(closure_type.clone(), closure_name.clone()));
                statements.push(
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
                    declarations: func_declarations,
                    body: func_statements,
                    ret,
                }));

                ast.replace_untyped(CValue::Ident(closure_name, closure_type))
            }
            &TypedValue::Tuple(ref vec) => {
                let mut data = Vec::new();
                for element in vec {
                    data.push(lift_expr(
                        element,
                        declarations,
                        statements,
                        expr_index,
                        functions,
                        function_index,
                    ));
                }

                let expr_name = get_expr_name("tuple", expr_index);
                declarations.push(CDeclaration(CType::Object, expr_name.clone()));
                statements.push(ast.replace_untyped(CStatement::ObjectInit {
                    name: expr_name.clone(),
                    data,
                }));

                ast.replace_untyped(CValue::Ident(expr_name, CType::Object))
            }
            _ => panic!(),
        },
        &TypedExpression::Ident(ref name) => ast.replace_untyped(CValue::DerefBound(
            get_ident_name(name),
            type_to_ctype(&ast.expr_type),
        )),
        &TypedExpression::BinOp(ref op, ref left, ref right) => {
            let left_c_ast = lift_expr(
                left,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
            );
            let right_c_ast = {
                let right_c_ast = lift_expr(
                    right,
                    declarations,
                    statements,
                    expr_index,
                    functions,
                    function_index,
                );
                match op {
                    &BinOp::Call => {
                        if let &EvaluatedType::Func(ref param_type, _) = &*left.expr_type {
                            let (casted_ast, _) = maybe_cast_representation(
                                right_c_ast,
                                &right.expr_type,
                                &param_type.expr,
                                declarations,
                                statements,
                                expr_index,
                                functions,
                                function_index,
                            );

                            casted_ast
                        } else {
                            panic!("Calling non-function `{}`", left.expr_type)
                        }
                    }
                    _ => right_c_ast,
                }
            };

            let result_name = get_expr_name("op_result", expr_index);
            let result_type = type_to_ctype(&ast.expr_type);
            declarations.push(CDeclaration(result_type.clone(), result_name.clone()));
            statements.push(ast.replace_untyped(CStatement::VarAssign(
                result_name.clone(),
                ast.replace_untyped(CExpr::BinOp(
                    op.clone(),
                    left_c_ast.clone(),
                    right_c_ast,
                    type_to_ctype(&ast.expr_type),
                )),
            )));
            let result = ast.replace_untyped(CValue::Ident(result_name, result_type));

            match op {
                &BinOp::Call => {
                    if let &EvaluatedType::Func(_, ref ret_type) = &*left.expr_type {
                        let (casted, _) = maybe_cast_representation(
                            result,
                            &ret_type.expr,
                            &ast.expr_type,
                            declarations,
                            statements,
                            expr_index,
                            functions,
                            function_index,
                        );
                        casted
                    } else {
                        panic!("Calling non-function `{}`", left.expr_type)
                    }
                }
                _ => result,
            }
        }
        &TypedExpression::If(ref condition, ref consequent, ref alternate) => {
            let condition_c_ast = lift_expr(
                condition,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
            );

            let expr_name = get_expr_name("if_result", expr_index);
            let expr_type = type_to_ctype(&ast.expr_type);

            let mut consequent_declarations = Vec::new();
            let mut consequent_statements = Vec::new();
            let consequent_c_ast = lift_expr(
                consequent,
                &mut consequent_declarations,
                &mut consequent_statements,
                expr_index,
                functions,
                function_index,
            );
            consequent_statements.push(ast.replace_untyped(CStatement::VarAssign(
                expr_name.clone(),
                consequent_c_ast.to_c_expr(),
            )));
            let consequent_block = consequent.replace_untyped(CStatement::Block(
                consequent_declarations,
                consequent_statements,
            ));

            let mut alternate_declarations = Vec::new();
            let mut alternate_statements = Vec::new();
            let alternate_c_ast = lift_expr(
                alternate,
                &mut alternate_declarations,
                &mut alternate_statements,
                expr_index,
                functions,
                function_index,
            );
            alternate_statements.push(ast.replace_untyped(CStatement::VarAssign(
                expr_name.clone(),
                alternate_c_ast.to_c_expr(),
            )));
            let alternate_block = alternate.replace_untyped(CStatement::Block(
                alternate_declarations,
                alternate_statements,
            ));

            declarations.push(CDeclaration(expr_type.clone(), expr_name.clone()));
            statements.push(ast.replace_untyped(CStatement::If(
                condition_c_ast,
                consequent_block,
                alternate_block,
            )));

            ast.replace_untyped(CValue::Ident(expr_name, expr_type))
        }
        &TypedExpression::Let(ref pattern, ref value, ref body) => {
            let body_name = get_expr_name("let_body", expr_index);
            let body_type = type_to_ctype(&body.expr_type);
            declarations.push(CDeclaration(body_type.clone(), body_name.clone()));

            let mut body_declarations = Vec::new();
            let mut body_statements = Vec::new();
            make_declarations(pattern, &mut body_declarations, &mut body_statements);
            let value_c_ast = lift_expr(
                value,
                &mut body_declarations,
                &mut body_statements,
                expr_index,
                functions,
                function_index,
            );
            bind_declarations(
                pattern,
                &value_c_ast,
                &value.expr_type,
                &mut body_declarations,
                &mut body_statements,
                expr_index,
                functions,
                function_index,
            );

            let body_c_ast = lift_expr(
                body,
                &mut body_declarations,
                &mut body_statements,
                expr_index,
                functions,
                function_index,
            );
            body_statements.push(body.replace_untyped(CStatement::VarAssign(
                body_name.clone(),
                body_c_ast.to_c_expr(),
            )));

            statements
                .push(ast.replace_untyped(CStatement::Block(body_declarations, body_statements)));

            ast.replace_untyped(CValue::Ident(body_name, body_type))
        }
        &TypedExpression::Cast(ref from, ref to_type) => {
            let from_cvalue = lift_expr(
                from,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
            );
            let (casted, _) = maybe_cast_representation(
                from_cvalue,
                &from.expr_type,
                &to_type.expr,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
            );

            casted
        }
        _ => panic!(),
    }
}

#[cfg(test)]
mod lift_expr {
    use super::*;
    use parse::source_to_ast;
    use std::collections::HashMap;
    use typecheck::typecheck_ast;

    fn assert_lift(
        aro_code: &str,
        expected_statements: &str,
        expected_functions: &str,
        expected_expr: &str,
    ) {
        let ast = source_to_ast(aro_code).unwrap();
        let typechecked_ast = typecheck_ast(&ast, &HashMap::new()).unwrap();

        let mut declarations = Vec::new();
        let mut statements = Vec::new();
        let mut expr_index = 0;
        let mut functions = Vec::new();
        let mut function_index = 0;
        let actual_expr = lift_expr(
            &typechecked_ast,
            &mut declarations,
            &mut statements,
            &mut expr_index,
            &mut functions,
            &mut function_index,
        );

        let mut actual_statements = String::new();
        for statement in declarations {
            actual_statements += &format!("{} ", statement);
        }
        for statement in statements {
            actual_statements += &format!("{} ", statement);
        }

        let mut actual_functions = String::new();
        for function in functions {
            actual_functions += &format!("{} ", function);
        }

        assert_eq!(actual_statements, expected_statements);
        assert_eq!(actual_functions, expected_functions);
        assert_eq!(format!("{}", actual_expr), expected_expr);
    }

    #[test]
    fn handles_arithmatic() {
        assert_lift(
            "1 + 1",
            "int _aro_expr_op_result_0; \
             _aro_expr_op_result_0 = (1 + 1); ",
            "",
            "_aro_expr_op_result_0",
        );
        assert_lift(
            "1.0 + 1.0",
            "double _aro_expr_op_result_0; \
             _aro_expr_op_result_0 = (1 + 1); ",
            "",
            "_aro_expr_op_result_0",
        );
        assert_lift(
            "1.1 - 1",
            "double _aro_expr_op_result_0; \
             _aro_expr_op_result_0 = (1.1 - 1); ",
            "",
            "_aro_expr_op_result_0",
        );
        assert_lift(
            "1 * 1.1",
            "double _aro_expr_op_result_0; \
             _aro_expr_op_result_0 = (1 * 1.1); ",
            "",
            "_aro_expr_op_result_0",
        );
        assert_lift(
            "1.3 / 1.1",
            "double _aro_expr_op_result_0; \
             _aro_expr_op_result_0 = ((double ) 1.3 / 1.1); ",
            "",
            "_aro_expr_op_result_0",
        );
        assert_lift(
            "1 / 1",
            "double _aro_expr_op_result_0; \
             _aro_expr_op_result_0 = ((double ) 1 / 1); ",
            "",
            "_aro_expr_op_result_0",
        );
    }

    #[test]
    fn handles_comparison() {
        assert_lift(
            "1.0 <= 1.0",
            "bool _aro_expr_op_result_0; \
             _aro_expr_op_result_0 = (1 <= 1); ",
            "",
            "_aro_expr_op_result_0",
        );
    }

    #[test]
    fn supports_nested_operations() {
        assert_lift(
            "1 - 1 <= 1.3 + 8 * 3",
            "int _aro_expr_op_result_0; \
             int _aro_expr_op_result_1; \
             double _aro_expr_op_result_2; \
             bool _aro_expr_op_result_3; \
             _aro_expr_op_result_0 = (1 - 1); \
             _aro_expr_op_result_1 = (8 * 3); \
             _aro_expr_op_result_2 = (1.3 + _aro_expr_op_result_1); \
             _aro_expr_op_result_3 = (_aro_expr_op_result_0 <= _aro_expr_op_result_2); ",
            "",
            "_aro_expr_op_result_3",
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
            "bool _aro_expr_op_result_0; \
             double _aro_expr_if_result_1; \
             _aro_expr_op_result_0 = (1 <= 2); \
             if (_aro_expr_op_result_0) {  \
             _aro_expr_if_result_1 = 5.3; \
             } else { \
             int _aro_expr_op_result_2; \
             _aro_expr_op_result_2 = (8 * 2); \
             _aro_expr_if_result_1 = _aro_expr_op_result_2; \
             } ",
            "",
            "_aro_expr_if_result_1",
        );
    }

    #[test]
    fn handles_let_expressions() {
        assert_lift(
            "
            let x: Num <- 5 + 2
            x + 3
            ",
            "double _aro_expr_let_body_0; \
             { \
             double * aro_x; \
             int _aro_expr_op_result_1; \
             double _aro_expr_casted_2; \
             double _aro_expr_op_result_3; \
             aro_x = malloc(sizeof(double )); \
             _aro_expr_op_result_1 = (5 + 2); \
             _aro_expr_casted_2 = ((double )_aro_expr_op_result_1); \
             *aro_x = _aro_expr_casted_2; \
             _aro_expr_op_result_3 = ((*aro_x) + 3); \
             _aro_expr_let_body_0 = _aro_expr_op_result_3; \
             } ",
            "",
            "_aro_expr_let_body_0",
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
            "int _aro_expr_let_body_0; \
             { \
             double * aro_x; \
             double _aro_expr_casted_1; \
             int _aro_expr_let_body_2; \
             aro_x = malloc(sizeof(double )); \
             _aro_expr_casted_1 = ((double )5); \
             *aro_x = _aro_expr_casted_1; \
             { \
             int * aro_x; \
             int _aro_expr_op_result_3; \
             aro_x = malloc(sizeof(int )); \
             *aro_x = 4; \
             _aro_expr_op_result_3 = ((*aro_x) + 3); \
             _aro_expr_let_body_2 = _aro_expr_op_result_3; \
             } \
             _aro_expr_let_body_0 = _aro_expr_let_body_2; \
             } ",
            "",
            "_aro_expr_let_body_0",
        );
    }

    #[test]
    fn handles_functions() {
        assert_lift(
            "2 |> (fn x: Int =Int=> x + 1)",
            "_Aro_Closure _aro_expr_closure_0; \
             int _aro_expr_op_result_1; \
             _aro_expr_closure_0 = malloc(sizeof(_Aro_Any) * 1); \
             _aro_expr_closure_0[0].Void_Ptr = _aro_func_0;  \
             _aro_expr_op_result_1 = (*(int (*)(int , _Aro_Object ))_aro_expr_closure_0[0].Void_Ptr)\
             (2, &_aro_expr_closure_0[1]); ",
            "int _aro_func_0(int _aro_arg, _Aro_Object _aro_captures) { \
             int * aro_x; \
             int _aro_expr_op_result_0; \
             aro_x = malloc(sizeof(int )); \
             *aro_x = _aro_arg; \
             _aro_expr_op_result_0 = ((*aro_x) + 1); \
             return _aro_expr_op_result_0; \
             } ",
            "_aro_expr_op_result_1",
        );
    }

    #[test]
    fn unpacks_tuples() {
        assert_lift(
            "
            let (a: Int  b: Num) <- (1 2.3)
            a + b
            ",
            "double _aro_expr_let_body_0; \
             { \
             int * aro_a; \
             double * aro_b; \
             _Aro_Object _aro_expr_tuple_1; \
             int _aro_expr_unpacked_2; \
             double _aro_expr_unpacked_3; \
             double _aro_expr_op_result_4; \
             aro_a = malloc(sizeof(int )); \
             aro_b = malloc(sizeof(double )); \
             _aro_expr_tuple_1 = malloc(sizeof(_Aro_Any) * 2); \
             _aro_expr_tuple_1[0].Int = 1; \
             _aro_expr_tuple_1[1].Float = 2.3; \
             _aro_expr_unpacked_2 = (_aro_expr_tuple_1[0].Int); \
             *aro_a = _aro_expr_unpacked_2; \
             _aro_expr_unpacked_3 = (_aro_expr_tuple_1[1].Float); \
             *aro_b = _aro_expr_unpacked_3; \
             _aro_expr_op_result_4 = ((*aro_a) + (*aro_b)); \
             _aro_expr_let_body_0 = _aro_expr_op_result_4; \
             } ",
            "",
            "_aro_expr_let_body_0",
        );
        assert_lift(
            "
            let my_tuple: (Int Num) <- (1 2.3)
            let (a: Num  b: Num) <- my_tuple
            a + b
            ",
            "double _aro_expr_let_body_0; \
             { \
             _Aro_Object * aro_my_tuple; \
             _Aro_Object _aro_expr_tuple_1; \
             double _aro_expr_let_body_4; \
             aro_my_tuple = malloc(sizeof(_Aro_Object )); \
             _aro_expr_tuple_1 = malloc(sizeof(_Aro_Any) * 2); \
             _aro_expr_tuple_1[0].Int = 1; \
             _aro_expr_tuple_1[1].Float = 2.3; \
             *aro_my_tuple = _aro_expr_tuple_1; \
             { \
             double * aro_a; \
             double * aro_b; \
             int _aro_expr_unpacked_5; \
             double _aro_expr_casted_6; \
             double _aro_expr_unpacked_7; \
             double _aro_expr_op_result_8; \
             aro_a = malloc(sizeof(double )); \
             aro_b = malloc(sizeof(double )); \
             _aro_expr_unpacked_5 = ((*aro_my_tuple)[0].Int); \
             _aro_expr_casted_6 = ((double )_aro_expr_unpacked_5); \
             *aro_a = _aro_expr_casted_6; \
             _aro_expr_unpacked_7 = ((*aro_my_tuple)[1].Float); \
             *aro_b = _aro_expr_unpacked_7; \
             _aro_expr_op_result_8 = ((*aro_a) + (*aro_b)); \
             _aro_expr_let_body_4 = _aro_expr_op_result_8; \
             } \
             _aro_expr_let_body_0 = _aro_expr_let_body_4; \
             } ",
            "",
            "_aro_expr_let_body_0",
        );
        assert_lift(
            "
            (1 2.3) |> (fn (a: Int  b: Num) =Num=> a + b)
            ",
            "_Aro_Closure _aro_expr_closure_0; \
             _Aro_Object _aro_expr_tuple_1; \
             double _aro_expr_op_result_4; \
             _aro_expr_closure_0 = malloc(sizeof(_Aro_Any) * 1); \
             _aro_expr_closure_0[0].Void_Ptr = _aro_func_0;  \
             _aro_expr_tuple_1 = malloc(sizeof(_Aro_Any) * 2); \
             _aro_expr_tuple_1[0].Int = 1; \
             _aro_expr_tuple_1[1].Float = 2.3; \
             _aro_expr_op_result_4 = (*(double (*)(_Aro_Object , _Aro_Object ))\
             _aro_expr_closure_0[0].Void_Ptr)(_aro_expr_tuple_1, &_aro_expr_closure_0[1]); ",
            "double _aro_func_0(_Aro_Object _aro_arg, _Aro_Object _aro_captures) { \
             int * aro_a; \
             double * aro_b; \
             int _aro_expr_unpacked_0; \
             double _aro_expr_unpacked_1; \
             double _aro_expr_op_result_2; \
             aro_a = malloc(sizeof(int )); \
             aro_b = malloc(sizeof(double )); \
             _aro_expr_unpacked_0 = (_aro_arg[0].Int); \
             *aro_a = _aro_expr_unpacked_0; \
             _aro_expr_unpacked_1 = (_aro_arg[1].Float); \
             *aro_b = _aro_expr_unpacked_1; \
             _aro_expr_op_result_2 = ((*aro_a) + (*aro_b)); \
             return _aro_expr_op_result_2; \
             } ",
            "_aro_expr_op_result_4",
        );
    }
}
