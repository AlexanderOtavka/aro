use c_ast::{CDeclaration, CExpr, CFunc, CName, CStatement, CType, CValue};
use std::collections::{HashMap, HashSet};
use typed_ast::{EvaluatedType, TypedAst, TypedExpression, TypedPattern, TypedValue};
use untyped_ast::Ast;

fn get_expr_name(name: &str, expr_index: &mut u64) -> CName {
    let old_i = *expr_index;
    *expr_index += 1;
    CName::Expr(String::from(name), old_i)
}

fn get_func_name(func_index: &mut u64) -> CName {
    let old_i = *func_index;
    *func_index += 1;
    CName::Func(old_i)
}

fn get_adaptor_func_name(func_index: &mut u64) -> CName {
    let old_i = *func_index;
    *func_index += 1;
    CName::AdaptorFunc(old_i)
}

fn get_ident_name(name: &str) -> CName {
    CName::Ident(String::from(name))
}

pub fn type_to_ctype(t: &EvaluatedType) -> CType {
    match t {
        &EvaluatedType::Any | &EvaluatedType::None => CType::Any,
        &EvaluatedType::Num => CType::Float,
        &EvaluatedType::Int => CType::Int,
        &EvaluatedType::Bool => CType::Bool,
        &EvaluatedType::Func(ref param, ref ret) => CType::Closure {
            param: param.replace_expr(type_to_ctype(&param.expr)),
            ret: ret.replace_expr(type_to_ctype(&ret.expr)),
        },
        &EvaluatedType::Tuple(_) | &EvaluatedType::Record(_) | &EvaluatedType::List(_) => {
            CType::Object
        }
        &EvaluatedType::GenericFunc { ref output, .. } => type_to_ctype(&output.expr),
        &EvaluatedType::Ident(_, ref supertype) => type_to_ctype(&supertype.expr),
        &EvaluatedType::Ref(_) => CType::Ref(Box::new(CType::Any)),
    }
}

fn maybe_cast_representation(
    from: Ast<CValue>,
    from_type: &EvaluatedType,
    to_type: &EvaluatedType,
    declarations: &mut Vec<CDeclaration>,
    statements: &mut Vec<Ast<CStatement>>,
    expr_index: &mut u64,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut u64,
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
            statements
                .push(from.replace_expr(CStatement::AnyAssign(expr_name.clone(), from.clone())));
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

            let arg = to_in.replace_expr(CValue::Ident(CName::FuncArg, type_to_ctype(&to_in.expr)));
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
                    object: from.replace_expr(CValue::Ident(CName::FuncCaptures, CType::Object)),
                    field_type: inner_func_ctype.clone(),
                }),
            )));

            let ret = from_out.replace_expr(CExpr::Call(
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
        &TypedExpression::BinOp(_, ref a, ref b)
        | &TypedExpression::Call(ref a, ref b)
        | &TypedExpression::Sequence(ref a, ref b)
        | &TypedExpression::RefSet(ref a, ref b) => {
            find_captures(a, locals, captures);
            find_captures(b, locals, captures);
        }
        &TypedExpression::RecordAccess(ref x, _)
        | &TypedExpression::Cast(ref x, _)
        | &TypedExpression::RefNew(ref x)
        | &TypedExpression::RefGet(ref x) => {
            find_captures(x, locals, captures);
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
                pattern.replace_untyped(CExpr::Value(CValue::Null)),
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
    expr_index: &mut u64,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut u64,
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

            let wrapper_name = get_expr_name("wrapper", expr_index);
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

pub fn bind_global(
    name: &str,
    value: &Ast<CValue>,
    value_type: &EvaluatedType,
    declarations: &mut Vec<CDeclaration>,
    statements: &mut Vec<Ast<CStatement>>,
    expr_index: &mut u64,
) {
    let value_name = get_ident_name(name);
    let value_ctype = type_to_ctype(value_type);

    let wrapper_name = get_expr_name("wrapper", expr_index);
    let wrapper_type = CType::Ref(Box::new(value_ctype.clone()));

    declarations.push(CDeclaration(wrapper_type.clone(), wrapper_name.clone()));
    statements.push(value.replace_expr(CStatement::RefAlloc(wrapper_name.clone(), value_ctype)));
    statements.push(value.replace_expr(CStatement::RefAssign(
        wrapper_name.clone(),
        value.to_c_expr(),
    )));
    statements.push(value.replace_expr(CStatement::RefAlloc(
        value_name.clone(),
        wrapper_type.clone(),
    )));
    statements.push(value.replace_expr(CStatement::RefAssign(
        value_name,
        value.replace_expr(CExpr::Value(CValue::Ident(wrapper_name, wrapper_type))),
    )));
}

pub fn layout_records(real_types: Vec<EvaluatedType>) -> HashMap<usize, HashMap<String, usize>> {
    let record_map = real_types.into_iter().enumerate().filter_map(
        |(id, real_type)| match real_type {
            EvaluatedType::Record(map) => Some((id, map)),
            _ => None,
        },
    );

    let mut fields = HashSet::new();
    for (_, record) in record_map.clone() {
        for (field, _) in record {
            fields.insert(field);
        }
    }

    record_map
        .map(|(id, record)| {
            let mut layout = HashMap::new();
            for (field, _) in record {
                layout.insert(
                    field.clone(),
                    fields
                        .iter()
                        .position(|known_field| known_field == &field)
                        .unwrap(),
                );
            }

            (id, layout)
        })
        .collect()
}

pub fn lift_expr(
    ast: &TypedAst<TypedExpression>,
    declarations: &mut Vec<CDeclaration>,
    statements: &mut Vec<Ast<CStatement>>,
    expr_index: &mut u64,
    functions: &mut Vec<Ast<CFunc>>,
    function_index: &mut u64,
    externs: &mut Vec<CDeclaration>,
    real_types: &Vec<EvaluatedType>,
    record_layouts: &HashMap<usize, HashMap<String, usize>>,
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
                let param_value =
                    pattern.replace_untyped(CValue::Ident(CName::FuncArg, param_ctype.clone()));
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

                    let captures_object =
                        ast.replace_untyped(CValue::Ident(CName::FuncCaptures, CType::Object));
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
                    externs,
                    real_types,
                    record_layouts,
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
                        externs,
                        real_types,
                        record_layouts,
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
            &TypedValue::Hook(ref names) => {
                let ctype = type_to_ctype(&ast.expr_type);
                let name = CName::Hook(names.clone());
                externs.push(CDeclaration(ctype.clone(), name.clone()));
                ast.replace_untyped(CValue::Ident(name, ctype))
            }
            &TypedValue::List(ref elements) => {
                let mut head = CValue::Null;

                let element_supertype =
                    if let &EvaluatedType::List(ref element_type_ast) = &*ast.expr_type {
                        &*element_type_ast.expr
                    } else {
                        panic!("Bad typing of list")
                    };

                for element in elements.into_iter().rev() {
                    let element_value = lift_expr(
                        element,
                        declarations,
                        statements,
                        expr_index,
                        functions,
                        function_index,
                        externs,
                        real_types,
                        record_layouts,
                    );

                    let (element_value, _) = maybe_cast_representation(
                        element_value,
                        &element.expr_type,
                        element_supertype,
                        declarations,
                        statements,
                        expr_index,
                        functions,
                        function_index,
                    );

                    let cons_name = get_expr_name("cons", expr_index);
                    let cons_type = CType::Object;
                    declarations.push(CDeclaration(cons_type.clone(), cons_name.clone()));
                    statements.push(element.replace_untyped(CStatement::ObjectInit {
                        name: cons_name.clone(),
                        data: vec![element_value, element.replace_untyped(head)],
                    }));

                    head = CValue::Ident(cons_name, cons_type);
                }

                ast.replace_untyped(head)
            }
            &TypedValue::Record(ref map) => {
                let type_index = real_types
                    .into_iter()
                    .position(|real_type| real_type == &*ast.expr_type)
                    .unwrap();
                let layout = record_layouts.get(&type_index).unwrap();

                let mut data = Vec::new();
                for (field, field_value) in map {
                    let field_position = *layout.get(field).unwrap();
                    if field_position >= data.len() {
                        data.resize(field_position + 1, ast.replace_untyped(CValue::Null));
                    }

                    data.remove(field_position);
                    data.insert(
                        field_position,
                        lift_expr(
                            field_value,
                            declarations,
                            statements,
                            expr_index,
                            functions,
                            function_index,
                            externs,
                            real_types,
                            record_layouts,
                        ),
                    );
                }

                let expr_name = get_expr_name("record", expr_index);
                declarations.push(CDeclaration(CType::Object, expr_name.clone()));
                statements.push(ast.replace_untyped(CStatement::ObjectInit {
                    name: expr_name.clone(),
                    data,
                }));

                ast.replace_untyped(CValue::Ident(expr_name, CType::Object))
            }
        },
        &TypedExpression::Ident(ref name) => ast.replace_untyped(CValue::DerefBound(
            get_ident_name(name),
            type_to_ctype(&ast.expr_type),
        )),
        &TypedExpression::Call(ref left, ref right) => {
            let left_c_ast = lift_expr(
                left,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );

            let right_c_ast = lift_expr(
                right,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );
            let right_c_ast = if let &EvaluatedType::Func(ref param_type, _) = &*left.expr_type {
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
            };

            let result_name = get_expr_name("op_result", expr_index);
            let result_type = type_to_ctype(&ast.expr_type);
            declarations.push(CDeclaration(result_type.clone(), result_name.clone()));
            statements.push(ast.replace_untyped(CStatement::VarAssign(
                result_name.clone(),
                ast.replace_untyped(CExpr::Call(
                    left_c_ast.clone(),
                    right_c_ast,
                    type_to_ctype(&ast.expr_type),
                )),
            )));
            let result = ast.replace_untyped(CValue::Ident(result_name, result_type));

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
        &TypedExpression::BinOp(ref op, ref left, ref right) => {
            let left_c_ast = lift_expr(
                left,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );
            let right_c_ast = lift_expr(
                right,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );

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

            ast.replace_untyped(CValue::Ident(result_name, result_type))
        }
        &TypedExpression::If(ref condition, ref consequent, ref alternate) => {
            let condition_c_ast = lift_expr(
                condition,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
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
                externs,
                real_types,
                record_layouts,
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
                externs,
                real_types,
                record_layouts,
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
                externs,
                real_types,
                record_layouts,
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
                externs,
                real_types,
                record_layouts,
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
                externs,
                real_types,
                record_layouts,
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
        &TypedExpression::Sequence(ref side_effect, ref result) => {
            lift_expr(
                side_effect,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );
            lift_expr(
                result,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            )
        }
        &TypedExpression::RefNew(ref value) => {
            let value = lift_expr(
                value,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );

            let ref_name = get_expr_name("ref", expr_index);
            let ref_type = type_to_ctype(&ast.expr_type);

            declarations.push(CDeclaration(ref_type.clone(), ref_name.clone()));
            statements
                .push(ast.replace_untyped(CStatement::RefAlloc(ref_name.clone(), CType::Any)));
            statements.push(ast.replace_untyped(CStatement::AnyRefAssign(
                ast.replace_untyped(CValue::Ident(ref_name.clone(), ref_type.clone())),
                value,
            )));

            ast.replace_untyped(CValue::Ident(ref_name, ref_type))
        }
        &TypedExpression::RefGet(ref reference) => {
            let reference = lift_expr(
                reference,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );

            let value_name = get_expr_name("ref_value", expr_index);
            let value_type = type_to_ctype(&ast.expr_type);

            declarations.push(CDeclaration(value_type.clone(), value_name.clone()));
            statements.push(ast.replace_untyped(CStatement::VarAssign(
                value_name.clone(),
                ast.replace_untyped(CExpr::AnyRefGet(reference, value_type.clone())),
            )));

            ast.replace_untyped(CValue::Ident(value_name, value_type))
        }
        &TypedExpression::RefSet(ref reference, ref value) => {
            let reference = lift_expr(
                reference,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );
            let value = lift_expr(
                value,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );

            statements
                .push(ast.replace_untyped(CStatement::AnyRefAssign(reference, value.clone())));

            value
        }
        &TypedExpression::RecordAccess(ref record, ref field) => {
            let record_cvalue = lift_expr(
                record,
                declarations,
                statements,
                expr_index,
                functions,
                function_index,
                externs,
                real_types,
                record_layouts,
            );

            let real_subtype_index = real_types
                .into_iter()
                .position(|real_type| real_type.is_sub_type(&record.expr_type))
                .expect(&format!(
                    "There is nothing that could be a subtype of `{}`",
                    &record.expr_type,
                ));

            // TODO: add an assertion to make sure all real subtypes have the field in the same place

            let access_position = *record_layouts
                .get(&real_subtype_index)
                .unwrap()
                .get(field)
                .unwrap();

            let value_name = get_expr_name("record_access", expr_index);
            let value_type = type_to_ctype(&ast.expr_type);

            declarations.push(CDeclaration(value_type.clone(), value_name.clone()));
            statements.push(ast.replace_untyped(CStatement::VarAssign(
                value_name.clone(),
                ast.replace_untyped(CExpr::ObjectAccess {
                    object: record_cvalue,
                    index: access_position,
                    field_type: type_to_ctype(&ast.expr_type),
                }),
            )));

            ast.replace_untyped(CValue::Ident(value_name, value_type))
        }
    }
}

pub fn print_value(
    value_ast: Ast<CValue>,
    value_type: &EvaluatedType,
    declarations: &mut Vec<CDeclaration>,
    statements: &mut Vec<Ast<CStatement>>,
    expr_index: &mut u64,
    real_types: &Vec<EvaluatedType>,
    record_layouts: &HashMap<usize, HashMap<String, usize>>,
) {
    match value_type {
        EvaluatedType::Bool
        | EvaluatedType::Int
        | EvaluatedType::Num
        | EvaluatedType::None
        | EvaluatedType::Any
        | EvaluatedType::Func(_, _) => {
            statements.push(value_ast.replace_expr(CStatement::PrintValue(value_ast.clone())));
        }
        EvaluatedType::Ident(_, ref value_type_ast)
        | EvaluatedType::GenericFunc {
            substituted_output: ref value_type_ast,
            ..
        } => {
            print_value(
                value_ast,
                &value_type_ast.expr,
                declarations,
                statements,
                expr_index,
                real_types,
                record_layouts,
            );
        }
        EvaluatedType::Tuple(ref element_types) => {
            statements.push(value_ast.replace_expr(CStatement::PrintText(String::from("("))));

            for (index, element_type) in element_types.into_iter().enumerate() {
                let element_name = get_expr_name("tuple_element", expr_index);
                let element_ctype = type_to_ctype(&element_type.expr);
                declarations.push(CDeclaration(element_ctype.clone(), element_name.clone()));
                statements.push(value_ast.replace_expr(CStatement::VarAssign(
                    element_name.clone(),
                    value_ast.replace_expr(CExpr::ObjectAccess {
                        object: value_ast.clone(),
                        index,
                        field_type: element_ctype.clone(),
                    }),
                )));

                print_value(
                    value_ast.replace_expr(CValue::Ident(element_name, element_ctype)),
                    &element_type.expr,
                    declarations,
                    statements,
                    expr_index,
                    real_types,
                    record_layouts,
                );

                if index < element_types.len() - 1 {
                    statements
                        .push(value_ast.replace_expr(CStatement::PrintText(String::from(" "))));
                }
            }

            statements.push(value_ast.replace_expr(CStatement::PrintText(String::from(")"))));
        }
        EvaluatedType::Record(ref type_map) => {
            statements.push(value_ast.replace_expr(CStatement::PrintText(String::from("{"))));

            let mut is_first = true;

            for (field, field_type) in type_map {
                if is_first {
                    is_first = false;
                } else {
                    statements
                        .push(value_ast.replace_expr(CStatement::PrintText(String::from("  "))));
                }

                let index = *real_types
                    .into_iter()
                    .enumerate()
                    .filter_map(|(index, real_type)| {
                        if real_type.is_sub_type(value_type) {
                            record_layouts.get(&index).unwrap().get(field)
                        } else {
                            None
                        }
                    })
                    .next()
                    .unwrap();

                let value_name = get_expr_name("record_field", expr_index);
                let value_ctype = type_to_ctype(&field_type.expr);
                declarations.push(CDeclaration(value_ctype.clone(), value_name.clone()));
                statements.push(value_ast.replace_expr(CStatement::VarAssign(
                    value_name.clone(),
                    value_ast.replace_expr(CExpr::ObjectAccess {
                        object: value_ast.clone(),
                        index,
                        field_type: value_ctype.clone(),
                    }),
                )));

                statements
                    .push(value_ast.replace_expr(CStatement::PrintText(format!("{} <- ", field))));

                print_value(
                    value_ast.replace_expr(CValue::Ident(value_name, value_ctype)),
                    &field_type.expr,
                    declarations,
                    statements,
                    expr_index,
                    real_types,
                    record_layouts,
                );
            }

            statements.push(value_ast.replace_expr(CStatement::PrintText(String::from("}"))));
        }
        EvaluatedType::List(ref element_type) => {
            statements.push(value_ast.replace_expr(CStatement::PrintText(String::from("["))));

            let iteration_var_name = get_expr_name("iteration_var", expr_index);
            let iteration_var_type = CType::Object;
            declarations.push(CDeclaration(
                iteration_var_type.clone(),
                iteration_var_name.clone(),
            ));
            statements.push(value_ast.replace_expr(CStatement::VarAssign(
                iteration_var_name.clone(),
                value_ast.to_c_expr(),
            )));

            let iteration_var = value_ast.replace_expr(CValue::Ident(
                iteration_var_name.clone(),
                iteration_var_type.clone(),
            ));

            let has_next_cond_expr =
                value_ast.replace_expr(CExpr::Not(value_ast.replace_expr(CExpr::Call(
                    value_ast.replace_expr(CValue::DerefBound(
                        get_ident_name("is_empty"),
                        CType::Closure {
                            param: value_ast.replace_expr(CType::Object),
                            ret: value_ast.replace_expr(CType::Bool),
                        },
                    )),
                    iteration_var.clone(),
                    CType::Bool,
                ))));

            let has_next_cond_name = get_expr_name("has_next", expr_index);
            let has_next_cond_ctype = CType::Bool;
            declarations.push(CDeclaration(
                has_next_cond_ctype.clone(),
                has_next_cond_name.clone(),
            ));
            statements.push(value_ast.replace_expr(CStatement::VarAssign(
                has_next_cond_name.clone(),
                has_next_cond_expr.clone(),
            )));

            let mut while_declarations = Vec::new();
            let mut while_statements = Vec::new();

            let element_name = get_expr_name("list_element", expr_index);
            let element_ctype = type_to_ctype(&element_type.expr);
            while_declarations.push(CDeclaration(element_ctype.clone(), element_name.clone()));
            while_statements.push(value_ast.replace_expr(CStatement::VarAssign(
                element_name.clone(),
                value_ast.replace_expr(CExpr::ObjectAccess {
                    object: iteration_var.clone(),
                    index: 0,
                    field_type: element_ctype.clone(),
                }),
            )));

            print_value(
                value_ast.replace_expr(CValue::Ident(element_name, element_ctype)),
                &element_type.expr,
                &mut while_declarations,
                &mut while_statements,
                expr_index,
                real_types,
                record_layouts,
            );

            while_statements.push(value_ast.replace_expr(CStatement::VarAssign(
                iteration_var_name.clone(),
                value_ast.replace_expr(CExpr::ObjectAccess {
                    object: iteration_var,
                    index: 1,
                    field_type: iteration_var_type,
                }),
            )));

            while_statements.push(value_ast.replace_expr(CStatement::VarAssign(
                has_next_cond_name.clone(),
                has_next_cond_expr,
            )));

            let has_next_cond_ident =
                value_ast.replace_expr(CValue::Ident(has_next_cond_name, has_next_cond_ctype));

            while_statements.push(value_ast.replace_expr(CStatement::If(
                has_next_cond_ident.clone(),
                value_ast.replace_expr(CStatement::PrintText(String::from(" "))),
                value_ast.replace_expr(CStatement::Block(vec![], vec![])),
            )));

            statements.push(value_ast.replace_expr(CStatement::While(
                has_next_cond_ident,
                value_ast.replace_expr(CStatement::Block(while_declarations, while_statements)),
            )));

            statements.push(value_ast.replace_expr(CStatement::PrintText(String::from("]"))));
        }
        EvaluatedType::Ref(ref wrapped_type) => {
            statements.push(value_ast.replace_expr(CStatement::PrintText(String::from("(&!"))));

            let deref_name = get_expr_name("deref", expr_index);
            let deref_type = type_to_ctype(&wrapped_type.expr);

            declarations.push(CDeclaration(deref_type.clone(), deref_name.clone()));
            statements.push(value_ast.replace_expr(CStatement::VarAssign(
                deref_name.clone(),
                value_ast.replace_expr(CExpr::AnyRefGet(value_ast.clone(), deref_type.clone())),
            )));

            print_value(
                value_ast.replace_expr(CValue::Ident(deref_name, deref_type)),
                &wrapped_type.expr,
                declarations,
                statements,
                expr_index,
                real_types,
                record_layouts,
            );

            statements.push(value_ast.replace_expr(CStatement::PrintText(String::from(")"))));
        }
    }
}
