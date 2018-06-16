use std::cell::RefCell;
use std::collections::HashMap;
use std::f64;
use std::iter::Iterator;
use std::rc::Rc;
use untyped_ast::{Ast, BinOp, Expression, NumOp, Pattern, RelOp, Type, Value};
use util::Error;

fn evaluate_number_operator<F>(
    ast: &Ast<Expression>,
    left: &Value,
    right: &Value,
    evaluate: F,
) -> Result<Ast<Expression>, Error>
where
    F: Fn(f64, f64) -> f64,
{
    Ok(ast.replace_expr(Expression::Value(match (left, right) {
        (&Value::Int(left_value), &Value::Int(right_value)) => {
            Value::Int(evaluate(left_value as f64, right_value as f64) as i32)
        }
        (&Value::Num(left_value), &Value::Num(right_value)) => {
            Value::Num(evaluate(left_value, right_value))
        }
        (&Value::Int(left_value), &Value::Num(right_value)) => {
            Value::Num(evaluate(left_value as f64, right_value))
        }
        (&Value::Num(left_value), &Value::Int(right_value)) => {
            Value::Num(evaluate(left_value, right_value as f64))
        }
        _ => {
            return Err(Error::LRLocated {
                message: String::from("Fuck off with your non-number bullshit."),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            })
        }
    })))
}

#[cfg(test)]
mod evaluate_number_operator {
    use super::*;
    use std::f64::NAN;

    fn ast() -> Ast<Expression> {
        Ast::<Expression>::new(0, 0, Expression::Value(Value::Int(1)))
    }

    #[test]
    fn operates_on_two_ints() {
        assert_eq!(
            *evaluate_number_operator(&ast(), &Value::Int(2), &Value::Int(4), |a, b| a + b)
                .unwrap()
                .expr,
            Expression::Value(Value::Int(6))
        );
    }

    #[test]
    fn operates_on_two_floats() {
        assert_eq!(
            *evaluate_number_operator(&ast(), &Value::Num(2.1), &Value::Num(4.3), |a, b| a + b)
                .unwrap()
                .expr,
            Expression::Value(Value::Num(6.4)),
        );
    }

    #[test]
    fn operates_on_mixed_ints_and_floats() {
        assert_eq!(
            *evaluate_number_operator(&ast(), &Value::Int(2), &Value::Num(4.3), |a, b| a + b)
                .unwrap()
                .expr,
            Expression::Value(Value::Num(6.3)),
        );
        assert_eq!(
            *evaluate_number_operator(&ast(), &Value::Num(2.3), &Value::Int(4), |a, b| a + b)
                .unwrap()
                .expr,
            Expression::Value(Value::Num(6.3)),
        );
    }

    #[test]
    fn doesnt_add_bools() {
        assert!(
            evaluate_number_operator(&ast(), &Value::Bool(true), &Value::Int(4), |a, b| a + b)
                .is_err()
        );
        assert!(
            evaluate_number_operator(&ast(), &Value::Int(4), &Value::Bool(true), |a, b| a + b)
                .is_err()
        );
        assert!(
            evaluate_number_operator(&ast(), &Value::Bool(true), &Value::Bool(true), |a, b| a + b)
                .is_err()
        );
    }

    fn assert_is_nan(actual: Ast<Expression>) {
        assert_eq!(format!("{}", actual), "nan");
    }

    #[test]
    fn turns_nan_add_inputs_to_nan_add_outputs() {
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Num(NAN), &Value::Int(4), |a, b| a + b)
                .unwrap(),
        );
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Int(4), &Value::Num(NAN), |a, b| a + b)
                .unwrap(),
        );
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Num(NAN), &Value::Num(4.2), |a, b| a + b)
                .unwrap(),
        );
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Num(4.2), &Value::Num(NAN), |a, b| a + b)
                .unwrap(),
        );
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Num(NAN), &Value::Num(NAN), |a, b| a + b)
                .unwrap(),
        );
    }
}

fn substitute(
    ast: &Ast<Expression>,
    pattern: &Ast<Pattern>,
    value: &Ast<Expression>,
) -> Ast<Expression> {
    match &*pattern.expr {
        &Pattern::Tuple(ref vec) => {
            let mut substituted_ast = ast.clone();

            if let &Expression::Value(Value::Tuple(ref value_vec)) = &*value.expr {
                for (el, value_el) in Iterator::zip(vec.into_iter(), value_vec.into_iter()) {
                    substituted_ast = substitute(&substituted_ast, el, value_el);
                }
            }

            substituted_ast
        }
        &Pattern::Ident(ref name, _) => match &*ast.expr {
            &Expression::GenericCall(ref expr, _) => substitute(expr, pattern, value),
            &Expression::Ident(ref ident_name) => {
                if ident_name == name {
                    value.clone()
                } else {
                    ast.clone()
                }
            }
            &Expression::Sequence(ref side_effect, ref result) => {
                ast.replace_expr(Expression::Sequence(
                    substitute(side_effect, pattern, value),
                    substitute(result, pattern, value),
                ))
            }
            &Expression::RecordAccess(ref record, ref field) => ast.replace_expr(
                Expression::RecordAccess(substitute(record, pattern, value), field.clone()),
            ),
            &Expression::Call(ref func, ref arg) => ast.replace_expr(Expression::Call(
                substitute(func, pattern, value),
                substitute(arg, pattern, value),
            )),
            &Expression::BinOp(ref op, ref left, ref right) => ast.replace_expr(Expression::BinOp(
                op.clone(),
                substitute(left, pattern, value),
                substitute(right, pattern, value),
            )),
            &Expression::If(ref c, ref t, ref e) => ast.replace_expr(Expression::If(
                substitute(c, pattern, value),
                substitute(t, pattern, value),
                substitute(e, pattern, value),
            )),
            &Expression::Let(ref bind_pattern, ref bind_value, ref body) => {
                if bind_pattern.contains_name(name) {
                    ast.clone()
                } else {
                    ast.replace_expr(Expression::Let(
                        bind_pattern.clone(),
                        substitute(bind_value, pattern, value),
                        substitute(body, pattern, value),
                    ))
                }
            }
            &Expression::TypeLet(ref bind_name, ref bind_value, ref body) => {
                ast.replace_expr(Expression::TypeLet(
                    bind_name.clone(),
                    bind_value.clone(),
                    substitute(body, pattern, value),
                ))
            }
            &Expression::Value(Value::Func(ref param_pattern, ref body_type, ref body)) => {
                ast.replace_expr(Expression::Value(Value::Func(
                    param_pattern.clone(),
                    body_type.clone(),
                    if param_pattern.contains_name(name) {
                        body.clone()
                    } else {
                        substitute(body, pattern, value)
                    },
                )))
            }
            &Expression::Value(Value::GenericFunc(_, _, _, ref body)) => {
                substitute(body, pattern, value)
            }
            &Expression::Value(Value::Tuple(ref vec)) => {
                ast.replace_expr(Expression::Value(Value::Tuple(
                    vec.into_iter()
                        .map(|element| substitute(element, pattern, value))
                        .collect(),
                )))
            }
            &Expression::Value(Value::List(ref vec)) => {
                ast.replace_expr(Expression::Value(Value::List(
                    vec.into_iter()
                        .map(|element| substitute(element, pattern, value))
                        .collect(),
                )))
            }
            &Expression::Value(Value::Record(ref map)) => {
                ast.replace_expr(Expression::Value(Value::Record(
                    map.iter()
                        .map(|(field, element)| {
                            (field.clone(), substitute(element, pattern, value))
                        })
                        .collect(),
                )))
            }
            &Expression::Value(Value::Bool(_))
            | &Expression::Value(Value::Hook(_, _))
            | &Expression::Value(Value::Int(_))
            | &Expression::Value(Value::Num(_))
            | &Expression::Value(Value::Ref(_)) => ast.clone(),
            &Expression::RefNew(ref value_ast) => {
                ast.replace_expr(Expression::RefNew(substitute(value_ast, pattern, value)))
            }
            &Expression::RefGet(ref reference) => {
                ast.replace_expr(Expression::RefGet(substitute(reference, pattern, value)))
            }
            &Expression::RefSet(ref reference, ref value_ast) => {
                let subref = substitute(reference, pattern, value);
                ast.replace_expr(Expression::RefSet(
                    subref,
                    substitute(value_ast, pattern, value),
                ))
            }
        },
    }
}

fn handle_hook_call(
    path: &Vec<String>,
    left_loc: usize,
    right_loc: usize,
    param_ast: &Ast<Expression>,
    param_value: &Value,
) -> Option<Result<Ast<Expression>, Error>> {
    Some(match path.join(".").as_str() {
        "std.list.push" => {
            if let &Value::Tuple(ref right_vec) = param_value {
                let el = &right_vec[0];
                let list = &right_vec[1];

                if let &Expression::Value(Value::List(ref vec)) = &*list.expr {
                    let mut list = vec.clone();
                    list.insert(0, el.clone());

                    Ok(Ast::<Expression>::new(
                        left_loc,
                        right_loc,
                        Expression::Value(Value::List(list)),
                    ))
                } else {
                    panic!("Second tuple arg should be list.")
                }
            } else {
                panic!("list.push arg should be tuple.")
            }
        }
        "std.list.is_empty" => {
            if let &Value::List(ref vec) = param_value {
                Ok(Ast::<Expression>::new(
                    left_loc,
                    right_loc,
                    Expression::Value(Value::Bool(vec.is_empty())),
                ))
            } else {
                panic!("list.is_empty arg should be list.")
            }
        }
        "std.list.head" => {
            if let &Value::List(ref vec) = param_value {
                if vec.is_empty() {
                    Err(Error::LRLocated {
                        message: String::from(
                            "As usual, you can't get head.\n\
                             Especially not from an empty list.",
                        ),
                        left_loc: param_ast.left_loc,
                        right_loc: param_ast.right_loc,
                    })
                } else {
                    Ok(vec[0].clone())
                }
            } else {
                panic!("list.head arg should be list.")
            }
        }
        "std.list.tail" => {
            if let &Value::List(ref vec) = param_value {
                if vec.is_empty() {
                    Err(Error::LRLocated {
                        message: String::from("There's no tail on that list."),
                        left_loc: param_ast.left_loc,
                        right_loc: param_ast.right_loc,
                    })
                } else {
                    Ok(Ast::<Expression>::new(
                        left_loc,
                        right_loc,
                        Expression::Value(Value::List(Vec::from(&vec[1..]))),
                    ))
                }
            } else {
                panic!("list.tail arg should be list.")
            }
        }
        "std.math.floordiv" => {
            if let &Value::Tuple(ref vec) = param_value {
                if let (&Expression::Value(ref left), &Expression::Value(ref right)) =
                    (&*vec[0].expr, &*vec[1].expr)
                {
                    let div_by_zero_error = Err(Error::LRLocated {
                        message: String::from("Can't divide by your future (which is zero)."),
                        left_loc: vec[1].left_loc,
                        right_loc: vec[1].right_loc,
                    });
                    match (left, right) {
                        (&Value::Int(_), &Value::Int(0)) | (&Value::Num(_), &Value::Int(0)) => {
                            div_by_zero_error
                        }
                        (&Value::Int(_), &Value::Num(val)) | (&Value::Num(_), &Value::Num(val))
                            if val as i32 == 0 =>
                        {
                            div_by_zero_error
                        }
                        _ => Ok(param_ast.replace_expr(Expression::Value(Value::Int(match (
                            left, right,
                        ) {
                            (&Value::Int(left_value), &Value::Int(right_value)) => {
                                left_value / right_value
                            }
                            (&Value::Int(left_value), &Value::Num(right_value)) => {
                                left_value / right_value as i32
                            }
                            (&Value::Num(left_value), &Value::Int(right_value)) => {
                                left_value as i32 / right_value
                            }
                            (&Value::Num(left_value), &Value::Num(right_value)) => {
                                left_value as i32 / right_value as i32
                            }
                            _ => panic!("int.floordiv can only be called on numbers"),
                        })))),
                    }
                } else {
                    panic!("int.floordiv must be called with values")
                }
            } else {
                panic!("int.floordiv arg should be a tuple")
            }
        }
        _ => {
            return None;
        }
    })
}

fn step_ast(ast: &Ast<Expression>) -> Result<Ast<Expression>, Error> {
    let left_loc = ast.left_loc;
    let right_loc = ast.right_loc;

    match &*ast.expr {
        &Expression::GenericCall(ref expr, _) => Ok(expr.clone()),
        &Expression::Value(Value::GenericFunc(_, _, _, ref body)) => Ok(body.clone()),
        &Expression::Value(Value::Tuple(ref vec)) => {
            let mut stepped_tup = Vec::new();

            for value in vec {
                if value.is_term() {
                    stepped_tup.push(value.clone());
                } else {
                    stepped_tup.push(step_ast(value)?);
                }
            }

            Ok(ast.replace_expr(Expression::Value(Value::Tuple(stepped_tup))))
        }
        &Expression::Value(Value::List(ref vec)) => {
            let mut stepped_list = Vec::new();

            for value in vec {
                if value.is_term() {
                    stepped_list.push(value.clone());
                } else {
                    stepped_list.push(step_ast(value)?);
                }
            }

            Ok(ast.replace_expr(Expression::Value(Value::List(stepped_list))))
        }
        &Expression::Value(Value::Record(ref map)) => {
            let mut stepped_record = HashMap::new();

            for (field, value) in map {
                if value.is_term() {
                    stepped_record.insert(field.clone(), value.clone());
                } else {
                    stepped_record.insert(field.clone(), step_ast(value)?);
                }
            }

            Ok(ast.replace_expr(Expression::Value(Value::Record(stepped_record))))
        }
        &Expression::Value(Value::Bool(_))
        | &Expression::Value(Value::Hook(_, _))
        | &Expression::Value(Value::Int(_))
        | &Expression::Value(Value::Num(_))
        | &Expression::Value(Value::Ref(_))
        | &Expression::Value(Value::Func(_, _, _)) => Ok(ast.clone()),
        &Expression::Ident(ref name) => Err(Error::LRLocated {
            message: format!("How do you expect me to evaluate a `{}`?", name),
            left_loc,
            right_loc,
        }),
        &Expression::RecordAccess(ref record, ref field) => {
            if record.is_term() {
                if let &Expression::Value(Value::Record(ref map)) = &*record.expr {
                    if let Some(field_value) = map.get(field) {
                        Ok(field_value.clone())
                    } else {
                        panic!()
                    }
                } else {
                    panic!()
                }
            } else {
                Ok(ast.replace_expr(Expression::RecordAccess(step_ast(record)?, field.clone())))
            }
        }
        &Expression::Sequence(ref side_effect, ref result) => Ok(if side_effect.is_term() {
            result.clone()
        } else {
            ast.replace_expr(Expression::Sequence(step_ast(side_effect)?, result.clone()))
        }),
        &Expression::Call(ref func_ast, ref arg_ast) => match (&*func_ast.expr, &*arg_ast.expr) {
            (&Expression::Value(ref func), &Expression::Value(ref arg))
                if func_ast.is_term() && arg_ast.is_term() =>
            {
                if let &Value::Hook(ref path, _) = func {
                    match handle_hook_call(path, left_loc, right_loc, arg_ast, arg) {
                        Some(result) => result,
                        None => Err(Error::LRLocated {
                            message: format!(
                                "`{}` ain't gonna hook up with your ugly ass.\n\
                                 'Cuz it's not a hook.",
                                path.join(".")
                            ),
                            left_loc: func_ast.left_loc,
                            right_loc: func_ast.right_loc,
                        }),
                    }
                } else if let &Value::Func(ref pattern, _, ref body) = func {
                    Ok(substitute(
                        body,
                        pattern,
                        &arg_ast.replace_expr(Expression::Value(arg.clone())),
                    ))
                } else {
                    Err(Error::LRLocated {
                        message: String::from(
                            "I only call two things on a regular basis: functions, and your mom.\
                             \nThat's not a function.",
                        ),
                        left_loc,
                        right_loc,
                    })
                }
            }
            (&Expression::Value(_), _) if func_ast.is_term() => {
                Ok(ast.replace_expr(Expression::Call(func_ast.clone(), step_ast(arg_ast)?)))
            }
            _ => Ok(ast.replace_expr(Expression::Call(step_ast(func_ast)?, arg_ast.clone()))),
        },
        &Expression::BinOp(ref operation, ref left_ast, ref right_ast) => match (
            &*left_ast.expr,
            &*right_ast.expr,
        ) {
            (&Expression::Value(ref left), &Expression::Value(ref right))
                if left_ast.is_term() && right_ast.is_term() =>
            {
                match operation {
                    &BinOp::Num(ref num_op) => match num_op {
                        &NumOp::Add => evaluate_number_operator(ast, &left, &right, |a, b| a + b),
                        &NumOp::Sub => evaluate_number_operator(ast, &left, &right, |a, b| a - b),
                        &NumOp::Mul => evaluate_number_operator(ast, &left, &right, |a, b| a * b),
                        &NumOp::Div => match (left, right) {
                            (&Value::Int(left_value), &Value::Int(right_value)) => {
                                Ok(ast.replace_expr(Expression::Value(Value::Num(
                                    (left_value as f64) / (right_value as f64),
                                ))))
                            }
                            (left_value, right_value) => {
                                evaluate_number_operator(ast, left_value, right_value, |a, b| a / b)
                            }
                        },
                    },
                    &BinOp::Rel(ref rel_op) => match rel_op {
                        &RelOp::LEq => Ok(ast.replace_expr(Expression::Value(Value::Bool(
                            match (left, right) {
                                (&Value::Int(left_value), &Value::Int(right_value)) => {
                                    Ok(left_value <= right_value)
                                }
                                (&Value::Num(left_value), &Value::Int(right_value)) => {
                                    Ok(left_value <= right_value as f64)
                                }
                                (&Value::Int(left_value), &Value::Num(right_value)) => {
                                    Ok(left_value as f64 <= right_value)
                                }
                                (&Value::Num(left_value), &Value::Num(right_value)) => {
                                    Ok(left_value <= right_value)
                                }
                                _ => Err(Error::LRLocated {
                                    message: String::from(
                                        "Fuck off with your non-number bullshit.",
                                    ),
                                    left_loc,
                                    right_loc,
                                }),
                            }?,
                        )))),
                    },
                }
            }
            (&Expression::Value(_), _) if left_ast.is_term() => Ok(ast.replace_expr(
                Expression::BinOp(operation.clone(), left_ast.clone(), step_ast(right_ast)?),
            )),
            _ => Ok(ast.replace_expr(Expression::BinOp(
                operation.clone(),
                step_ast(left_ast)?,
                right_ast.clone(),
            ))),
        },
        &Expression::If(ref guard, ref consequent, ref alternate) => match &*guard.expr {
            &Expression::Value(Value::Bool(guard_value)) => {
                if guard_value {
                    Ok(consequent.clone())
                } else {
                    Ok(alternate.clone())
                }
            }
            &Expression::Value(_) => Err(Error::LRLocated {
                message: String::from("This isn't JavaScript.  Only bools in `if`s.  Dumbass."),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            }),
            _ => Ok(ast.replace_expr(Expression::If(
                step_ast(guard)?,
                consequent.clone(),
                alternate.clone(),
            ))),
        },
        &Expression::Let(ref bind_pattern, ref bind_value, ref body) => if bind_value.is_term() {
            Ok(substitute(
                body,
                bind_pattern,
                &substitute(
                    bind_value,
                    bind_pattern,
                    &bind_value.replace_expr(Expression::Let(
                        bind_pattern.clone(),
                        bind_value.clone(),
                        bind_value.clone(),
                    )),
                ),
            ))
        } else {
            Ok(ast.replace_expr(Expression::Let(
                bind_pattern.clone(),
                step_ast(bind_value)?,
                body.clone(),
            )))
        },
        &Expression::TypeLet(_, _, ref body) => Ok(body.clone()),
        &Expression::RefNew(ref value) => Ok(ast.replace_expr(match &*value.expr {
            &Expression::Value(ref term_value) if value.is_term() => {
                Expression::Value(Value::Ref(Rc::new(RefCell::new(term_value.clone()))))
            }
            _ => Expression::RefNew(step_ast(value)?),
        })),
        &Expression::RefGet(ref reference) => Ok(ast.replace_expr(match &*reference.expr {
            &Expression::Value(Value::Ref(ref rc)) if reference.is_term() => {
                Expression::Value(rc.borrow().clone())
            }
            _ => Expression::RefGet(step_ast(reference)?),
        })),
        &Expression::RefSet(ref reference, ref value) => {
            Ok(ast.replace_expr(match &*reference.expr {
                &Expression::Value(Value::Ref(ref rc)) if reference.is_term() => match &*value.expr
                {
                    &Expression::Value(ref term_value) if value.is_term() => {
                        *rc.borrow_mut() = term_value.clone();
                        Expression::Value(term_value.clone())
                    }
                    _ => Expression::RefSet(reference.clone(), step_ast(value)?),
                },
                _ => Expression::RefSet(step_ast(reference)?, value.clone()),
            }))
        }
    }
}

pub fn get_eval_steps(
    ast: &Ast<Expression>,
    globals: &HashMap<String, Value>,
) -> Result<Vec<Ast<Expression>>, Error> {
    let mut ast = ast.clone();

    for (name, value) in globals {
        ast = substitute(
            &ast,
            &Ast::<Pattern>::new(
                0,
                0,
                Pattern::Ident(name.clone(), Ast::<Type>::new(0, 0, Type::Tuple(vec![]))),
            ),
            &Ast::<Expression>::new(0, 0, Expression::Value(value.clone())),
        );
    }

    let mut steps = Vec::<Ast<Expression>>::new();
    loop {
        steps.push(ast.clone());
        let stepped_ast = step_ast(&ast)?;

        if ast.is_term() {
            return Ok(steps);
        }

        if stepped_ast == ast {
            for step in steps {
                println!("--> {}", step);
            }
            panic!("Infinite loop");
        }

        ast = stepped_ast;
    }
}

pub fn evaluate_ast(
    ast: &Ast<Expression>,
    globals: &HashMap<String, Value>,
) -> Result<Value, Error> {
    let steps = get_eval_steps(ast, globals)?;
    if let &Expression::Value(ref value) = &*steps[steps.len() - 1].expr {
        Ok(value.clone())
    } else {
        panic!("Terminated without a value");
    }
}

#[cfg(test)]
mod evaluate_ast {
    use super::*;
    use parse::source_to_ast;

    fn assert_eval_eq(actual: &str, expected: &str) {
        assert_eq!(
            format!(
                "{}",
                evaluate_ast(&source_to_ast(actual).unwrap(), &HashMap::new()).unwrap()
            ),
            expected
        );
    }

    fn assert_eval_err(source: &str) {
        assert!(evaluate_ast(&source_to_ast(source).unwrap(), &HashMap::new()).is_err());
    }

    #[test]
    fn spits_back_out_an_int() {
        assert_eval_eq("5", "5");
    }

    #[test]
    fn spits_back_out_a_float() {
        assert_eval_eq("5.0", "5");
        assert_eval_eq("5.2", "5.2");
        assert_eval_eq(
            "500000000000000000000000000000000000000000000000000.0",
            "500000000000000000000000000000000000000000000000000",
        );
        assert_eval_eq(
            "0.000000000000000000000000000000000000000000000000006",
            "0.000000000000000000000000000000000000000000000000006",
        );
        assert_eval_eq("500000000000.1", "500000000000.1");
    }

    #[test]
    fn spits_back_out_a_bool() {
        assert_eval_eq("#true ()", "#true ()");
    }

    #[test]
    fn spits_back_out_globals() {
        let mut globals = HashMap::new();
        globals.insert(String::from("global_val"), Value::Int(5));

        assert_eq!(
            format!(
                "{}",
                evaluate_ast(&source_to_ast("global_val").unwrap(), &globals).unwrap()
            ),
            "5"
        );
    }

    #[test]
    fn doesnt_handle_straight_identifiers() {
        assert_eval_err("foo");
    }

    #[test]
    fn adds() {
        assert_eval_eq("1 + 2", "3");
    }

    #[test]
    fn adds_negative_numbers() {
        assert_eval_eq("-1 + -2", "-3");
    }

    #[test]
    fn subtracts() {
        assert_eval_eq("2 - 1", "1");
    }

    #[test]
    fn multiplies() {
        assert_eval_eq("3 * 2", "6");
    }

    #[test]
    fn divides_integers_as_floats() {
        assert_eval_eq("3 / 2", "1.5");
    }

    #[test]
    fn can_floor_divide_with_a_hook() {
        assert_eval_eq(
            r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (7 4)"#,
            "1",
        );
        assert_eval_eq(
            r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (7.0 4)"#,
            "1",
        );
        assert_eval_eq(
            r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (7 4.0)"#,
            "1",
        );
        assert_eval_eq(
            r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (7.0 4.0)"#,
            "1",
        );
        assert_eval_err(r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (7 0)"#);
        assert_eval_err(r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (7 0.0)"#);
        assert_eval_err(r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (7.0 0)"#);
        assert_eval_err(r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (7.0 0.0)"#);
        assert_eval_err(r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (0 0)"#);
        assert_eval_err(r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (0 0.0)"#);
        assert_eval_err(r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (0.0 0)"#);
        assert_eval_err(r#"@hook ("std.math.floordiv"  (Num Num) => Int) <| (0.0 0.0)"#);
    }

    #[test]
    fn divides_floats() {
        assert_eval_eq("3.0 / 2", "1.5");
    }

    #[test]
    fn divides_numbers_by_zero() {
        assert_eval_eq("3.0 / 0", "inf");
        assert_eval_eq("0.0 / 0", "nan");
        assert_eval_eq("3 / 0", "inf");
        assert_eval_eq("0 / 0", "nan");
    }

    #[test]
    fn handles_nested_arithmatic() {
        assert_eval_eq("1 + (2 * 3)", "7");
    }

    #[test]
    fn compares_with_leq() {
        assert_eval_eq("3 <= 2", "#false ()");
        assert_eval_eq("2 <= 2", "#true ()");
        assert_eval_eq("1 <= 2", "#true ()");
    }

    #[test]
    fn returns_the_consequent_of_an_if() {
        assert_eval_eq("if 1 <= 1 then 1 else 2", "1");
    }

    #[test]
    fn returns_the_alternate_of_an_if() {
        assert_eval_eq("if (#false ()) then 1 else 2", "2");
    }

    #[test]
    fn requires_a_bool_if_guard() {
        assert_eval_err("if 1 then 2 else 3");
    }

    #[test]
    fn substitutes_in_a_nested_function_call() {
        assert_eval_eq(
            "(inc: (Int => Int) =Int=> inc <| 5) <| (x: Int =Int=> x + 1)",
            "6",
        );
    }

    #[test]
    fn substitutes_with_a_let_expression() {
        assert_eval_eq(
            "
            let added_val: Int <- 5
            ((inc: (Int => Int) =Int=> inc <| added_val) <| (x: Int =Int=> x + 1))
            ",
            "6",
        );
    }

    #[test]
    fn substitutes_nested_let_expression() {
        assert_eval_eq(
            "
            let x: Int <- 5
            let y: Int <- x + 1
            y
            ",
            "6",
        );
    }

    #[test]
    fn substitutes_in_tuples() {
        assert_eval_eq(
            "
            let x: Int <- 5
            let tup: (Int Num) <- (x 2.3)
            tup
            ",
            "(5 2.3)",
        );
    }

    #[test]
    fn destructures_tuples() {
        assert_eval_eq(
            "
            let x: Int <- 5
            let (a: Int  b: Num) <- (x 2.3)
            a + b
            ",
            "7.3",
        );
        assert_eval_eq(
            "
            (2  5.3  #true ()) |> (fn (a: Int  b: Num  bool: Bool) =Num=> a + b)
            ",
            "7.3",
        );
        assert_eval_eq(
            "
            () |> (fn () =Int=> 2 + 3)
            ",
            "5",
        );
    }

    #[test]
    fn substitutes_in_lists() {
        assert_eval_eq(
            "
            let x: Num <- 5.0
            let list: [Num] <- [x 2.3 x]
            list
            ",
            "[5 2.3 5]",
        );
    }

    #[test]
    fn substitutes_in_records() {
        assert_eval_eq(
            "
            let x: Int <- 5
            let rec: Any <- {a <- x  bar <- 2.3}
            rec
            ",
            "{a <- 5 bar <- 2.3}",
        );
    }

    #[test]
    fn evaluates_in_tuples() {
        assert_eval_eq(
            "
            ((4 + 1 5) 2.3)
            ",
            "((5 5) 2.3)",
        );
    }

    #[test]
    fn evaluates_in_lists() {
        assert_eval_eq(
            "
            [[4 + 1  5] []]
            ",
            "[[5 5] []]",
        );
    }

    #[test]
    fn evaluates_in_records() {
        assert_eval_eq(
            "
            {a <- {x <- 4 + 1  y <- 5}  bar <- 2.3}
            ",
            "{a <- {x <- 5 y <- 5} bar <- 2.3}",
        );
    }

    #[test]
    fn sequences_execute_side_effects() {
        assert_eval_eq(
            r#"
            let x: Any <- &!5
            *!x <- 7;
            *!x <- *!x + 1;
            *!x
            "#,
            "8",
        );
    }
    #[test]
    fn supports_recursion_in_the_let_expression() {
        assert_eval_eq(
            "
            let factorial: (Int => Int) <- n: Int =Int=>
                if n <= 0 then
                    1
                else
                    n * (factorial <| n - 1)

            factorial <| 5
            ",
            &format!("{}", 5 * 4 * 3 * 2 * 1),
        );
    }

    #[test]
    fn doesnt_substitute_shadowed_variables() {
        assert_eval_eq(
            "(x: Int =(Int => Int)=> x: Int =Int=> x + 1) <| 5",
            "(fn x: Int =Int=> ((x) + 1))",
        );
    }

    #[test]
    fn list_push_hook() {
        assert_eval_eq(
            r#"
            (1 [2 3]) |> @hook("std.list.push"  ((Int  [Int]) => [Int]))
            "#,
            "[1 2 3]",
        );
        assert_eval_eq(
            r#"
            (1 []) |> @hook("std.list.push"  ((Int  [Int]) => [Int]))
            "#,
            "[1]",
        );
        assert_eval_eq(
            r#"
            (1.6 [2.1 3.7]) |> @hook("std.list.push"  ((Num  [Num]) => [Num]))
            "#,
            "[1.6 2.1 3.7]",
        );
    }

    #[test]
    fn list_is_empty_hook() {
        assert_eval_eq(
            r#"
            [1 2 3] |> @hook("std.list.is_empty"  ([Int] => Bool))
            "#,
            "#false ()",
        );
        assert_eval_eq(
            r#"
            [] |> @hook("std.list.is_empty"  ([Int] => Bool))
            "#,
            "#true ()",
        );
        assert_eval_eq(
            r#"
            [#true() #false()] |> @hook("std.list.is_empty"  ([Bool] => Bool))
            "#,
            "#false ()",
        );
        assert_eval_eq(
            r#"
            [] |> @hook("std.list.is_empty"  ([Num] => Bool))
            "#,
            "#true ()",
        );
    }

    #[test]
    fn list_head_hook() {
        assert_eval_eq(
            r#"
            [1 2 3] |> @hook("std.list.head"  ([Int] => Int))
            "#,
            "1",
        );
        assert_eval_err(
            r#"
            [] |> @hook("std.list.head"  ([Int] => Int))
            "#,
        );
        assert_eval_eq(
            r#"
            [#true() #false()] |> @hook("std.list.head"  ([Bool] => Bool))
            "#,
            "#true ()",
        );
    }

    #[test]
    fn list_tail_hook() {
        assert_eval_eq(
            r#"
            [1 2 3] |> @hook("std.list.tail"  ([Int] => Int))
            "#,
            "[2 3]",
        );
        assert_eval_err(
            r#"
            [] |> @hook("std.list.tail"  ([Int] => Int))
            "#,
        );
        assert_eval_eq(
            r#"
            [#true() #false()] |> @hook("std.list.tail"  ([Bool] => Bool))
            "#,
            "[#false ()]",
        );
    }
}
