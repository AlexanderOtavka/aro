use ast::{Ast, BinOp, Expression, Pattern, Type, Value};
use util::Error;
use std::collections::HashMap;
use std::f64;
use std::iter::Iterator;

fn evaluate_number_operator<F>(
    ast: &Ast<Expression>,
    left: &Value,
    right: &Value,
    evaluate: F,
) -> Result<Ast<Expression>, Error>
where
    F: Fn(f64, f64) -> f64,
{
    Ok(Ast {
        expr: Box::new(Expression::Value(match (left, right) {
            (&Value::Int(left_value), &Value::Int(right_value)) => {
                Ok(Value::Int(evaluate(left_value as f64, right_value as f64)
                    as i32))
            }
            (&Value::Float(left_value), &Value::Float(right_value)) => {
                Ok(Value::Float(evaluate(left_value, right_value)))
            }
            (&Value::Int(left_value), &Value::Float(right_value)) => {
                Ok(Value::Float(evaluate(left_value as f64, right_value)))
            }
            (&Value::Float(left_value), &Value::Int(right_value)) => {
                Ok(Value::Float(evaluate(left_value, right_value as f64)))
            }
            _ => Err(Error::LRLocated {
                message: String::from("Fuck off with your non-number bullshit."),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            }),
        }?)),
        left_loc: ast.left_loc,
        right_loc: ast.right_loc,
    })
}

#[cfg(test)]
mod evaluate_number_operator {
    use super::*;
    use std::f64::NAN;

    fn ast() -> Ast<Expression> {
        Ast {
            expr: Box::new(Expression::Value(Value::Int(1))),
            left_loc: 0,
            right_loc: 0,
        }
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
            *evaluate_number_operator(&ast(), &Value::Float(2.1), &Value::Float(4.3), |a, b| a + b)
                .unwrap()
                .expr,
            Expression::Value(Value::Float(6.4)),
        );
    }

    #[test]
    fn operates_on_mixed_ints_and_floats() {
        assert_eq!(
            *evaluate_number_operator(&ast(), &Value::Int(2), &Value::Float(4.3), |a, b| a + b)
                .unwrap()
                .expr,
            Expression::Value(Value::Float(6.3)),
        );
        assert_eq!(
            *evaluate_number_operator(&ast(), &Value::Float(2.3), &Value::Int(4), |a, b| a + b)
                .unwrap()
                .expr,
            Expression::Value(Value::Float(6.3)),
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
            evaluate_number_operator(&ast(), &Value::Float(NAN), &Value::Int(4), |a, b| a + b)
                .unwrap(),
        );
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Int(4), &Value::Float(NAN), |a, b| a + b)
                .unwrap(),
        );
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Float(NAN), &Value::Float(4.2), |a, b| a + b)
                .unwrap(),
        );
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Float(4.2), &Value::Float(NAN), |a, b| a + b)
                .unwrap(),
        );
        assert_is_nan(
            evaluate_number_operator(&ast(), &Value::Float(NAN), &Value::Float(NAN), |a, b| a + b)
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
            &Expression::Ident(ref ident_name) => {
                if ident_name == name {
                    value.clone()
                } else {
                    ast.clone()
                }
            }
            &Expression::BinOp(ref op, ref left, ref right) => Ast {
                expr: Box::new(Expression::BinOp(
                    op.clone(),
                    substitute(left, pattern, value),
                    substitute(right, pattern, value),
                )),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            },
            &Expression::If(ref c, ref t, ref e) => Ast {
                expr: Box::new(Expression::If(
                    substitute(c, pattern, value),
                    substitute(t, pattern, value),
                    substitute(e, pattern, value),
                )),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            },
            &Expression::Let(ref bind_pattern, ref bind_value, ref body) => {
                if bind_pattern.contains_name(name) {
                    ast.clone()
                } else {
                    Ast {
                        expr: Box::new(Expression::Let(
                            bind_pattern.clone(),
                            substitute(bind_value, pattern, value),
                            substitute(body, pattern, value),
                        )),
                        left_loc: ast.left_loc,
                        right_loc: ast.right_loc,
                    }
                }
            }
            &Expression::Value(Value::Func(ref param_pattern, ref body_type, ref body)) => Ast {
                expr: Box::new(Expression::Value(Value::Func(
                    param_pattern.clone(),
                    body_type.clone(),
                    if param_pattern.contains_name(name) {
                        body.clone()
                    } else {
                        substitute(body, pattern, value)
                    },
                ))),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            },
            &Expression::Value(Value::Tuple(ref vec)) => Ast::<Expression>::new(
                ast.left_loc,
                ast.right_loc,
                Expression::Value(Value::Tuple(
                    vec.into_iter()
                        .map(|element| substitute(element, pattern, value))
                        .collect(),
                )),
            ),
            &Expression::Value(_) => ast.clone(),
        },
    }
}

fn step_ast(ast: &Ast<Expression>) -> Result<Ast<Expression>, Error> {
    let left_loc = ast.left_loc;
    let right_loc = ast.right_loc;

    match &*ast.expr {
        &Expression::Value(Value::Tuple(ref vec)) => {
            let mut stepped_tup = Vec::new();

            for value in vec {
                if let &Expression::Value(_) = &*value.expr {
                    stepped_tup.push(value.clone());
                } else {
                    stepped_tup.push(step_ast(value)?);
                }
            }

            Ok(Ast::<Expression>::new(
                left_loc,
                right_loc,
                Expression::Value(Value::Tuple(stepped_tup)),
            ))
        }
        &Expression::Value(_) => Ok(ast.clone()),
        &Expression::Ident(ref name) => Err(Error::LRLocated {
            message: format!(
                "Am I supposed to read your god damn mind?  What's a `{}`?",
                name
            ),
            left_loc,
            right_loc,
        }),
        &Expression::BinOp(ref operation, ref left_ast, ref right_ast) => match (
            &*left_ast.expr,
            &*right_ast.expr,
        ) {
            (&Expression::Value(ref left), &Expression::Value(ref right))
                if left_ast.is_term() && right_ast.is_term() =>
            {
                match operation {
                    &BinOp::Call => {
                        if let &Value::Hook(ref name, _) = left {
                            match name.as_str() {
                                "list_push" => {
                                    if let &Value::Tuple(ref right_vec) = right {
                                        let el = &right_vec[0];
                                        let list = &right_vec[1];

                                        if let &Expression::Value(Value::List(ref vec)) =
                                            &*list.expr
                                        {
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
                                        panic!("list_push arg should be tuple.")
                                    }
                                }
                                "list_is_empty" => {
                                    if let &Value::List(ref vec) = right {
                                        Ok(Ast::<Expression>::new(
                                            left_loc,
                                            right_loc,
                                            Expression::Value(Value::Bool(vec.is_empty())),
                                        ))
                                    } else {
                                        panic!("list_is_empty arg should be list.")
                                    }
                                }
                                "list_head" => {
                                    if let &Value::List(ref vec) = right {
                                        if vec.is_empty() {
                                            Err(Error::LRLocated {
                                                message: String::from(
                                                    "As always, you can't get head.\n\
                                                     Especially not from an empty list.",
                                                ),
                                                left_loc: right_ast.left_loc,
                                                right_loc: right_ast.right_loc,
                                            })
                                        } else {
                                            Ok(vec[0].clone())
                                        }
                                    } else {
                                        panic!("list_head arg should be list.")
                                    }
                                }
                                "list_tail" => {
                                    if let &Value::List(ref vec) = right {
                                        if vec.is_empty() {
                                            Err(Error::LRLocated {
                                                message: String::from(
                                                    "There's no tail on that list.",
                                                ),
                                                left_loc: right_ast.left_loc,
                                                right_loc: right_ast.right_loc,
                                            })
                                        } else {
                                            Ok(Ast::<Expression>::new(
                                                left_loc,
                                                right_loc,
                                                Expression::Value(Value::List(Vec::from(
                                                    &vec[1..],
                                                ))),
                                            ))
                                        }
                                    } else {
                                        panic!("list_tail arg should be list.")
                                    }
                                }
                                _ => Err(Error::LRLocated {
                                    message: format!(
                                        "`{}` ain't gonna hook up with your ugly ass.\n\
                                         'Cuz it's not a hook.",
                                        name
                                    ),
                                    left_loc: left_ast.left_loc,
                                    right_loc: left_ast.right_loc,
                                }),
                            }
                        } else if let &Value::Func(ref pattern, _, ref body) = left {
                            Ok(substitute(
                                body,
                                pattern,
                                &Ast {
                                    expr: Box::new(Expression::Value(right.clone())),
                                    left_loc: right_ast.left_loc,
                                    right_loc: right_ast.right_loc,
                                },
                            ))
                        } else {
                            Err(Error::LRLocated {
                                message: String::from(
                                    "I only call two things on a regular basis: functions, and your mom.\
                                    \nThat's not a function."
                                ),
                                left_loc,
                                right_loc,
                            })
                        }
                    }
                    &BinOp::Add => evaluate_number_operator(ast, &left, &right, |a, b| a + b),
                    &BinOp::Sub => evaluate_number_operator(ast, &left, &right, |a, b| a - b),
                    &BinOp::Mul => evaluate_number_operator(ast, &left, &right, |a, b| a * b),
                    &BinOp::Div => match (left, right) {
                        (&Value::Int(_), &Value::Int(0)) => Err(Error::LRLocated {
                            message: String::from("Fuck off with your divide-by-zero bullshit."),
                            left_loc,
                            right_loc,
                        }),
                        (left_value, right_value) => {
                            evaluate_number_operator(ast, left_value, right_value, |a, b| a / b)
                        }
                    },
                    &BinOp::LEq => Ok(Ast {
                        expr: Box::new(Expression::Value(Value::Bool(match (left, right) {
                            (&Value::Int(left_value), &Value::Int(right_value)) => {
                                Ok(left_value <= right_value)
                            }
                            (&Value::Float(left_value), &Value::Int(right_value)) => {
                                Ok(left_value <= right_value as f64)
                            }
                            (&Value::Int(left_value), &Value::Float(right_value)) => {
                                Ok(left_value as f64 <= right_value)
                            }
                            (&Value::Float(left_value), &Value::Float(right_value)) => {
                                Ok(left_value <= right_value)
                            }
                            _ => Err(Error::LRLocated {
                                message: String::from("Fuck off with your non-number bullshit."),
                                left_loc,
                                right_loc,
                            }),
                        }?))),
                        left_loc,
                        right_loc,
                    }),
                }
            }
            (&Expression::Value(_), _) if left_ast.is_term() => Ok(Ast {
                expr: Box::new(Expression::BinOp(
                    operation.clone(),
                    left_ast.clone(),
                    step_ast(right_ast)?,
                )),
                left_loc,
                right_loc,
            }),
            _ => Ok(Ast {
                expr: Box::new(Expression::BinOp(
                    operation.clone(),
                    step_ast(left_ast)?,
                    right_ast.clone(),
                )),
                left_loc,
                right_loc,
            }),
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
            _ => Ok(Ast {
                expr: Box::new(Expression::If(
                    step_ast(guard)?,
                    consequent.clone(),
                    alternate.clone(),
                )),
                left_loc,
                right_loc,
            }),
        },
        &Expression::Let(ref bind_pattern, ref bind_value, ref body) => if bind_value.is_term() {
            // TODO: improve error message when doing non-function recursion
            Ok(substitute(
                body,
                bind_pattern,
                &substitute(
                    bind_value,
                    bind_pattern,
                    &Ast::<Expression>::new(
                        bind_value.left_loc,
                        bind_value.right_loc,
                        Expression::Let(
                            bind_pattern.clone(),
                            bind_value.clone(),
                            bind_value.clone(),
                        ),
                    ),
                ),
            ))
        } else {
            Ok(Ast {
                expr: Box::new(Expression::Let(
                    bind_pattern.clone(),
                    step_ast(bind_value)?,
                    body.clone(),
                )),
                left_loc,
                right_loc,
            })
        },
    }
}

pub fn get_eval_steps(
    ast: &Ast<Expression>,
    globals: &HashMap<String, (Value, Type)>,
) -> Result<Vec<Ast<Expression>>, Error> {
    let mut ast = ast.clone();

    for (name, &(ref value, _)) in globals {
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
            panic!("Infinite loop");
        }

        ast = stepped_ast;
    }
}

pub fn evaluate_ast(
    ast: &Ast<Expression>,
    globals: &HashMap<String, (Value, Type)>,
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
        globals.insert(String::from("global_val"), (Value::Int(5), Type::Int));

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
    fn divides_integers() {
        assert_eval_eq("3 / 2", "1");
    }

    #[test]
    fn divides_floats() {
        assert_eval_eq("3.0 / 2", "1.5");
    }

    #[test]
    fn does_not_divide_ints_by_zero() {
        assert_eval_err("3 / 0");
        assert_eval_err("0 / 0");
    }

    #[test]
    fn divides_floats_by_zero() {
        assert_eval_eq("3.0 / 0", "inf");
        assert_eval_eq("0.0 / 0", "nan");
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
            "(inc: (Int -> Int) -Int-> inc <| 5) <| (x: Int -Int-> x + 1)",
            "6",
        );
    }

    #[test]
    fn substitutes_with_a_let_expression() {
        assert_eval_eq(
            "
            let added_val: Int <== 5
            ((inc: (Int -> Int) -Int-> inc <| added_val) <| (x: Int -Int-> x + 1))
            ",
            "6",
        );
    }

    #[test]
    fn substitutes_nested_let_expression() {
        assert_eval_eq(
            "
            let x: Int <== 5
            let y: Int <== x + 1
            y
            ",
            "6",
        );
    }

    #[test]
    fn substitutes_in_tuples() {
        assert_eval_eq(
            "
            let x: Int <== 5
            let tup: (Int Float) <== (x 2.3)
            tup
            ",
            "(5 2.3)",
        );
    }

    #[test]
    fn destructures_tuples() {
        assert_eval_eq(
            "
            let x: Int <== 5
            let (a: Int  b: Float) <== (x 2.3)
            a + b
            ",
            "7.3",
        );
        assert_eval_eq(
            "
            (2  5.3  #true ()) |> (fn (a: Int  b: Float  bool: Bool) -Float-> a + b)
            ",
            "7.3",
        );
        assert_eval_eq(
            "
            () |> (fn () -Int-> 2 + 3)
            ",
            "5",
        );
    }

    #[test]
    fn supports_recursion_in_the_let_expression() {
        assert_eval_eq(
            "
            let factorial: (Int -> Int) <== n: Int -Int->
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
            "(x: Int -(Int -> Int)-> x: Int -Int-> x + 1) <| 5",
            "(fn x: Int -Int-> ((x) + 1))",
        );
    }

    #[test]
    fn list_push_hook() {
        assert_eval_eq(
            r#"
            (1 [2 3]) |> @hook("list_push"  ((Int  [Int..]) -> [Int..]))
            "#,
            "[1 2 3]",
        );
        assert_eval_eq(
            r#"
            (1 []) |> @hook("list_push"  ((Int  [Int..]) -> [Int..]))
            "#,
            "[1]",
        );
        assert_eval_eq(
            r#"
            (1.6 [2.1 3.7]) |> @hook("list_push"  ((Float  [Float..]) -> [Float..]))
            "#,
            "[1.6 2.1 3.7]",
        );
    }

    #[test]
    fn list_is_empty_hook() {
        assert_eval_eq(
            r#"
            [1 2 3] |> @hook("list_is_empty"  ([Int..] -> Bool))
            "#,
            "#false ()",
        );
        assert_eval_eq(
            r#"
            [] |> @hook("list_is_empty"  ([Int..] -> Bool))
            "#,
            "#true ()",
        );
        assert_eval_eq(
            r#"
            [#true() #false()] |> @hook("list_is_empty"  ([Bool..] -> Bool))
            "#,
            "#false ()",
        );
        assert_eval_eq(
            r#"
            [] |> @hook("list_is_empty"  ([Float..] -> Bool))
            "#,
            "#true ()",
        );
    }

    #[test]
    fn list_head_hook() {
        assert_eval_eq(
            r#"
            [1 2 3] |> @hook("list_head"  ([Int..] -> Int))
            "#,
            "1",
        );
        assert_eval_err(
            r#"
            [] |> @hook("list_head"  ([Int..] -> Int))
            "#,
        );
        assert_eval_eq(
            r#"
            [#true() #false()] |> @hook("list_head"  ([Bool..] -> Bool))
            "#,
            "#true ()",
        );
    }

    #[test]
    fn list_tail_hook() {
        assert_eval_eq(
            r#"
            [1 2 3] |> @hook("list_tail"  ([Int..] -> Int))
            "#,
            "[2 3]",
        );
        assert_eval_err(
            r#"
            [] |> @hook("list_tail"  ([Int..] -> Int))
            "#,
        );
        assert_eval_eq(
            r#"
            [#true() #false()] |> @hook("list_tail"  ([Bool..] -> Bool))
            "#,
            "[#false ()]",
        );
    }
}
