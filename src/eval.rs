use ast::{Ast, BinOp, Expression, Value};
use util::Error;

fn evaluate_number_operator<F>(left: &Value, right: &Value, evaluate: F) -> Result<Value, String>
where
    F: Fn(f64, f64) -> f64,
{
    match (left, right) {
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
        _ => Err(String::from("Fuck off with your non-number bullshit.")),
    }
}

#[cfg(test)]
mod evaluate_number_operator {
    use super::*;
    use std::f64::NAN;

    #[test]
    fn operates_on_two_ints() {
        assert_eq!(
            evaluate_number_operator(&Value::Int(2), &Value::Int(4), |a, b| a + b).unwrap(),
            Value::Int(6),
        );
    }

    #[test]
    fn operates_on_two_floats() {
        assert_eq!(
            evaluate_number_operator(&Value::Float(2.1), &Value::Float(4.3), |a, b| a + b).unwrap(),
            Value::Float(6.4),
        );
    }

    #[test]
    fn operates_on_mixed_ints_and_floats() {
        assert_eq!(
            evaluate_number_operator(&Value::Int(2), &Value::Float(4.3), |a, b| a + b).unwrap(),
            Value::Float(6.3),
        );
        assert_eq!(
            evaluate_number_operator(&Value::Float(2.3), &Value::Int(4), |a, b| a + b).unwrap(),
            Value::Float(6.3),
        );
    }

    #[test]
    fn doesnt_add_bools() {
        assert_eq!(
            evaluate_number_operator(&Value::Bool(true), &Value::Int(4), |a, b| a + b).unwrap_err(),
            "Fuck off with your non-number bullshit.",
        );
        assert_eq!(
            evaluate_number_operator(&Value::Int(4), &Value::Bool(true), |a, b| a + b).unwrap_err(),
            "Fuck off with your non-number bullshit.",
        );
        assert_eq!(
            evaluate_number_operator(&Value::Bool(true), &Value::Bool(true), |a, b| a + b)
                .unwrap_err(),
            "Fuck off with your non-number bullshit.",
        );
    }

    fn val_is_nan(value: Value) -> bool {
        if let Value::Float(num) = value {
            num.is_nan()
        } else {
            false
        }
    }

    #[test]
    fn turns_nan_add_inputs_to_nan_add_outputs() {
        assert!(val_is_nan(
            evaluate_number_operator(&Value::Float(NAN), &Value::Int(4), |a, b| a + b).unwrap(),
        ));
        assert!(val_is_nan(
            evaluate_number_operator(&Value::Int(4), &Value::Float(NAN), |a, b| a + b).unwrap(),
        ));
        assert!(val_is_nan(
            evaluate_number_operator(&Value::Float(NAN), &Value::Float(4.2), |a, b| a + b).unwrap(),
        ));
        assert!(val_is_nan(
            evaluate_number_operator(&Value::Float(4.2), &Value::Float(NAN), |a, b| a + b).unwrap(),
        ));
        assert!(val_is_nan(
            evaluate_number_operator(&Value::Float(NAN), &Value::Float(NAN), |a, b| a + b).unwrap(),
        ));
    }
}

fn substitute(ast: &Ast, name: &str, value: &Ast) -> Ast {
    match &*ast.expr {
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
                substitute(left, name, value),
                substitute(right, name, value),
            )),
            left_loc: ast.left_loc,
            right_loc: ast.right_loc,
        },
        &Expression::If(ref c, ref t, ref e) => Ast {
            expr: Box::new(Expression::If(
                substitute(c, name, value),
                substitute(t, name, value),
                substitute(e, name, value),
            )),
            left_loc: ast.left_loc,
            right_loc: ast.right_loc,
        },
        &Expression::Let(ref bind_name, ref bind_value, ref body) => Ast {
            expr: Box::new(Expression::Let(
                bind_name.clone(),
                bind_value.clone(),
                if bind_name == name {
                    body.clone()
                } else {
                    substitute(body, name, value)
                },
            )),
            left_loc: ast.left_loc,
            right_loc: ast.right_loc,
        },
        &Expression::Value(Value::Func(ref param_name, ref body)) => Ast {
            expr: Box::new(Expression::Value(Value::Func(
                param_name.clone(),
                if param_name == name {
                    body.clone()
                } else {
                    substitute(body, name, value)
                },
            ))),
            left_loc: ast.left_loc,
            right_loc: ast.right_loc,
        },
        &Expression::Value(_) => ast.clone(),
    }
}

pub fn evaluate_expression(ast: &Ast) -> Result<Value, Error> {
    let left_loc = ast.left_loc;
    let right_loc = ast.right_loc;

    match &*ast.expr {
        &Expression::Value(ref value) => Ok(value.clone()),
        &Expression::Ident(ref name) => Err(Error::LRLocated {
            message: format!(
                "Am I supposed to read your god damn mind?  What's a `{}`?",
                name
            ),
            left_loc,
            right_loc,
        }),
        &Expression::BinOp(ref operation, ref left, ref right) => match operation {
            &BinOp::Add => evaluate_number_operator(
                &evaluate_expression(left)?,
                &evaluate_expression(right)?,
                |a, b| a + b,
            ),
            &BinOp::Sub => evaluate_number_operator(
                &evaluate_expression(left)?,
                &evaluate_expression(right)?,
                |a, b| a - b,
            ),
            &BinOp::Mul => evaluate_number_operator(
                &evaluate_expression(left)?,
                &evaluate_expression(right)?,
                |a, b| a * b,
            ),
            &BinOp::Div => match (evaluate_expression(left)?, evaluate_expression(right)?) {
                (Value::Int(_), Value::Int(0)) => {
                    Err(String::from("Fuck off with your divide-by-zero bullshit."))
                }
                (left_value, right_value) => {
                    evaluate_number_operator(&left_value, &right_value, |a, b| a / b)
                }
            },
            &BinOp::LEq => match (evaluate_expression(left)?, evaluate_expression(right)?) {
                (Value::Int(left_value), Value::Int(right_value)) => {
                    Ok(Value::Bool(left_value <= right_value))
                }
                (Value::Float(left_value), Value::Int(right_value)) => {
                    Ok(Value::Bool(left_value <= right_value as f64))
                }
                (Value::Int(left_value), Value::Float(right_value)) => {
                    Ok(Value::Bool(left_value as f64 <= right_value))
                }
                (Value::Float(left_value), Value::Float(right_value)) => {
                    Ok(Value::Bool(left_value <= right_value))
                }
                _ => Err(String::from("Fuck off with your non-number bullshit.")),
            },
            &BinOp::Call => {
                if let Value::Func(ref param_name, ref body) = evaluate_expression(left)? {
                    Ok(evaluate_expression(&substitute(
                        body,
                        param_name,
                        &Ast {
                            expr: Box::new(Expression::Value(evaluate_expression(right)?)),
                            left_loc: right.left_loc,
                            right_loc: right.right_loc,
                        },
                    ))?)
                } else {
                    Err(String::from(
                        "I only call two things on a regular basis: functions, and your mom.\
                         \nThat's not a function.",
                    ))
                }
            }
        }.map_err(|message| {
            (Error::LRLocated {
                message: message.clone(),
                left_loc,
                right_loc,
            })
        }),
        &Expression::If(ref guard, ref consequent, ref alternate) => {
            match evaluate_expression(guard)? {
                Value::Bool(guard_value) => {
                    evaluate_expression(if guard_value { consequent } else { alternate })
                }
                _ => Err(Error::LRLocated {
                    message: String::from("This isn't JavaScript.  Only bools in `if`s.  Dumbass."),
                    left_loc: ast.left_loc,
                    right_loc: ast.right_loc,
                }),
            }
        }
        &Expression::Let(ref bind_name, ref bind_value, ref body) => {
            Ok(evaluate_expression(&substitute(
                body,
                bind_name,
                &substitute(
                    bind_value,
                    bind_name,
                    &Ast {
                        expr: Box::new(Expression::Let(
                            bind_name.clone(),
                            bind_value.clone(),
                            bind_value.clone(),
                        )),
                        left_loc: bind_value.left_loc,
                        right_loc: bind_value.right_loc,
                    },
                ),
            ))?)
        }
    }
}

#[cfg(test)]
mod evaluate_expression {
    use super::*;
    use parse::source_to_ast;

    fn assert_eval_eq(source: &str, result: &str) {
        assert_eq!(
            format!(
                "{}",
                evaluate_expression(&source_to_ast(source).unwrap()).unwrap()
            ),
            result
        )
    }

    fn assert_eval_err(source: &str) {
        assert!(evaluate_expression(&source_to_ast(source).unwrap()).is_err())
    }

    #[test]
    fn spits_back_out_an_int() {
        assert_eval_eq("5", "5")
    }

    #[test]
    fn spits_back_out_a_bool() {
        assert_eval_eq("#true ()", "#true ()")
    }

    #[test]
    fn doesnt_handle_straight_identifiers() {
        assert_eval_err("foo")
    }

    #[test]
    fn adds() {
        assert_eval_eq("1 + 2", "3")
    }

    #[test]
    fn adds_negative_numbers() {
        assert_eval_eq("-1 + -2", "-3")
    }

    #[test]
    fn subtracts() {
        assert_eval_eq("2 - 1", "1")
    }

    #[test]
    fn multiplies() {
        assert_eval_eq("3 * 2", "6")
    }

    #[test]
    fn divides_integers() {
        assert_eval_eq("3 / 2", "1")
    }

    #[test]
    fn divides_floats() {
        assert_eval_eq("3.0 / 2", "1.5")
    }

    #[test]
    fn does_not_divide_ints_by_zero() {
        assert_eval_err("3 / 0");
        assert_eval_err("0 / 0");
    }

    #[test]
    fn divides_zero_by_zero() {
        assert_eval_eq("0.0 / 0", "NaN")
    }

    #[test]
    fn divides_floats_by_zero() {
        assert_eval_eq("3.0 / 0", "inf")
    }

    #[test]
    fn returns_the_consequent_of_an_if() {
        assert_eval_eq("if 1 <= 1 then 1 else 2", "1")
    }

    #[test]
    fn returns_the_alternate_of_an_if() {
        assert_eval_eq("if (#false ()) then 1 else 2", "2")
    }

    #[test]
    fn requires_a_bool_if_guard() {
        assert_eval_err("if 1 then 2 else 3")
    }

    #[test]
    fn substitutes_in_a_nested_function_call() {
        assert_eval_eq("(inc -> inc <| 5) <| (x -> x + 1)", "6")
    }

    #[test]
    fn substitutes_with_a_let_expression() {
        assert_eval_eq(
            "
            let added_val <== 5
            ((inc -> inc <| added_val) <| (x -> x + 1))
            ",
            "6",
        )
    }

    #[test]
    fn supports_recursion_in_the_let_expression() {
        assert_eval_eq(
            "
            let factorial <== n ->
                if n <= 0 then
                    1
                else
                    n * (factorial <| n - 1)

            factorial <| 5
            ",
            &format!("{}", 5 * 4 * 3 * 2 * 1),
        )
    }

    #[test]
    fn doesnt_substitute_shadowed_variables() {
        assert_eval_eq("(x -> x -> x + 1) <| 5", "(x -> ((x) + 1))")
    }

    #[test]
    fn handles_a_nested_tree() {
        assert_eval_eq("1 + (2 * 3)", "7")
    }

    #[test]
    fn compares_with_leq() {
        assert_eval_eq("3 <= 2", "#false ()");
        assert_eval_eq("2 <= 2", "#true ()");
        assert_eval_eq("1 <= 2", "#true ()");
    }
}
