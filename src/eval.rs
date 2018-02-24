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
            Value::Int(6)
        );
    }

    #[test]
    fn operates_on_two_floats() {
        assert_eq!(
            evaluate_number_operator(&Value::Float(2.1), &Value::Float(4.3), |a, b| a + b).unwrap(),
            Value::Float(6.4)
        );
    }

    #[test]
    fn operates_on_mixed_ints_and_floats() {
        assert_eq!(
            evaluate_number_operator(&Value::Int(2), &Value::Float(4.3), |a, b| a + b).unwrap(),
            Value::Float(6.3)
        );
        assert_eq!(
            evaluate_number_operator(&Value::Float(2.3), &Value::Int(4), |a, b| a + b).unwrap(),
            Value::Float(6.3)
        );
    }

    #[test]
    fn doesnt_add_bools() {
        assert_eq!(
            evaluate_number_operator(&Value::Bool(true), &Value::Int(4), |a, b| a + b).unwrap_err(),
            "Fuck off with your non-number bullshit."
        );
        assert_eq!(
            evaluate_number_operator(&Value::Int(4), &Value::Bool(true), |a, b| a + b).unwrap_err(),
            "Fuck off with your non-number bullshit."
        );
        assert_eq!(
            evaluate_number_operator(&Value::Bool(true), &Value::Bool(true), |a, b| a + b)
                .unwrap_err(),
            "Fuck off with your non-number bullshit."
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

pub fn evaluate_expression(ast: Ast) -> Result<Value, Error> {
    let expr = *ast.expr;
    let left_loc = ast.left_loc;
    let right_loc = ast.right_loc;

    match expr {
        Expression::Value(value) => Ok(value),
        Expression::Ident(name) => panic!(),
        Expression::BinOp(operation, left, right) => match operation {
            BinOp::Add => evaluate_number_operator(
                &evaluate_expression(left)?,
                &evaluate_expression(right)?,
                |a, b| a + b,
            ),
            BinOp::Sub => evaluate_number_operator(
                &evaluate_expression(left)?,
                &evaluate_expression(right)?,
                |a, b| a - b,
            ),
            BinOp::Mul => evaluate_number_operator(
                &evaluate_expression(left)?,
                &evaluate_expression(right)?,
                |a, b| a * b,
            ),
            BinOp::Div => match (evaluate_expression(left)?, evaluate_expression(right)?) {
                (Value::Int(_), Value::Int(0)) => {
                    Err(String::from("Fuck off with your divide-by-zero bullshit."))
                }
                (left_value, right_value) => {
                    evaluate_number_operator(&left_value, &right_value, |a, b| a / b)
                }
            },
            BinOp::LEq => match (evaluate_expression(left)?, evaluate_expression(right)?) {
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
            BinOp::Call => panic!(),
        }.map_err(|message| {
            (Error::LRLocated {
                message: message.clone(),
                left_loc,
                right_loc,
            })
        }),
        Expression::If(guard, consequent, alternate) => match evaluate_expression(guard)? {
            Value::Bool(guard_value) => {
                evaluate_expression(if guard_value { consequent } else { alternate })
            }
            _ => Err(Error::LRLocated {
                message: String::from("This isn't JavaScript.  Only bools in `if`s.  Dumbass."),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            }),
        },
        Expression::Func(param, body) => panic!(),
    }
}

#[cfg(test)]
mod evaluate_expression {
    use super::*;

    pub fn ast(expr: Expression) -> Ast {
        Ast::new(0, 0, expr)
    }

    #[test]
    fn spits_back_out_an_int() {
        assert_eq!(
            evaluate_expression(ast(Expression::Value(Value::Int(5)))).unwrap(),
            Value::Int(5)
        )
    }

    #[test]
    fn spits_back_out_a_bool() {
        assert_eq!(
            evaluate_expression(ast(Expression::Value(Value::Bool(true)))).unwrap(),
            Value::Bool(true)
        )
    }

    #[test]
    fn adds() {
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Add,
                ast(Expression::Value(Value::Int(1))),
                ast(Expression::Value(Value::Int(2))),
            ))).unwrap(),
            Value::Int(3)
        )
    }

    #[test]
    fn adds_negative_numbers() {
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Add,
                ast(Expression::Value(Value::Int(-1))),
                ast(Expression::Value(Value::Int(-2))),
            ))).unwrap(),
            Value::Int(-3)
        )
    }

    #[test]
    fn subtracts() {
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Sub,
                ast(Expression::Value(Value::Int(2))),
                ast(Expression::Value(Value::Int(1))),
            ))).unwrap(),
            Value::Int(1)
        )
    }

    #[test]
    fn multiplies() {
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Mul,
                ast(Expression::Value(Value::Int(3))),
                ast(Expression::Value(Value::Int(2))),
            ))).unwrap(),
            Value::Int(6)
        )
    }

    #[test]
    fn divides_integers() {
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Div,
                ast(Expression::Value(Value::Int(3))),
                ast(Expression::Value(Value::Int(2))),
            ))).unwrap(),
            Value::Int(1)
        )
    }

    #[test]
    fn divides_floats() {
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Div,
                ast(Expression::Value(Value::Float(3.0))),
                ast(Expression::Value(Value::Int(2))),
            ))).unwrap(),
            Value::Float(1.5)
        )
    }

    #[test]
    fn does_not_divide_ints_by_zero() {
        assert!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Div,
                ast(Expression::Value(Value::Int(3))),
                ast(Expression::Value(Value::Int(0))),
            ))).is_err()
        );
        assert!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Div,
                ast(Expression::Value(Value::Int(0))),
                ast(Expression::Value(Value::Int(0))),
            ))).is_err()
        );
    }

    #[test]
    fn divides_zero_by_zero() {
        if let Value::Float(num) = evaluate_expression(ast(Expression::BinOp(
            BinOp::Div,
            ast(Expression::Value(Value::Float(0.0))),
            ast(Expression::Value(Value::Int(0))),
        ))).unwrap()
        {
            assert!(num.is_nan());
        } else {
            panic!()
        }
    }

    #[test]
    fn returns_the_consequent_of_an_if() {
        assert_eq!(
            evaluate_expression(ast(Expression::If(
                ast(Expression::BinOp(
                    BinOp::LEq,
                    ast(Expression::Value(Value::Int(1))),
                    ast(Expression::Value(Value::Int(1))),
                )),
                ast(Expression::Value(Value::Int(1))),
                ast(Expression::Value(Value::Int(2))),
            ))).unwrap(),
            Value::Int(1)
        )
    }

    #[test]
    fn returns_the_alternate_of_an_if() {
        assert_eq!(
            evaluate_expression(ast(Expression::If(
                ast(Expression::Value(Value::Bool(false))),
                ast(Expression::Value(Value::Int(1))),
                ast(Expression::Value(Value::Int(2))),
            ))).unwrap(),
            Value::Int(2)
        )
    }

    #[test]
    fn requires_a_bool_if_guard() {
        assert!(
            evaluate_expression(ast(Expression::If(
                ast(Expression::Value(Value::Int(1))),
                ast(Expression::Value(Value::Int(2))),
                ast(Expression::Value(Value::Int(3))),
            ))).is_err()
        )
    }

    #[test]
    fn handles_a_nested_tree() {
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::Add,
                ast(Expression::Value(Value::Int(1))),
                ast(Expression::BinOp(
                    BinOp::Mul,
                    ast(Expression::Value(Value::Int(2))),
                    ast(Expression::Value(Value::Int(3))),
                ))
            ))).unwrap(),
            Value::Int(7)
        )
    }

    #[test]
    fn compares_with_leq() {
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::LEq,
                ast(Expression::Value(Value::Int(3))),
                ast(Expression::Value(Value::Int(2))),
            ))).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            evaluate_expression(ast(Expression::BinOp(
                BinOp::LEq,
                ast(Expression::Value(Value::Int(2))),
                ast(Expression::Value(Value::Int(2))),
            ))).unwrap(),
            Value::Bool(true)
        );
    }
}
