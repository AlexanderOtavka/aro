use super::parse::Expression;

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f64),
    Bool(bool),
}

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
mod test_evaluate_number_operator {
    use super::*;

    #[test]
    fn it_operates_on_two_ints() {
        assert_eq!(
            evaluate_number_operator(&Value::Int(2), &Value::Int(4), |a, b| a + b).unwrap(),
            Value::Int(6)
        );
    }

    #[test]
    fn it_operates_on_two_floats() {
        assert_eq!(
            evaluate_number_operator(&Value::Float(2.1), &Value::Float(4.3), |a, b| a + b).unwrap(),
            Value::Float(6.4)
        );
    }

    #[test]
    fn it_operates_on_mixed_ints_and_floats() {
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
    fn it_doesnt_add_bools() {
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
    fn it_turns_nan_add_inputs_to_nan_add_outputs() {
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

pub fn evaluate_expression(expression: Box<Expression>) -> Result<Value, String> {
    let expr = *expression;
    match expr {
        Expression::Int(value) => Ok(Value::Int(value)),
        Expression::Float(value) => Ok(Value::Float(value)),
        Expression::Bool(value) => Ok(Value::Bool(value)),
        Expression::Add(left, right) => evaluate_number_operator(
            &evaluate_expression(left)?,
            &evaluate_expression(right)?,
            |a, b| a + b,
        ),
        Expression::Subtract(left, right) => evaluate_number_operator(
            &evaluate_expression(left)?,
            &evaluate_expression(right)?,
            |a, b| a - b,
        ),
        Expression::Multiply(left, right) => evaluate_number_operator(
            &evaluate_expression(left)?,
            &evaluate_expression(right)?,
            |a, b| a * b,
        ),
        Expression::Divide(left, right) => {
            match (evaluate_expression(left)?, evaluate_expression(right)?) {
                (Value::Int(_), Value::Int(0)) => {
                    Err(String::from("Fuck off with your divide-by-zero bullshit."))
                }
                (left_value, right_value) => {
                    evaluate_number_operator(&left_value, &right_value, |a, b| a / b)
                }
            }
        }
        Expression::If(guard, consequent, alternate) => match evaluate_expression(guard)? {
            Value::Bool(guard_value) => evaluate_expression(match guard_value {
                true => consequent,
                false => alternate,
            }),
            _ => Err(String::from(
                "This isn't JavaScript.  Only bools in `if`s.  Dumbass.",
            )),
        },
        Expression::LEq(left, right) => {
            match (evaluate_expression(left)?, evaluate_expression(right)?) {
                (Value::Int(left_value), Value::Int(right_value)) => {
                    Ok(Value::Bool(left_value <= right_value))
                }
                _ => Err(String::from("Fuck off with your non-number bullshit.")),
            }
        }
    }
}

#[cfg(test)]
mod test_evaluate_expression {
    use super::*;

    #[test]
    fn it_spits_back_out_an_int() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Int(5))).unwrap(),
            Value::Int(5)
        )
    }

    #[test]
    fn it_spits_back_out_a_bool() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Bool(true))).unwrap(),
            Value::Bool(true)
        )
    }

    #[test]
    fn it_adds() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Add(
                Box::new(Expression::Int(1)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Int(3)
        )
    }

    #[test]
    fn it_adds_negative_numbers() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Add(
                Box::new(Expression::Int(-1)),
                Box::new(Expression::Int(-2)),
            ))).unwrap(),
            Value::Int(-3)
        )
    }

    #[test]
    fn it_subtracts() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Subtract(
                Box::new(Expression::Int(2)),
                Box::new(Expression::Int(1)),
            ))).unwrap(),
            Value::Int(1)
        )
    }

    #[test]
    fn it_multiplies() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Multiply(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Int(6)
        )
    }

    #[test]
    fn it_divides_integers() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Int(1)
        )
    }

    #[test]
    fn it_divides_floats() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Float(3.0)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Float(1.5)
        )
    }

    #[test]
    fn it_does_not_divide_ints_by_zero() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(0)),
            ))).unwrap_err(),
            "Fuck off with your divide-by-zero bullshit."
        );
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(0)),
                Box::new(Expression::Int(0)),
            ))).unwrap_err(),
            "Fuck off with your divide-by-zero bullshit."
        );
    }

    #[test]
    fn it_divides_zero_by_zero() {
        if let Value::Float(num) = evaluate_expression(Box::new(Expression::Divide(
            Box::new(Expression::Float(0.0)),
            Box::new(Expression::Int(0)),
        ))).unwrap()
        {
            assert!(num.is_nan());
        } else {
            assert!(false, "Not a float value");
        }
    }

    #[test]
    fn it_returns_the_consequent_of_an_if() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::If(
                Box::new(Expression::LEq(
                    Box::new(Expression::Int(1)),
                    Box::new(Expression::Int(1)),
                )),
                Box::new(Expression::Int(1)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Int(1)
        )
    }

    #[test]
    fn it_returns_the_alternate_of_an_if() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::If(
                Box::new(Expression::Bool(false)),
                Box::new(Expression::Int(1)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Int(2)
        )
    }

    #[test]
    fn it_requires_a_bool_if_guard() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::If(
                Box::new(Expression::Int(1)),
                Box::new(Expression::Int(2)),
                Box::new(Expression::Int(3)),
            ))).unwrap_err(),
            "This isn't JavaScript.  Only bools in `if`s.  Dumbass."
        )
    }

    #[test]
    fn it_handles_a_nested_tree() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Add(
                Box::new(Expression::Int(1)),
                Box::new(Expression::Multiply(
                    Box::new(Expression::Int(2)),
                    Box::new(Expression::Int(3)),
                ))
            ))).unwrap(),
            Value::Int(7)
        )
    }

    #[test]
    fn it_compares_with_leq() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::LEq(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            evaluate_expression(Box::new(Expression::LEq(
                Box::new(Expression::Int(2)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Bool(true)
        );
    }
}
