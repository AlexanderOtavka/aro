use super::parse::Expression;

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    NaN,
}

fn evaluate_number_operator<F>(left: &Value, right: &Value, evaluate: F) -> Result<Value, String>
where
    F: Fn(i32, i32) -> i32,
{
    match (left, right) {
        (&Value::Int(left_value), &Value::Int(right_value)) => {
            Ok(Value::Int(evaluate(left_value, right_value)))
        }
        (&Value::Int(_), &Value::NaN)
        | (&Value::NaN, &Value::Int(_))
        | (&Value::NaN, &Value::NaN) => Ok(Value::NaN),
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

    #[test]
    fn it_turns_nan_add_inputs_to_nan_add_outputs() {
        assert_eq!(
            evaluate_number_operator(&Value::NaN, &Value::Int(4), |a, b| a + b).unwrap(),
            Value::NaN
        );
        assert_eq!(
            evaluate_number_operator(&Value::Int(4), &Value::NaN, |a, b| a + b).unwrap(),
            Value::NaN
        );
        assert_eq!(
            evaluate_number_operator(&Value::NaN, &Value::NaN, |a, b| a + b).unwrap(),
            Value::NaN
        );
    }
}

pub fn evaluate_expression(expression: Box<Expression>) -> Result<Value, String> {
    let expr = *expression;
    match expr {
        Expression::Int(value) => Ok(Value::Int(value)),
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
                (Value::Int(0), Value::Int(0)) => Ok(Value::NaN), // I hate this
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
    fn it_divides() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(2)),
            ))).unwrap(),
            Value::Int(1)
        )
    }

    #[test]
    fn it_does_not_divide_by_zero() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(0)),
            ))).unwrap_err(),
            "Fuck off with your divide-by-zero bullshit."
        )
    }

    #[test]
    fn it_divides_zero_by_zero() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(0)),
                Box::new(Expression::Int(0)),
            ))).unwrap(),
            Value::NaN
        )
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
