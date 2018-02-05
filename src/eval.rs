use super::parse::Expression;

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    NaN,
}

pub fn evaluate_expression(expression: Box<Expression>) -> Value {
    let expr = *expression;
    match expr {
        Expression::Int(value) => Value::Int(value),
        Expression::Bool(value) => Value::Bool(value),
        Expression::Add(left, right) => {
            match (evaluate_expression(left), evaluate_expression(right)) {
                (Value::Int(left_value), Value::Int(right_value)) => {
                    Value::Int(left_value + right_value)
                }
                _ => Value::NaN,
            }
        }
        Expression::Subtract(left, right) => {
            match (evaluate_expression(left), evaluate_expression(right)) {
                (Value::Int(left_value), Value::Int(right_value)) => {
                    Value::Int(left_value - right_value)
                }
                _ => Value::NaN,
            }
        }
        Expression::Multiply(left, right) => {
            match (evaluate_expression(left), evaluate_expression(right)) {
                (Value::Int(left_value), Value::Int(right_value)) => {
                    Value::Int(left_value * right_value)
                }
                _ => Value::NaN,
            }
        }
        Expression::Divide(left, right) => {
            match (evaluate_expression(left), evaluate_expression(right)) {
                (Value::Int(_), Value::Int(0)) => Value::NaN,
                (Value::Int(left_value), Value::Int(right_value)) => {
                    Value::Int(left_value / right_value)
                }
                _ => Value::NaN,
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
            evaluate_expression(Box::new(Expression::Int(5))),
            Value::Int(5)
        )
    }

    #[test]
    fn it_spits_back_out_a_bool() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Bool(true))),
            Value::Bool(true)
        )
    }

    #[test]
    fn it_adds() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Add(
                Box::new(Expression::Int(1)),
                Box::new(Expression::Int(2)),
            ))),
            Value::Int(3)
        )
    }

    #[test]
    fn it_adds_negative_numbers() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Add(
                Box::new(Expression::Int(-1)),
                Box::new(Expression::Int(-2)),
            ))),
            Value::Int(-3)
        )
    }

    #[test]
    fn it_subtracts() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Subtract(
                Box::new(Expression::Int(2)),
                Box::new(Expression::Int(1)),
            ))),
            Value::Int(1)
        )
    }

    #[test]
    fn it_multiplies() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Multiply(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(2)),
            ))),
            Value::Int(6)
        )
    }

    #[test]
    fn it_divides() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(2)),
            ))),
            Value::Int(1)
        )
    }

    #[test]
    fn it_divides_by_zero() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(0)),
            ))),
            Value::NaN
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
            ))),
            Value::Int(7)
        )
    }
}
