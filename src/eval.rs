use super::parse::Expression;

pub fn evaluate_expression(expression: Box<Expression>) -> i32 {
    let expr = *expression;
    match expr {
        Expression::Int(value) => value,
        Expression::Add(left, right) => evaluate_expression(left) + evaluate_expression(right),
        Expression::Subtract(left, right) => evaluate_expression(left) - evaluate_expression(right),
        Expression::Multiply(left, right) => evaluate_expression(left) * evaluate_expression(right),
        Expression::Divide(left, right) => evaluate_expression(left) / evaluate_expression(right),
    }
}

#[cfg(test)]
mod test_evaluate_expression {
    use super::*;

    #[test]
    fn it_spits_back_out_a_value() {
        assert_eq!(evaluate_expression(Box::new(Expression::Int(5))), 5)
    }

    #[test]
    fn it_adds() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Add(
                Box::new(Expression::Int(1)),
                Box::new(Expression::Int(2)),
            ))),
            3
        )
    }

    #[test]
    fn it_subtracts() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Subtract(
                Box::new(Expression::Int(2)),
                Box::new(Expression::Int(1)),
            ))),
            1
        )
    }

    #[test]
    fn it_multiplies() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Multiply(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(2)),
            ))),
            6
        )
    }

    #[test]
    fn it_divides() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Divide(
                Box::new(Expression::Int(3)),
                Box::new(Expression::Int(2)),
            ))),
            1
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
            7
        )
    }
}
