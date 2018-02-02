use super::parse::Expression;

pub fn evaluate_expression(expression: Box<Expression>) -> i32 {
    let expr = *expression;
    match expr {
        Expression::Int(value) => value,
        Expression::Add(left, right) => evaluate_expression(left) + evaluate_expression(right),
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
    fn it_handles_a_deeply_nested_plus() {
        assert_eq!(
            evaluate_expression(Box::new(Expression::Add(
                Box::new(Expression::Int(1)),
                Box::new(Expression::Add(
                    Box::new(Expression::Int(2)),
                    Box::new(Expression::Int(3)),
                ))
            ))),
            6
        )
    }
}
