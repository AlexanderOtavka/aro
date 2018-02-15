use ast::Expression;
use grammar::parse_Expr;

pub fn source_to_ast(source: &str) -> Result<Box<Expression>, String> {
    parse_Expr(source).map_err(|err| format!("{}", err))
}

#[cfg(test)]
mod source_to_ast {
    use super::*;
    use std::f64::INFINITY;

    #[test]
    fn makes_an_int_tree() {
        assert_eq!(*source_to_ast("5").unwrap(), Expression::Int(5));
    }

    #[test]
    fn makes_a_float_tree() {
        assert_eq!(*source_to_ast("5.3").unwrap(), Expression::Float(5.3));
    }

    #[test]
    fn makes_a_nan_tree() {
        if let Expression::Float(val) = *source_to_ast("NaN").unwrap() {
            assert!(val.is_nan());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn makes_a_inf_tree() {
        assert_eq!(*source_to_ast("inf").unwrap(), Expression::Float(INFINITY));
    }

    #[test]
    fn makes_a_bool_tree() {
        assert_eq!(*source_to_ast("true").unwrap(), Expression::Bool(true));
    }

    #[test]
    fn makes_a_simple_plus_tree() {
        assert_eq!(
            *source_to_ast("2 + 5.1").unwrap(),
            Expression::Add(
                Box::new(Expression::Int(2)),
                Box::new(Expression::Float(5.1))
            )
        );
    }

    #[test]
    fn makes_a_simple_minus_tree() {
        assert_eq!(
            *source_to_ast("2 - 5").unwrap(),
            Expression::Subtract(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
    }

    #[test]
    fn makes_a_simple_star_tree() {
        assert_eq!(
            *source_to_ast("2 * 5").unwrap(),
            Expression::Multiply(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
    }

    #[test]
    fn makes_a_simple_slash_tree() {
        assert_eq!(
            *source_to_ast("2 / 5").unwrap(),
            Expression::Divide(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
    }

    #[test]
    fn makes_an_if_tree() {
        assert_eq!(
            *source_to_ast("if true then 2 else 5").unwrap(),
            Expression::If(
                Box::new(Expression::Bool(true)),
                Box::new(Expression::Int(2)),
                Box::new(Expression::Int(5))
            )
        );
    }

    #[test]
    fn makes_a_simple_leq_tree() {
        assert_eq!(
            *source_to_ast("2 <= 5").unwrap(),
            Expression::LEq(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
    }

    #[test]
    fn makes_a_nested_tree() {
        assert_eq!(
            *source_to_ast("2 + 3 * 5").unwrap(),
            Expression::Add(
                Box::new(Expression::Int(2)),
                Box::new(Expression::Multiply(
                    Box::new(Expression::Int(3)),
                    Box::new(Expression::Int(5))
                ))
            )
        );
    }
}
