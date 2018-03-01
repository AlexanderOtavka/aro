use ast::{Ast, BinOp, Expression, Type, Value};
use util::Error;
use std::collections::HashMap;

impl Error {
    pub fn type_error(left_loc: usize, right_loc: usize, expected: &Type, actual: &Type) -> Error {
        Error::LRLocated {
            message: format!(
                "Did you have trouble matching shapes as a toddler?\n\
                 Looks like you can't match types either.\n\
                 Expected type `{}` but saw `{}`.",
                expected, actual
            ),
            left_loc,
            right_loc,
        }
    }
}

pub fn typecheck_ast(ast: &Ast, env: &HashMap<String, Type>) -> Result<Type, Error> {
    let left_loc = ast.left_loc;
    let right_loc = ast.right_loc;

    match &*ast.expr {
        &Expression::Value(ref value) => match value {
            &Value::Int(_) => Ok(Type::Int),
            &Value::Float(_) => Ok(Type::Float),
            &Value::Bool(_) => Ok(Type::Bool),
            &Value::Func(ref param_name, ref param_type, ref body_type, ref body) => {
                let mut body_env = env.clone();
                body_env.insert(param_name.clone(), *param_type.expr.clone());

                let actual_body_type = typecheck_ast(body, &body_env)?;
                if *body_type.expr != actual_body_type {
                    Err(Error::type_error(
                        body.left_loc,
                        body.right_loc,
                        &*body_type.expr,
                        &actual_body_type,
                    ))
                } else {
                    Ok(Type::Func(param_type.clone(), body_type.clone()))
                }
            }
        },
        &Expression::Ident(ref name) => if let Some(ident_type) = env.get(name) {
            Ok(ident_type.clone())
        } else {
            Err(Error::LRLocated {
                message: format!(
                    "Am I supposed to read your god damn mind?  What's a `{}`?",
                    name
                ),
                left_loc,
                right_loc,
            })
        },
        &Expression::If(ref condition, ref consequent, ref alternate) => {
            let condition_type = typecheck_ast(condition, env)?;
            if condition_type != Type::Bool {
                Err(Error::type_error(
                    alternate.left_loc,
                    alternate.right_loc,
                    &Type::Bool,
                    &condition_type,
                ))
            } else {
                let consequent_type = typecheck_ast(consequent, env)?;
                let alternate_type = typecheck_ast(alternate, env)?;

                if consequent_type != alternate_type {
                    Err(Error::type_error(
                        alternate.left_loc,
                        alternate.right_loc,
                        &consequent_type,
                        &alternate_type,
                    ))
                } else {
                    Ok(consequent_type)
                }
            }
        }
        &Expression::Let(ref name, ref value_type, ref value, ref body) => {
            let mut body_env = env.clone();
            body_env.insert(name.clone(), *value_type.expr.clone());

            let actual_value_type = typecheck_ast(value, &body_env)?;
            if actual_value_type != *value_type.expr {
                Err(Error::type_error(
                    value.left_loc,
                    value.right_loc,
                    &*value_type.expr,
                    &actual_value_type,
                ))
            } else {
                typecheck_ast(body, &body_env)
            }
        }
        &Expression::BinOp(ref operation, ref left, ref right) => match operation {
            &BinOp::Call => {
                let left_type = typecheck_ast(left, env)?;
                let right_type = typecheck_ast(right, env)?;

                if let Type::Func(ref param_type, ref output_type) = left_type {
                    if right_type != *param_type.expr {
                        Err(Error::type_error(
                            right.left_loc,
                            right.right_loc,
                            &*param_type.expr,
                            &right_type,
                        ))
                    } else {
                        Ok(*output_type.expr.clone())
                    }
                } else {
                    Err(Error::LRLocated {
                        message: String::from(
                            "I only call two things on a regular basis: functions, and your mom.\n\
                             That's not a function.",
                        ),
                        left_loc: left.left_loc,
                        right_loc: left.right_loc,
                    })
                }
            }
            &BinOp::Add | &BinOp::Sub | &BinOp::Mul | &BinOp::Div | &BinOp::LEq => {
                let left_type = typecheck_ast(left, env)?;
                let right_type = typecheck_ast(right, env)?;

                if left_type != Type::Int && left_type != Type::Float {
                    Err(Error::LRLocated {
                        message: format!(
                            "Did you think this was python?  You can't do math with `{}`s.",
                            left_type
                        ),
                        left_loc: left.left_loc,
                        right_loc: left.right_loc,
                    })
                } else if right_type != Type::Int && right_type != Type::Float {
                    Err(Error::LRLocated {
                        message: format!(
                            "Did you think this was python?  You can't do math with `{}`s.",
                            right_type
                        ),
                        left_loc: right.left_loc,
                        right_loc: right.right_loc,
                    })
                } else {
                    match operation {
                        &BinOp::LEq => Ok(Type::Bool),
                        &BinOp::Add | &BinOp::Sub | &BinOp::Mul | &BinOp::Div => {
                            if (left_type, right_type) == (Type::Int, Type::Int) {
                                Ok(Type::Int)
                            } else {
                                Ok(Type::Float)
                            }
                        }
                        _ => panic!("Cannot typecheck operation"),
                    }
                }
            }
        },
    }
}

#[cfg(test)]
mod typecheck_ast {
    use super::*;
    use parse::source_to_ast;

    fn assert_typecheck_eq(actual: &str, expected: &str) {
        assert_eq!(
            format!(
                "{}",
                typecheck_ast(&source_to_ast(actual).unwrap(), &HashMap::new()).unwrap()
            ),
            expected
        );
    }

    fn assert_typecheck_err(source: &str) {
        assert!(typecheck_ast(&source_to_ast(source).unwrap(), &HashMap::new()).is_err());
    }

    #[test]
    fn checks_an_int_literal() {
        assert_typecheck_eq("5", "Int");
    }

    #[test]
    fn checks_a_bool_literal() {
        assert_typecheck_eq("#true ()", "Bool");
        assert_typecheck_eq("#false ()", "Bool");
    }

    #[test]
    fn doesnt_handle_straight_identifiers() {
        assert_typecheck_err("foo");
    }

    #[test]
    fn checks_arithmatic() {
        assert_typecheck_eq("1 + 2", "Int");
        assert_typecheck_eq("-1.0 / -2", "Float");
        assert_typecheck_eq("1.0 * 2.0", "Float");
    }

    #[test]
    fn checks_nested_arithmatic() {
        assert_typecheck_eq("1 + (2 * 3)", "Int");
    }

    #[test]
    fn checks_leq() {
        assert_typecheck_eq("3 <= 2", "Bool");
    }

    #[test]
    fn checks_an_if() {
        assert_typecheck_eq("if 1 <= 1 then 1 else 2", "Int");
    }

    #[test]
    fn requires_a_bool_if_guard() {
        assert_typecheck_err("if 1 then 2 else 3");
    }

    #[test]
    fn checks_a_nested_function_call() {
        assert_typecheck_eq(
            "(inc: (Int -> Int) -Int-> inc <| 5) <| (x: Int -Int-> x + 1)",
            "Int",
        );
    }

    #[test]
    fn checks_a_let_expression() {
        assert_typecheck_eq(
            "
            let added_val: Int <== 5
            ((inc: (Int -> Int) -Int-> inc <| added_val) <| (x: Int -Int-> x + 1))
            ",
            "Int",
        );
    }

    #[test]
    fn checks_nested_let_expression() {
        assert_typecheck_eq(
            "
            let x: Int <== 5
            let y: Int <== x + 1
            y
            ",
            "Int",
        );
    }

    #[test]
    fn supports_recursion_in_the_let_expression() {
        assert_typecheck_eq(
            "
            let factorial: (Int -> Int) <== n: Int -Int->
                if n <= 0 then
                    1
                else
                    n * (factorial <| n - 1)
            factorial <| 5
            ",
            "Int",
        );
    }

    #[test]
    fn checks_iifes() {
        assert_typecheck_eq(
            "(x: Int -(Int -> Int)-> y: Int -Int-> x + y) <| 5",
            "(Int -> Int)",
        );
    }
}