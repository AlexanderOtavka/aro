use grammar::parse_Expr;
use lalrpop_util::ParseError;
use untyped_ast::{Ast, Expression};
use util::Error;

pub fn source_to_ast<'input>(source: &'input str) -> Result<Ast<Expression>, Error> {
    parse_Expr(source).map_err(|err| match err {
        ParseError::InvalidToken { location } => Error::Located {
            message: String::from("Bitch, do I look like I speak perl?"),
            loc: location,
        },
        ParseError::ExtraToken {
            token: (left_loc, token, right_loc),
        } => Error::LRLocated {
            message: format!("Hey, I have an idea, let's put `{}` everywhere!", token),
            left_loc,
            right_loc,
        },
        ParseError::UnrecognizedToken {
            token: Some((left, ref token, right)),
            ref expected,
        } => Error::LRLocated {
            message: {
                let mut message = String::new();

                message += &format!("What the fuck is a `{}` doing here?", token);

                if !expected.is_empty() {
                    message += "\n";
                    for (i, e) in expected.iter().enumerate() {
                        message += match i {
                            0 => "Expected one of ",
                            _ if i < expected.len() - 1 => ", ",
                            _ => " or ",
                        };
                        message += e;
                    }
                }

                message
            },
            left_loc: left,
            right_loc: right,
        },
        ParseError::UnrecognizedToken {
            token: None,
            expected: _,
        } if source.len() > 0 =>
        {
            Error::Located {
                message: String::from(
                    "Oh, so we're just ending files wherever we want now?  Think again, Dumbass!",
                ),
                loc: source.len() - 1,
            }
        }
        ParseError::UnrecognizedToken {
            token: None,
            expected: _,
        } => Error::Unlocated {
            message: String::from(
                "You're gonna have to give me something to work with.  My craft requires it!",
            ),
        },
        ParseError::User { error } => Error::Unlocated {
            message: String::from(error),
        },
    })
}

#[cfg(test)]
mod source_to_ast {
    use super::*;

    fn assert_parse_eq(actual: Result<Ast<Expression>, Error>, expected: &str) {
        assert_eq!(format!("{}", actual.unwrap()), expected);
    }

    #[test]
    fn unwraps_parens() {
        assert_parse_eq(source_to_ast("(((((5)))))"), "5");
    }

    #[test]
    fn makes_an_int_tree() {
        assert_parse_eq(source_to_ast("5"), "5");
    }

    #[test]
    fn makes_a_float_tree() {
        assert_parse_eq(source_to_ast("5.3"), "5.3");
    }

    #[test]
    fn makes_an_identifier_tree() {
        assert_parse_eq(source_to_ast("foo"), "(foo)");
        assert_parse_eq(source_to_ast("foo_bar"), "(foo_bar)");
        assert!(source_to_ast("_foo").is_err());
        assert!(source_to_ast("foo_").is_err());
        assert!(source_to_ast("foo__bar").is_err());
    }

    #[test]
    fn makes_a_nan_tree() {
        assert_parse_eq(source_to_ast("nan"), "(nan)");
    }

    #[test]
    fn makes_a_inf_tree() {
        assert_parse_eq(source_to_ast("inf"), "(inf)");
    }

    #[test]
    fn makes_a_bool_tree() {
        assert_parse_eq(source_to_ast("#true ()"), "#true ()");
        assert_parse_eq(source_to_ast("#false()"), "#false ()");
        assert_parse_eq(source_to_ast("#true ( )"), "#true ()");
    }

    #[test]
    fn makes_a_tuple_tree() {
        assert_parse_eq(source_to_ast("()"), "()");
        assert_parse_eq(source_to_ast("(1)"), "1");
        assert_parse_eq(source_to_ast("(1  #false())"), "(1 #false ())");
        assert_parse_eq(source_to_ast("(1  #false()  3.1)"), "(1 #false () 3.1)");
    }

    #[test]
    fn makes_a_simple_plus_tree() {
        assert_parse_eq(source_to_ast("2 + 5.1"), "(2 + 5.1)");
    }

    #[test]
    fn makes_a_simple_minus_tree() {
        assert_parse_eq(source_to_ast("2 - 5"), "(2 - 5)");
    }

    #[test]
    fn makes_a_simple_star_tree() {
        assert_parse_eq(source_to_ast("2 * 5"), "(2 * 5)");
    }

    #[test]
    fn makes_a_simple_slash_tree() {
        assert_parse_eq(source_to_ast("2 / 5"), "(2 / 5)");
    }

    #[test]
    fn makes_a_simple_call_tree() {
        assert_parse_eq(source_to_ast("a <| 5"), "((a) <| 5)");
    }

    #[test]
    fn makes_a_simple_function_tree() {
        assert_parse_eq(source_to_ast("a: Int =Int=> 5"), "(fn a: Int =Int=> 5)");
    }

    #[test]
    fn spits_out_type_identifiers() {
        assert_parse_eq(source_to_ast("a: Foo =Int=> 5"), "(fn a: (Foo) =Int=> 5)");
    }

    #[test]
    fn makes_an_if_tree() {
        assert_parse_eq(
            source_to_ast("if #true () then 2 else 5"),
            "(if #true () then 2 else 5)",
        );
    }

    #[test]
    fn makes_a_simple_leq_tree() {
        assert_parse_eq(source_to_ast("2 <= 5"), "(2 <= 5)");
    }

    #[test]
    fn makes_a_nested_tree() {
        assert_parse_eq(source_to_ast("2 + 3 * 5"), "(2 + (3 * 5))");
    }

    #[test]
    fn makes_a_nested_function_tree() {
        assert_parse_eq(
            source_to_ast("a: (Int => Bool) =Bool=> a <| 5 + 1"),
            "(fn a: (Int => Bool) =Bool=> ((a) <| (5 + 1)))",
        );
    }

    #[test]
    fn makes_a_let_tree() {
        assert_parse_eq(source_to_ast("let a: Int <- 5 a"), "(let a: Int <- 5 (a))");
    }

    #[test]
    fn overrides_precedence_with_parens() {
        assert_parse_eq(source_to_ast("(2 + 3) * 5"), "((2 + 3) * 5)");
    }

    #[test]
    fn complains_about_empty_input() {
        assert!(source_to_ast("").is_err());
    }
}
