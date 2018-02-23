use ast::Ast;
use grammar::parse_Expr;
use lalrpop_util::ParseError;
use util::CompilerError;
use std::usize;

pub fn source_to_ast<'input>(source: &'input str) -> Result<Ast, CompilerError> {
    parse_Expr(source).map_err(|err| match err {
        ParseError::InvalidToken { location } => CompilerError::Located {
            message: String::from("Invalid token"),
            loc: location,
        },
        ParseError::ExtraToken {
            token: (left_loc, token, right_loc),
        } => CompilerError::LRLocated {
            message: format!("Extra token `{}`", token),
            left_loc,
            right_loc,
        },
        ParseError::UnrecognizedToken { token, expected } => CompilerError::LRLocated {
            message: {
                let mut message = String::new();

                match token {
                    Some((_, ref token, _)) => {
                        message += &format!("Unrecognized token `{}`", token);
                    }
                    None => {
                        message += "Unexpected EOF";
                    }
                };

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
            left_loc: if let Some((left, _, _)) = token {
                left
            } else {
                usize::MAX
            },
            right_loc: if let Some((_, _, right)) = token {
                right
            } else {
                usize::MAX
            },
        },
        ParseError::User { error } => CompilerError::Unlocated {
            message: String::from(error),
        },
    })
}

#[cfg(test)]
mod source_to_ast {
    use super::*;

    fn assert_parse_eq(actual: Result<Ast, CompilerError>, expected: &str) {
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
    fn makes_a_nan_tree() {
        assert_parse_eq(source_to_ast("NaN"), "NaN");
    }

    #[test]
    fn makes_a_inf_tree() {
        assert_parse_eq(source_to_ast("inf"), "inf");
    }

    #[test]
    fn makes_a_bool_tree() {
        assert_parse_eq(source_to_ast("true"), "true");
    }

    #[test]
    fn makes_a_simple_plus_tree() {
        assert_parse_eq(source_to_ast("2 + 5.1"), "(+ 2 5.1)");
    }

    #[test]
    fn makes_a_simple_minus_tree() {
        assert_parse_eq(source_to_ast("2 - 5"), "(- 2 5)");
    }

    #[test]
    fn makes_a_simple_star_tree() {
        assert_parse_eq(source_to_ast("2 * 5"), "(* 2 5)");
    }

    #[test]
    fn makes_a_simple_slash_tree() {
        assert_parse_eq(source_to_ast("2 / 5"), "(/ 2 5)");
    }

    #[test]
    fn makes_an_if_tree() {
        assert_parse_eq(source_to_ast("if true then 2 else 5"), "(if true 2 5)");
    }

    #[test]
    fn makes_a_simple_leq_tree() {
        assert_parse_eq(source_to_ast("2 <= 5"), "(<= 2 5)");
    }

    #[test]
    fn makes_a_nested_tree() {
        assert_parse_eq(source_to_ast("2 + 3 * 5"), "(+ 2 (* 3 5))");
    }

    #[test]
    fn overrides_precedence_with_parens() {
        assert_parse_eq(source_to_ast("(2 + 3) * 5"), "(* (+ 2 3) 5)");
    }

    #[test]
    fn complains_about_empty_input() {
        assert!(source_to_ast("").is_err());
    }

    #[test]
    fn doesnt_like_empty_parens() {
        assert!(source_to_ast("()").is_err());
    }
}
