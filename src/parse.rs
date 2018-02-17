use ast::Ast;
use grammar::parse_Expr;

pub fn source_to_ast(source: &str) -> Result<Ast, String> {
    parse_Expr(source).map_err(|err| format!("{}", err))
}

#[cfg(test)]
mod source_to_ast {
    use super::*;

    fn assert_parse_eq(actual: Result<Ast, String>, expected: &str) {
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
