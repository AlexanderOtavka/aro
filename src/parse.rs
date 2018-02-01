use super::lex::Token;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(i32),
    Add(Box<Expression>, Box<Expression>),
}

fn call_to_ast(tokens: &[Token]) -> Result<(Box<Expression>, &[Token]), &str> {
    if let Some(token) = tokens.get(0) {
        return match *token {
            Token::Plus => {
                let (left_expr, unprocessed_tokens) = tokens_to_ast(&tokens[1..])?;
                let (right_expr, unprocessed_tokens) = tokens_to_ast(unprocessed_tokens)?;

                match unprocessed_tokens.get(0) {
                    Some(&Token::RParen) => Ok((
                        Box::new(Expression::Add(left_expr, right_expr)),
                        &unprocessed_tokens[1..],
                    )),
                    _ => Err("Expected `)`."),
                }
            }
            _ => Err("Expected operator."),
        };
    } else {
        return Err("Unexpected end of file.");
    }
}

pub fn tokens_to_ast(tokens: &[Token]) -> Result<(Box<Expression>, &[Token]), &str> {
    if let Some(token) = tokens.get(0) {
        return match *token {
            Token::Int(value) => Ok((Box::new(Expression::Int(value)), &tokens[1..])),
            Token::LParen => call_to_ast(&tokens[1..]),
            _ => Err("Expected value or `(`."),
        };
    } else {
        return Err("Unexpected end of file.");
    }
}

#[cfg(test)]
mod test_tokens_to_ast {
    use super::*;

    #[test]
    fn it_makes_a_0_tree() {
        let tokens = vec![Token::Int(0)];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(*expr, Expression::Int(0));
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_an_int_tree() {
        let tokens = vec![Token::Int(5)];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(*expr, Expression::Int(5));
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_a_simple_plus_tree() {
        let tokens = vec![
            Token::LParen,
            Token::Plus,
            Token::Int(2),
            Token::Int(5),
            Token::RParen,
        ];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(
            *expr,
            Expression::Add(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_a_nested_plus_tree() {
        let tokens = vec![
            Token::LParen,
            Token::Plus,
            Token::Int(2),
            Token::LParen,
            Token::Plus,
            Token::Int(3),
            Token::Int(5),
            Token::RParen,
            Token::RParen,
        ];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(
            *expr,
            Expression::Add(
                Box::new(Expression::Int(2)),
                Box::new(Expression::Add(
                    Box::new(Expression::Int(3)),
                    Box::new(Expression::Int(5))
                ))
            )
        );
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_handles_an_empty_token_list() {
        let tokens = vec![];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Unexpected end of file.");
    }

    #[test]
    fn it_handles_an_unclosed_paren() {
        let tokens = vec![Token::LParen];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Unexpected end of file.");
    }

    #[test]
    fn it_handles_a_value_after_paren() {
        let tokens = vec![Token::LParen, Token::Int(3), Token::RParen];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Expected operator.");
    }

    #[test]
    fn it_handles_an_early_r_paren() {
        let tokens = vec![Token::RParen];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Expected value or `(`.");
    }

    #[test]
    fn it_handles_a_too_long_plus_operation() {
        let tokens = vec![
            Token::LParen,
            Token::Plus,
            Token::Int(2),
            Token::Int(5),
            Token::Int(7),
            Token::RParen,
        ];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Expected `)`.");
    }

    #[test]
    fn it_handles_an_unclosed_plus_operation() {
        let tokens = vec![Token::LParen, Token::Plus, Token::Int(2), Token::Int(5)];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Expected `)`.");
    }

    #[test]
    fn it_handles_a_too_short_plus_operation() {
        let tokens = vec![Token::LParen, Token::Plus, Token::Int(2), Token::RParen];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Expected value or `(`.");
    }
}
