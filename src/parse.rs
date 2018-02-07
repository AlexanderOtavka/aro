use super::lex::Token;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(i32),
    Bool(bool),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    LEq(Box<Expression>, Box<Expression>),
}

fn binary_operator_to_ast<F>(
    tokens: &[Token],
    get_expression: F,
) -> Result<(Box<Expression>, &[Token]), &str>
where
    F: Fn(Box<Expression>, Box<Expression>) -> Expression,
{
    let (left_expr, unprocessed_tokens) = tokens_to_ast(tokens)?;
    let (right_expr, unprocessed_tokens) = tokens_to_ast(unprocessed_tokens)?;

    match unprocessed_tokens.get(0) {
        Some(&Token::RParen) => Ok((
            Box::new(get_expression(left_expr, right_expr)),
            &unprocessed_tokens[1..],
        )),
        _ => Err("Nah-ah.  Close that shit with a `)`."),
    }
}

fn call_to_ast(tokens: &[Token]) -> Result<(Box<Expression>, &[Token]), &str> {
    if let Some(token) = tokens.get(0) {
        return match *token {
            Token::Plus => {
                binary_operator_to_ast(&tokens[1..], |left, right| Expression::Add(left, right))
            }
            Token::Minus => binary_operator_to_ast(&tokens[1..], |left, right| {
                Expression::Subtract(left, right)
            }),
            Token::Star => binary_operator_to_ast(&tokens[1..], |left, right| {
                Expression::Multiply(left, right)
            }),
            Token::Slash => {
                binary_operator_to_ast(&tokens[1..], |left, right| Expression::Divide(left, right))
            }
            Token::If => {
                let (guard_expr, unprocessed_tokens) = tokens_to_ast(&tokens[1..])?;
                let (consequent_expr, unprocessed_tokens) = tokens_to_ast(unprocessed_tokens)?;
                let (alternate_expr, unprocessed_tokens) = tokens_to_ast(unprocessed_tokens)?;

                match unprocessed_tokens.get(0) {
                    Some(&Token::RParen) => Ok((
                        Box::new(Expression::If(guard_expr, consequent_expr, alternate_expr)),
                        &unprocessed_tokens[1..],
                    )),
                    _ => Err("Nah-ah.  Close that shit with a `)`."),
                }
            }
            Token::LEq => {
                binary_operator_to_ast(&tokens[1..], |left, right| Expression::LEq(left, right))
            }
            _ => Err("God damn it.  OPERATOR GOES HERE.  It's like I'm talking to a monkey."),
        };
    } else {
        return Err("You ass goblin!  You can't end the file there.");
    }
}

pub fn tokens_to_ast(tokens: &[Token]) -> Result<(Box<Expression>, &[Token]), &str> {
    if let Some(token) = tokens.get(0) {
        return match *token {
            Token::Int(value) => Ok((Box::new(Expression::Int(value)), &tokens[1..])),
            Token::Bool(value) => Ok((Box::new(Expression::Bool(value)), &tokens[1..])),
            Token::LParen => call_to_ast(&tokens[1..]),
            _ => Err("Hey asshole, expected value or `(`."),
        };
    } else {
        return Err("You ass goblin!  You can't end the file there.");
    }
}

#[cfg(test)]
mod test_tokens_to_ast {
    use super::*;

    #[test]
    fn it_makes_an_int_tree() {
        let tokens = vec![Token::Int(5)];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(*expr, Expression::Int(5));
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_a_bool_tree() {
        let tokens = vec![Token::Bool(true)];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(*expr, Expression::Bool(true));
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
    fn it_makes_a_simple_minus_tree() {
        let tokens = vec![
            Token::LParen,
            Token::Minus,
            Token::Int(2),
            Token::Int(5),
            Token::RParen,
        ];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(
            *expr,
            Expression::Subtract(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_a_simple_star_tree() {
        let tokens = vec![
            Token::LParen,
            Token::Star,
            Token::Int(2),
            Token::Int(5),
            Token::RParen,
        ];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(
            *expr,
            Expression::Multiply(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_a_simple_slash_tree() {
        let tokens = vec![
            Token::LParen,
            Token::Slash,
            Token::Int(2),
            Token::Int(5),
            Token::RParen,
        ];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(
            *expr,
            Expression::Divide(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_an_if_tree() {
        let tokens = vec![
            Token::LParen,
            Token::If,
            Token::Bool(true),
            Token::Int(2),
            Token::Int(5),
            Token::RParen,
        ];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(
            *expr,
            Expression::If(
                Box::new(Expression::Bool(true)),
                Box::new(Expression::Int(2)),
                Box::new(Expression::Int(5))
            )
        );
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_a_simple_leq_tree() {
        let tokens = vec![
            Token::LParen,
            Token::LEq,
            Token::Int(2),
            Token::Int(5),
            Token::RParen,
        ];
        let (expr, unprocessed) = tokens_to_ast(&tokens).unwrap();
        assert_eq!(
            *expr,
            Expression::LEq(Box::new(Expression::Int(2)), Box::new(Expression::Int(5)))
        );
        assert_eq!(unprocessed.len(), 0);
    }

    #[test]
    fn it_makes_a_nested_tree() {
        let tokens = vec![
            Token::LParen,
            Token::Plus,
            Token::Int(2),
            Token::LParen,
            Token::Star,
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
                Box::new(Expression::Multiply(
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
        assert_eq!(message, "You ass goblin!  You can't end the file there.");
    }

    #[test]
    fn it_handles_an_unclosed_paren() {
        let tokens = vec![Token::LParen];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "You ass goblin!  You can't end the file there.");
    }

    #[test]
    fn it_handles_a_value_after_paren() {
        let tokens = vec![Token::LParen, Token::Int(3), Token::RParen];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(
            message,
            "God damn it.  OPERATOR GOES HERE.  It's like I'm talking to a monkey."
        );
    }

    #[test]
    fn it_handles_an_early_r_paren() {
        let tokens = vec![Token::RParen];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Hey asshole, expected value or `(`.");
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
        assert_eq!(message, "Nah-ah.  Close that shit with a `)`.");
    }

    #[test]
    fn it_handles_an_unclosed_plus_operation() {
        let tokens = vec![Token::LParen, Token::Plus, Token::Int(2), Token::Int(5)];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Nah-ah.  Close that shit with a `)`.");
    }

    #[test]
    fn it_handles_a_too_short_plus_operation() {
        let tokens = vec![Token::LParen, Token::Plus, Token::Int(2), Token::RParen];
        let message = tokens_to_ast(&tokens).unwrap_err();
        assert_eq!(message, "Hey asshole, expected value or `(`.");
    }
}
