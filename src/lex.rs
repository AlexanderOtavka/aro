extern crate regex;

use self::regex::Regex;
use std::f64::NAN;

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Int(i32),
    Float(f64),
    Bool(bool),
    Plus,
    Minus,
    Star,
    Slash,
    If,
    LEq,
}

pub fn source_to_tokens(source: &str) -> Result<Box<Vec<Token>>, String> {
    let l_paren_regex = Regex::new(r"^\s*\(").unwrap();
    let r_paren_regex = Regex::new(r"^\s*\)").unwrap();
    let float_regex = Regex::new(r"^\s*(\-?\d+\.\d+)").unwrap();
    let nan_regex = Regex::new(r"^\s*NaN").unwrap();
    let int_regex = Regex::new(r"^\s*(\-?\d+)").unwrap();
    let true_regex = Regex::new(r"^\s*true").unwrap();
    let false_regex = Regex::new(r"^\s*false").unwrap();
    let plus_regex = Regex::new(r"^\s*\+").unwrap();
    let minus_regex = Regex::new(r"^\s*\-").unwrap();
    let multiply_regex = Regex::new(r"^\s*\*").unwrap();
    let divide_regex = Regex::new(r"^\s*/").unwrap();
    let if_regex = Regex::new(r"^\s*if").unwrap();
    let leq_regex = Regex::new(r"^\s*<=").unwrap();
    let end_regex = Regex::new(r"^\s*$").unwrap();

    let mut token_list = Vec::new();
    let mut unprocessed_source = source;

    loop {
        let end;

        if let Some(substr) = l_paren_regex.find(unprocessed_source) {
            token_list.push(Token::LParen);
            end = substr.end();
        } else if let Some(substr) = r_paren_regex.find(unprocessed_source) {
            token_list.push(Token::RParen);
            end = substr.end();
        } else if let Some(captures) = float_regex.captures(unprocessed_source) {
            let value_match = captures.get(1).unwrap();
            let value = value_match.as_str().parse::<f64>().unwrap();
            token_list.push(Token::Float(value));
            end = captures.get(0).unwrap().end();
        } else if let Some(substr) = nan_regex.find(unprocessed_source) {
            token_list.push(Token::Float(NAN));
            end = substr.end();
        } else if let Some(captures) = int_regex.captures(unprocessed_source) {
            let value_match = captures.get(1).unwrap();
            let value = value_match.as_str().parse::<i32>().unwrap();
            token_list.push(Token::Int(value));
            end = captures.get(0).unwrap().end();
        } else if let Some(substr) = true_regex.find(unprocessed_source) {
            token_list.push(Token::Bool(true));
            end = substr.end();
        } else if let Some(substr) = false_regex.find(unprocessed_source) {
            token_list.push(Token::Bool(false));
            end = substr.end();
        } else if let Some(substr) = plus_regex.find(unprocessed_source) {
            token_list.push(Token::Plus);
            end = substr.end();
        } else if let Some(substr) = minus_regex.find(unprocessed_source) {
            token_list.push(Token::Minus);
            end = substr.end();
        } else if let Some(substr) = multiply_regex.find(unprocessed_source) {
            token_list.push(Token::Star);
            end = substr.end();
        } else if let Some(substr) = divide_regex.find(unprocessed_source) {
            token_list.push(Token::Slash);
            end = substr.end();
        } else if let Some(substr) = if_regex.find(unprocessed_source) {
            token_list.push(Token::If);
            end = substr.end();
        } else if let Some(substr) = leq_regex.find(unprocessed_source) {
            token_list.push(Token::LEq);
            end = substr.end();
        } else if end_regex.is_match(unprocessed_source) {
            break;
        } else {
            return Err(String::from("Suprise unknown token, muthafaka."));
        }

        unprocessed_source = &unprocessed_source[end..];
    }

    Ok(Box::new(token_list))
}

#[cfg(test)]
mod test_source_to_tokens {
    use super::*;

    #[test]
    fn it_returns_an_empty_list_when_given_empty_source() {
        assert_eq!(*source_to_tokens("").unwrap(), vec![]);
    }

    #[test]
    fn it_returns_an_empty_list_when_given_only_whitespace() {
        assert_eq!(*source_to_tokens("     \n \t   ").unwrap(), vec![]);
    }

    #[test]
    fn it_lexes_a_left_paren() {
        assert_eq!(
            *source_to_tokens("    \n   \t (   ").unwrap(),
            vec![Token::LParen]
        );
    }

    #[test]
    fn it_lexes_two_left_parens() {
        assert_eq!(
            *source_to_tokens("   ( \n   \t (   ").unwrap(),
            vec![Token::LParen, Token::LParen]
        );
    }

    #[test]
    fn it_lexes_a_right_paren() {
        assert_eq!(
            *source_to_tokens(" \n\n \t\t\n )  ").unwrap(),
            vec![Token::RParen]
        );
    }

    #[test]
    fn it_lexes_an_int() {
        assert_eq!(*source_to_tokens("    27  ").unwrap(), vec![Token::Int(27)]);
    }

    #[test]
    fn it_lexes_a_negative_int() {
        assert_eq!(
            *source_to_tokens("    -27  ").unwrap(),
            vec![Token::Int(-27)]
        );
    }

    #[test]
    fn it_lexes_a_minus_and_then_an_int() {
        assert_eq!(
            *source_to_tokens("    - 27  ").unwrap(),
            vec![Token::Minus, Token::Int(27)]
        );
    }

    #[test]
    fn it_lexes_a_float() {
        assert_eq!(
            *source_to_tokens("    27.2  ").unwrap(),
            vec![Token::Float(27.2)]
        );
    }

    #[test]
    fn it_lexes_a_negative_float() {
        assert_eq!(
            *source_to_tokens("    -27.2  ").unwrap(),
            vec![Token::Float(-27.2)]
        );
    }

    #[test]
    fn it_lexes_a_minus_and_then_a_float() {
        assert_eq!(
            *source_to_tokens("    - 27.2  ").unwrap(),
            vec![Token::Minus, Token::Float(27.2)]
        );
    }

    #[test]
    fn it_lexes_nan() {
        let tokens = source_to_tokens("    NaN  ").unwrap();
        assert_eq!(tokens.len(), 1);
        if let Token::Float(first_token) = tokens[0] {
            assert!(first_token.is_nan());
        } else {
            assert!(false, "Not a float token")
        }
    }

    #[test]
    fn it_lexes_a_true_bool() {
        assert_eq!(
            *source_to_tokens("    true  ").unwrap(),
            vec![Token::Bool(true)]
        );
    }

    #[test]
    fn it_lexes_a_false_bool() {
        assert_eq!(
            *source_to_tokens("    false  ").unwrap(),
            vec![Token::Bool(false)]
        );
    }

    #[test]
    fn it_lexes_a_plus_sign() {
        assert_eq!(*source_to_tokens("      +  ").unwrap(), vec![Token::Plus]);
    }

    #[test]
    fn it_lexes_a_minus_sign() {
        assert_eq!(*source_to_tokens("      -  ").unwrap(), vec![Token::Minus]);
    }

    #[test]
    fn it_lexes_a_multiply_sign() {
        assert_eq!(*source_to_tokens("      *  ").unwrap(), vec![Token::Star]);
    }

    #[test]
    fn it_lexes_a_divide_sign() {
        assert_eq!(*source_to_tokens("      /  ").unwrap(), vec![Token::Slash]);
    }

    #[test]
    fn it_lexes_an_if() {
        assert_eq!(*source_to_tokens("      if  ").unwrap(), vec![Token::If]);
    }

    #[test]
    fn it_lexes_a_simple_expression() {
        assert_eq!(
            *source_to_tokens(
                "( + 21
                    (+24 31)
                 )
                "
            ).unwrap(),
            vec![
                Token::LParen,
                Token::Plus,
                Token::Int(21),
                Token::LParen,
                Token::Plus,
                Token::Int(24),
                Token::Int(31),
                Token::RParen,
                Token::RParen,
            ]
        );
    }

    #[test]
    fn it_doesnt_lex_unknown_tokens() {
        assert_eq!(
            source_to_tokens("&").unwrap_err(),
            "Suprise unknown token, muthafaka."
        );
        assert_eq!(
            source_to_tokens(" \n \t  & \n\t  ").unwrap_err(),
            "Suprise unknown token, muthafaka."
        );
    }
}
