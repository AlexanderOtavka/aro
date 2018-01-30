extern crate regex;

use self::regex::Regex;

#[derive(Debug, PartialEq)]
enum Token {
    LParen,
    RParen,
    Plus,
    Int(i32),
}

fn get_token_list(source: &str) -> Box<Vec<Token>> {
    let l_paren_regex = Regex::new(r"^\s*\(").unwrap();
    let r_paren_regex = Regex::new(r"^\s*\)").unwrap();
    let plus_regex = Regex::new(r"^\s*\+").unwrap();
    let int_regex = Regex::new(r"^\s*(\d+)").unwrap();

    let mut token_list = Vec::new();
    let mut unprocessed_source = source;

    loop {
        if let Some(substr) = l_paren_regex.find(unprocessed_source) {
            token_list.push(Token::LParen);
            unprocessed_source = &unprocessed_source[substr.end()..];
        } else if let Some(substr) = r_paren_regex.find(unprocessed_source) {
            token_list.push(Token::RParen);
            unprocessed_source = &unprocessed_source[substr.end()..];
        } else if let Some(substr) = plus_regex.find(unprocessed_source) {
            token_list.push(Token::Plus);
            unprocessed_source = &unprocessed_source[substr.end()..];
        } else if let Some(captures) = int_regex.captures(unprocessed_source) {
            if let Some(value_match) = captures.get(1) {
                let value = value_match.as_str().parse::<i32>().unwrap();
                token_list.push(Token::Int(value));
            }

            let end = captures.get(0).unwrap().end();
            unprocessed_source = &unprocessed_source[end..];
        } else {
            break;
        }
    }

    Box::new(token_list)
}

#[cfg(test)]
mod test_get_token_list {
    use super::*;

    #[test]
    fn it_returns_an_empty_list_when_given_empty_source() {
        assert_eq!(*get_token_list(""), vec![]);
    }

    #[test]
    fn it_returns_an_empty_list_when_given_only_whitespace() {
        assert_eq!(*get_token_list("     \n \t   "), vec![]);
    }

    #[test]
    fn it_lexes_a_left_paren() {
        assert_eq!(*get_token_list("    \n   \t (   "), vec![Token::LParen]);
    }

    #[test]
    fn it_lexes_two_left_parens() {
        assert_eq!(
            *get_token_list("   ( \n   \t (   "),
            vec![Token::LParen, Token::LParen]
        );
    }

    #[test]
    fn it_lexes_a_right_paren() {
        assert_eq!(*get_token_list(" \n\n \t\t\n )  "), vec![Token::RParen]);
    }

    #[test]
    fn it_lexes_a_plus_sign() {
        assert_eq!(*get_token_list("      +  "), vec![Token::Plus]);
    }

    #[test]
    fn it_lexes_an_int() {
        assert_eq!(*get_token_list("    27  "), vec![Token::Int(27)]);
    }

    #[test]
    fn it_lexes_a_simple_expression() {
        assert_eq!(
            *get_token_list(
                "( + 21
                    (+24 31)
                 )
                "
            ),
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
}
