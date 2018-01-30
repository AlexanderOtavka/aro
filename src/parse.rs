use super::lex::Token;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(i32),
    Add(Box<Expression>, Box<Expression>),
}

pub fn tokens_to_ast(tokens: &Vec<Token>) -> Box<Expression> {
    Box::new(Expression::Int(0))
}

#[cfg(test)]
mod test_tokens_to_ast {
    use super::*;

    #[test]
    fn it_makes_a_0_tree() {
        assert_eq!(*tokens_to_ast(&vec![Token::Int(0)]), Expression::Int(0));
    }
}
