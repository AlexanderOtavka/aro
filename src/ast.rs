use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub left_loc: usize,
    pub right_loc: usize,
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Value(Value),
    BinOp(BinOp, Ast, Ast),
    If(Ast, Ast, Ast),
    Ident(String),
    Func(String, Ast),
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    LEq,
    Call,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f64),
    Bool(bool),
}

impl Ast {
    pub fn new(left_loc: usize, right_loc: usize, expr: Expression) -> Ast {
        Ast {
            left_loc,
            right_loc,
            expr: Box::new(expr),
        }
    }
}

#[cfg(test)]
impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self.expr {
            &Expression::Value(ref value) => value.fmt(f),
            &Expression::BinOp(ref op, ref a, ref b) => write!(
                f,
                "({} {} {})",
                a,
                match op {
                    &BinOp::Add => "+",
                    &BinOp::Sub => "-",
                    &BinOp::Mul => "*",
                    &BinOp::Div => "/",
                    &BinOp::LEq => "<=",
                    &BinOp::Call => "<|",
                },
                b,
            ),
            &Expression::If(ref c, ref t, ref e) => write!(f, "(if {} then {} else {})", c, t, e),
            &Expression::Func(ref p, ref e) => write!(f, "({} -> {})", p, e),
            &Expression::Ident(ref n) => write!(f, "({})", n),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Value::Int(value) => format!("{}", value),
                &Value::Float(value) => format!("{}", value),
                &Value::Bool(true) => String::from("#true ()"),
                &Value::Bool(false) => String::from("#false ()"),
            }
        )
    }
}
