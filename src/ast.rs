use std::fmt;
use std::f64;

#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    pub left_loc: usize,
    pub right_loc: usize,
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Value(Value),
    BinOp(BinOp, Ast, Ast),
    If(Ast, Ast, Ast),
    Ident(String),
    Let(String, Ast, Ast),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    LEq,
    Call,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i32),
    Float(f64),
    Bool(bool),
    Func(String, Ast),
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
            &Expression::Ident(ref n) => write!(f, "({})", n),
            &Expression::Let(ref n, ref v, ref e) => write!(f, "(let {} <== {} {})", n, v, e),
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
                &Value::Float(value) => {
                    if value.is_nan() {
                        String::from("nan")
                    } else if value == f64::INFINITY {
                        String::from("inf")
                    } else {
                        format!("{}", value)
                    }
                }
                &Value::Bool(true) => String::from("#true ()"),
                &Value::Bool(false) => String::from("#false ()"),
                &Value::Func(ref p, ref e) => format!("({} -> {})", p, e),
            }
        )
    }
}
