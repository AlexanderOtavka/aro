use std::fmt;
use std::f64;

#[derive(Debug, Clone)]
pub struct Ast<T> {
    pub left_loc: usize,
    pub right_loc: usize,
    pub expr: Box<T>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Value(Value),
    BinOp(BinOp, Ast<Expression>, Ast<Expression>),
    If(Ast<Expression>, Ast<Expression>, Ast<Expression>),
    Ident(String),
    Let(String, Ast<Type>, Ast<Expression>, Ast<Expression>),
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
    Func(String, Ast<Type>, Ast<Type>, Ast<Expression>),
    Tuple(Vec<Ast<Expression>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Ident(String, Ast<Type>),
    Tuple(Vec<Pattern>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    Func(Ast<Type>, Ast<Type>),
    Tuple(Vec<Ast<Type>>),
}

impl Ast<Expression> {
    pub fn new(left_loc: usize, right_loc: usize, expr: Expression) -> Ast<Expression> {
        Ast {
            left_loc,
            right_loc,
            expr: Box::new(expr),
        }
    }

    pub fn is_term(&self) -> bool {
        match &*self.expr {
            &Expression::Value(Value::Tuple(ref vec)) => vec.into_iter().all(|ast| ast.is_term()),
            &Expression::Value(_) => true,
            _ => false,
        }
    }
}

impl Ast<Type> {
    pub fn new(left_loc: usize, right_loc: usize, expr: Type) -> Ast<Type> {
        Ast {
            left_loc,
            right_loc,
            expr: Box::new(expr),
        }
    }
}

impl<T: PartialEq> PartialEq for Ast<T> {
    fn eq(&self, other: &Ast<T>) -> bool {
        self.expr == other.expr
    }
}

impl<T: fmt::Display> fmt::Display for Ast<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
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
            // &Expression::Let(ref p, ref v, ref e) => write!(f, "(let {} <== {} {})", p, v, e),
            &Expression::Let(ref n, ref t, ref v, ref e) => {
                write!(f, "(let {}: {} <== {} {})", n, t, v, e)
            }
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
                &Value::Func(ref p, ref tp, ref te, ref e) => {
                    format!("({}: {} -{}-> {})", p, tp, te, e)
                }
                &Value::Tuple(ref vec) => {
                    let mut string = String::new();
                    string += "(";

                    if vec.len() >= 1 {
                        string += &format!("{}", vec[0]);

                        for element in &vec[1..] {
                            string += &format!(" {}", element);
                        }
                    }

                    string += ")";

                    string
                }
            }
        )
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Pattern::Ident(ref name, ref type_annotation) => {
                    format!("{}: {}", name, type_annotation)
                }
                &Pattern::Tuple(ref vec) => {
                    let mut string = String::new();
                    string += "(";

                    if vec.len() >= 1 {
                        string += &format!("{}", vec[0]);

                        for element in &vec[1..] {
                            string += &format!(" {}", element);
                        }
                    }

                    string += ")";

                    string
                }
            }
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Type::Int => String::from("Int"),
                &Type::Float => String::from("Float"),
                &Type::Bool => String::from("Bool"),
                &Type::Func(ref input, ref output) => format!("({} -> {})", input, output),
                &Type::Tuple(ref vec) => {
                    let mut string = String::new();
                    string += "(";

                    if vec.len() >= 1 {
                        string += &format!("{}", vec[0]);

                        for element in &vec[1..] {
                            string += &format!(" {}", element);
                        }
                    }

                    string += ")";

                    string
                }
            }
        )
    }
}
