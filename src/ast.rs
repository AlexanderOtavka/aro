use std::fmt;
use std::f64;

#[derive(Debug, Clone)]
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
    Let(String, TypeAst, Ast, Ast),
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
    Func(String, TypeAst, TypeAst, Ast),
    Tuple(Vec<Ast>),
}

#[derive(Debug, Clone)]
pub struct TypeAst {
    pub left_loc: usize,
    pub right_loc: usize,
    pub expr: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    Func(TypeAst, TypeAst),
    Tuple(Vec<TypeAst>),
}

impl Ast {
    pub fn new(left_loc: usize, right_loc: usize, expr: Expression) -> Ast {
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

impl PartialEq for Ast {
    fn eq(&self, other: &Ast) -> bool {
        self.expr == other.expr
    }
}

impl TypeAst {
    pub fn new(left_loc: usize, right_loc: usize, expr: Type) -> TypeAst {
        TypeAst {
            left_loc,
            right_loc,
            expr: Box::new(expr),
        }
    }
}

impl fmt::Display for TypeAst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
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

impl PartialEq for TypeAst {
    fn eq(&self, other: &TypeAst) -> bool {
        self.expr == other.expr
    }
}
