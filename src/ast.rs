use std::fmt;
use std::fmt::{Display, Formatter};
use std::f64;
use std::iter::Iterator;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

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
    Let(Ast<Pattern>, Ast<Expression>, Ast<Expression>),
    GenericCall(Ast<Expression>, Ast<Type>),
    TypeLet(String, Ast<Type>, Ast<Expression>),
    Sequence(Ast<Expression>, Ast<Expression>),
    RecordAccess(Ast<Expression>, String),
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
    Num(f64),
    Bool(bool),
    Func(Ast<Pattern>, Ast<Type>, Ast<Expression>),
    GenericFunc(String, Ast<Type>, Ast<Type>, Ast<Expression>),
    Tuple(Vec<Ast<Expression>>),
    List(Vec<Ast<Expression>>),
    Hook(Vec<String>, Ast<Type>),
    Ref(Rc<RefCell<Value>>),
    Record(HashMap<String, Ast<Expression>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Ident(String, Ast<Type>),
    Tuple(Vec<Ast<Pattern>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Num,
    Bool,
    Any,
    Empty,
    Ident(String),
    Func(Ast<Type>, Ast<Type>),
    GenericFunc(String, Ast<Type>, Ast<Type>),
    Tuple(Vec<Ast<Type>>),
    List(Ast<Type>),
    Ref(Ast<Type>),
    Record(HashMap<String, Ast<Type>>),
}

impl<T> Ast<T> {
    pub fn new(left_loc: usize, right_loc: usize, expr: T) -> Ast<T> {
        Ast {
            left_loc,
            right_loc,
            expr: Box::new(expr),
        }
    }

    pub fn replace_expr(&self, expr: T) -> Ast<T> {
        Ast::new(self.left_loc, self.right_loc, expr)
    }
}

impl Ast<Expression> {
    pub fn new_hook(
        left_loc: usize,
        right_loc: usize,
        name: &str,
        hook_type: Ast<Type>,
    ) -> Ast<Expression> {
        let path = name[1..name.len() - 1] // Wipe away the surrounding quotes
            .split(".")
            .map(String::from)
            .collect::<Vec<_>>();

        Ast::<Expression>::new(
            left_loc,
            right_loc,
            Expression::Value(Value::Hook(path, hook_type)),
        )
    }

    pub fn is_term(&self) -> bool {
        match &*self.expr {
            &Expression::Value(Value::Tuple(ref vec)) => vec.into_iter().all(|ast| ast.is_term()),
            &Expression::Value(Value::List(ref vec)) => vec.into_iter().all(|ast| ast.is_term()),
            &Expression::Value(Value::Record(ref map)) => {
                map.into_iter().all(|(_, ast)| ast.is_term())
            }
            &Expression::Value(_) => true,
            _ => false,
        }
    }
}

impl Ast<Pattern> {
    pub fn contains_name(&self, name: &str) -> bool {
        match &*self.expr {
            &Pattern::Ident(ref ident_name, _) => name == ident_name,
            &Pattern::Tuple(ref vec) => vec.into_iter().any(|el| el.contains_name(name)),
        }
    }
}

impl<T: PartialEq> PartialEq for Ast<T> {
    fn eq(&self, other: &Ast<T>) -> bool {
        self.expr == other.expr
    }
}

fn sequence_to_str<T: Display>(start: &str, sequence: &Vec<T>, end: &str) -> String {
    let mut string = String::new();

    if sequence.len() >= 1 {
        string += &format!("{}", sequence[0]);

        for element in &sequence[1..] {
            string += &format!(" {}", element);
        }
    }

    format!("{}{}{}", start, string, end)
}

impl<T: Display> Display for Ast<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
            &Expression::GenericCall(ref e, ref t) => write!(f, "({} <| type {})", e, t),
            &Expression::If(ref c, ref t, ref e) => write!(f, "(if {} then {} else {})", c, t, e),
            &Expression::Ident(ref n) => write!(f, "({})", n),
            &Expression::Let(ref p, ref v, ref e) => write!(f, "(let {} <- {} {})", p, v, e),
            &Expression::TypeLet(ref n, ref v, ref e) => write!(f, "(let {} <- {} {})", n, v, e),
            &Expression::Sequence(ref s, ref r) => write!(f, "({}; {})", s, r),
            &Expression::RecordAccess(ref r, ref n) => write!(f, "({}.{})", r, n),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Value::Int(value) => format!("{}", value),
                &Value::Num(value) => {
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
                &Value::Func(ref p, ref te, ref e) => format!("(fn {} ={}=> {})", p, te, e),
                &Value::GenericFunc(ref n, ref t, ref te, ref e) => {
                    format!("({}: {} ={}=> {})", n, t, te, e)
                }
                &Value::Tuple(ref vec) => sequence_to_str("(", vec, ")"),
                &Value::List(ref vec) => sequence_to_str("[", vec, "]"),
                &Value::Hook(ref name, ref hook_type) => {
                    format!("@hook (\"{}\" {})", name.join("."), hook_type)
                }
                &Value::Ref(ref rc) => format!("(ref <| {})", rc.borrow()),
                &Value::Record(ref map) => sequence_to_str(
                    "{",
                    &{
                        let mut vec = map.iter()
                            .map(|(name, value)| format!("{} <- {}", name, value))
                            .collect::<Vec<String>>();
                        vec.sort();
                        vec
                    },
                    "}"
                ),
            }
        )
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Pattern::Ident(ref name, ref type_annotation) => {
                    format!("{}: {}", name, type_annotation)
                }
                &Pattern::Tuple(ref vec) => sequence_to_str("(", vec, ")"),
            }
        )
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &Type::Int => String::from("Int"),
                &Type::Num => String::from("Num"),
                &Type::Bool => String::from("Bool"),
                &Type::Any => String::from("Any"),
                &Type::Empty => String::from("Empty"),
                &Type::Ident(ref name) => format!("({})", name),
                &Type::Func(ref input, ref output) => format!("({} => {})", input, output),
                &Type::GenericFunc(ref name, ref supertype, ref output) => {
                    format!("({}: {} => {})", name, supertype, output)
                }
                &Type::Tuple(ref vec) => sequence_to_str("(", vec, ")"),
                &Type::List(ref element_type) => format!("[{}..]", element_type),
                &Type::Ref(ref value_type) => format!("(Ref <| {})", value_type),
                &Type::Record(ref map) => sequence_to_str(
                    "{",
                    &{
                        let mut vec = map.iter()
                            .map(|(name, value_type)| format!("{}: {}", name, value_type))
                            .collect::<Vec<String>>();
                        vec.sort();
                        vec
                    },
                    "}"
                ),
            }
        )
    }
}
