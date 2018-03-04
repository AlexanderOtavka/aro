use std::fmt;
use std::fmt::{Display, Formatter};
use std::f64;
use std::iter::Iterator;

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
    GenericFunc(String, Ast<Expression>),
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
    Func(Ast<Pattern>, Ast<Type>, Ast<Expression>),
    Tuple(Vec<Ast<Expression>>),
    List(Vec<Ast<Expression>>),
    Hook(Vec<String>, Ast<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Ident(String, Ast<Type>),
    Tuple(Vec<Ast<Pattern>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    Empty,
    Generic(String),
    Ident(String),
    Func(Ast<Type>, Ast<Type>),
    Tuple(Vec<Ast<Type>>),
    List(Ast<Type>),
}

impl<T> Ast<T> {
    pub fn new(left_loc: usize, right_loc: usize, expr: T) -> Ast<T> {
        Ast {
            left_loc,
            right_loc,
            expr: Box::new(expr),
        }
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
            &Expression::Value(_) => true,
            _ => false,
        }
    }
}

impl Type {
    pub fn is_sub_type(&self, other: &Type) -> bool {
        match (self, other) {
            (&Type::Empty, _)
            | (&Type::Bool, &Type::Bool)
            | (&Type::Int, &Type::Int)
            | (&Type::Float, &Type::Float) => true,
            (&Type::Func(ref self_in, ref self_out), &Type::Func(ref other_in, ref other_out)) => {
                self_out.is_sub_type(other_out) && other_in.is_sub_type(self_in)
            }
            (&Type::Tuple(ref self_vec), &Type::Tuple(ref other_vec)) => {
                self_vec.len() == other_vec.len()
                    && Iterator::zip(self_vec.into_iter(), other_vec.into_iter())
                        .all(|(self_el, other_el)| self_el.is_sub_type(other_el))
            }
            (&Type::List(ref self_el), &Type::List(ref other_el)) => self_el.is_sub_type(other_el),
            _ => false,
        }
    }
}

impl Ast<Type> {
    pub fn is_sub_type(&self, other: &Ast<Type>) -> bool {
        self.expr.is_sub_type(&other.expr)
    }
}

#[cfg(test)]
mod is_sub_type {
    use super::*;

    fn ast(t: Type) -> Ast<Type> {
        Ast::<Type>::new(0, 0, t)
    }

    #[test]
    fn same_type() {
        // let x: [] <== []
        //   same ^^     ^^ same
        // Empty.is_sub_type(Empty) = true  -- so this typechecks (if [] were allowed)
        assert!(ast(Type::Empty).is_sub_type(&ast(Type::Empty)));

        assert!(ast(Type::Int).is_sub_type(&ast(Type::Int)));
        assert!(ast(Type::Bool).is_sub_type(&ast(Type::Bool)));
        assert!(ast(Type::Float).is_sub_type(&ast(Type::Float)));

        assert!(Type::Int.is_sub_type(&Type::Int));
    }

    #[test]
    fn empty() {
        // An empty list IS A list of integers.  Thus, Empty is subtype of Int
        // let x: [Int..] <== []
        //  super ^^^^^^^     ^^ subtype
        // Empty.is_sub_type(Int) = true  -- so this typechecks
        assert!(ast(Type::Empty).is_sub_type(&ast(Type::Int)));

        // let x: [] <== [5]
        //    sub ^^     ^^^ supertype
        // Int.is_sub_type(Empty) = false  -- so this does not typecheck
        assert!(!ast(Type::Int).is_sub_type(&ast(Type::Empty)));
    }

    //
    // The general form is:
    // value.is_sub_type(declared) <=> it typechecks
    //

    // It's wierder with functions:
    #[test]
    fn with_functions() {
        // let x: (Int -> [Int..]) <== x: Int -[]-> []
        //  super ^^^^^^^^^^^^^^^^     ^^^^^^^^^^^^^^^ subtype
        // (Int -> []).is_sub_type(Int -> [Int..]) = true  -- so this typechecks
        // because:
        //   value_out.is_sub_type(delcared_out)
        //   <=> [].is_sub_type([Int..])
        //   <=> Empty.is_sub_type(Int)
        assert!(
            ast(Type::Func(ast(Type::Int), ast(Type::Empty)))
                .is_sub_type(&ast(Type::Func(ast(Type::Int), ast(Type::Int))))
        );

        // Reverse them, and it's false
        assert!(!ast(Type::Func(ast(Type::Int), ast(Type::Int)))
            .is_sub_type(&ast(Type::Func(ast(Type::Int), ast(Type::Empty)))));

        // let x: ([] -> Int) <== x: [Int..] -Int-> 1
        //  super ^^^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^ subtype
        // ([Int..] -> Int).is_sub_type([] -> Int) = true  -- so this typechecks
        // because:
        //   declared_in.is_sub_type(value_in)
        //   <=> [].is_sub_type([Int..])
        //   <=> Empty.is_sub_type(Int)
        assert!(
            ast(Type::Func(ast(Type::Int), ast(Type::Int)))
                .is_sub_type(&ast(Type::Func(ast(Type::Empty), ast(Type::Int))))
        );

        // Again, reverse them, and it's false
        assert!(!ast(Type::Func(ast(Type::Empty), ast(Type::Int)))
            .is_sub_type(&ast(Type::Func(ast(Type::Int), ast(Type::Int)))));
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
            &Expression::If(ref c, ref t, ref e) => write!(f, "(if {} then {} else {})", c, t, e),
            &Expression::Ident(ref n) => write!(f, "({})", n),
            &Expression::Let(ref p, ref v, ref e) => write!(f, "(let {} <== {} {})", p, v, e),
            &Expression::GenericFunc(ref n, ref e) => write!(f, "({} -> {})", n, e),
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
                &Value::Func(ref p, ref te, ref e) => format!("(fn {} -{}-> {})", p, te, e),
                &Value::Tuple(ref vec) => sequence_to_str("(", vec, ")"),
                &Value::List(ref vec) => sequence_to_str("[", vec, "]"),
                &Value::Hook(ref name, ref hook_type) => {
                    format!("@hook({} {})", name.join("."), hook_type)
                }
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
                &Type::Float => String::from("Float"),
                &Type::Bool => String::from("Bool"),
                &Type::Empty => String::from("Empty"),
                &Type::Ident(ref name) | &Type::Generic(ref name) => format!("({})", name),
                &Type::Func(ref input, ref output) => format!("({} -> {})", input, output),
                &Type::Tuple(ref vec) => sequence_to_str("(", vec, ")"),
                &Type::List(ref element_type) => format!("[{}..]", element_type),
            }
        )
    }
}
