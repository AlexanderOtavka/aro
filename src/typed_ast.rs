use std::collections::HashMap;
use std::f64;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use untyped_ast::{Ast, BinOp};

#[derive(Debug, PartialEq, Clone)]
pub struct TypedAst<Expr> {
    pub left_loc: usize,
    pub right_loc: usize,
    pub expr: Box<Expr>,
    pub expr_type: Box<EvaluatedType>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpression {
    Value(TypedValue),
    BinOp(BinOp, TypedAst<TypedExpression>, TypedAst<TypedExpression>),
    Call(TypedAst<TypedExpression>, TypedAst<TypedExpression>),
    If(
        TypedAst<TypedExpression>,
        TypedAst<TypedExpression>,
        TypedAst<TypedExpression>,
    ),
    Ident(String),
    Let(
        TypedAst<TypedPattern>,
        TypedAst<TypedExpression>,
        TypedAst<TypedExpression>,
    ),
    Cast(TypedAst<TypedExpression>, Ast<EvaluatedType>),
    Sequence(TypedAst<TypedExpression>, TypedAst<TypedExpression>),
    RecordAccess(TypedAst<TypedExpression>, String),
    RefNew(TypedAst<TypedExpression>),
    RefGet(TypedAst<TypedExpression>),
    RefSet(TypedAst<TypedExpression>, TypedAst<TypedExpression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedValue {
    Int(i32),
    Num(f64),
    Bool(bool),
    Func(
        TypedAst<TypedPattern>,
        Ast<EvaluatedType>,
        TypedAst<TypedExpression>,
    ),
    Tuple(Vec<TypedAst<TypedExpression>>),
    List(Vec<TypedAst<TypedExpression>>),
    Hook(Vec<String>),
    Record(HashMap<String, TypedAst<TypedExpression>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedPattern {
    Ident(String),
    Tuple(Vec<TypedAst<TypedPattern>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum EvaluatedType {
    Int,
    Num,
    Bool,
    Any,
    None,
    Ident(String, Ast<EvaluatedType>),
    Func(Ast<EvaluatedType>, Ast<EvaluatedType>),
    GenericFunc {
        param_name: String,
        param_supertype: Ast<EvaluatedType>,
        output: Ast<EvaluatedType>,
        substituted_output: Ast<EvaluatedType>,
    },
    Tuple(Vec<Ast<EvaluatedType>>),
    List(Ast<EvaluatedType>),
    Ref(Ast<EvaluatedType>),
    Record(HashMap<String, Ast<EvaluatedType>>),
}

impl<T> Ast<T> {
    pub fn to_typed<NewExpr>(&self, expr: NewExpr, expr_type: EvaluatedType) -> TypedAst<NewExpr> {
        TypedAst {
            left_loc: self.left_loc,
            right_loc: self.right_loc,
            expr: Box::new(expr),
            expr_type: Box::new(expr_type),
        }
    }
}

impl<Expr> TypedAst<Expr> {
    pub fn replace_untyped<NewExpr>(&self, expr: NewExpr) -> Ast<NewExpr> {
        Ast {
            left_loc: self.left_loc,
            right_loc: self.right_loc,
            expr: Box::new(expr),
        }
    }

    pub fn to_type_ast(&self) -> Ast<EvaluatedType> {
        Ast::new(self.left_loc, self.right_loc, *self.expr_type.clone())
    }
}

impl<Expr: Clone> TypedAst<Expr> {
    pub fn replace_type(&self, expr_type: EvaluatedType) -> TypedAst<Expr> {
        TypedAst {
            left_loc: self.left_loc,
            right_loc: self.right_loc,
            expr: self.expr.clone(),
            expr_type: Box::new(expr_type),
        }
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

impl Display for EvaluatedType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &EvaluatedType::Int => String::from("Int"),
                &EvaluatedType::Num => String::from("Num"),
                &EvaluatedType::Bool => String::from("Bool"),
                &EvaluatedType::Any => String::from("Any"),
                &EvaluatedType::None => String::from("None"),
                &EvaluatedType::Ident(ref name, ref supertype) => {
                    format!("({} <: {})", name, supertype)
                }
                &EvaluatedType::Func(ref input, ref output) => format!("({} => {})", input, output),
                &EvaluatedType::GenericFunc {
                    ref param_name,
                    ref param_supertype,
                    ref output,
                    ..
                } => format!("({}: {} => {})", param_name, param_supertype, output),
                &EvaluatedType::Tuple(ref vec) => sequence_to_str("(", vec, ")"),
                &EvaluatedType::List(ref element_type) => format!("[{}]", element_type),
                &EvaluatedType::Ref(ref value_type) => format!("(&{})", value_type),
                &EvaluatedType::Record(ref map) => sequence_to_str(
                    "{",
                    &{
                        let mut vec = map.iter()
                            .map(|(name, value_type)| format!("{}: {}", name, value_type))
                            .collect::<Vec<String>>();
                        vec.sort();
                        vec
                    },
                    "}",
                ),
            }
        )
    }
}
