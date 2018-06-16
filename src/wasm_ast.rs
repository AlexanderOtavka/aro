use c_ast::CName;
use std::fmt::{self, Display, Formatter};
use untyped_ast::Ast;

#[derive(Debug, PartialEq, Clone)]
pub enum WASMType {
    I32,
    I64,
    F64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum WASMValue {
    I32(i64), // Extra space for unsigned integers
    F64(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum WASMExpr {
    Const(WASMType, WASMValue),
    SetLocal(CName, Ast<WASMExpr>),
    GetLocal(CName),
}

#[derive(Debug, PartialEq, Clone)]
pub struct WASMLocal(pub CName, pub WASMType);

#[derive(Debug, PartialEq, Clone)]
pub struct WASMFunc {
    name: CName,
    param: Ast<WASMType>,
    locals: Vec<WASMLocal>,
    body: Vec<Ast<WASMExpr>>,
    ret: Ast<WASMExpr>,
}

impl Display for WASMLocal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "(local ${} {})", self.0, self.1)
    }
}

impl Display for WASMValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            WASMValue::I32(value) => write!(f, "{}", value),
            WASMValue::F64(value) => write!(f, "{}", value),
        }
    }
}

impl Display for WASMType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                WASMType::I32 => "i32",
                WASMType::I64 => "i64",
                WASMType::F64 => "f64",
            }
        )
    }
}

impl Display for WASMExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            WASMExpr::Const(ref value_type, ref value) => {
                write!(f, "({}.const {})", value_type, value)
            }
            WASMExpr::GetLocal(ref name) => write!(f, "(get_local ${})", name),
            WASMExpr::SetLocal(ref name, ref value) => write!(f, "(set_local ${} {})", name, value),
        }
    }
}
