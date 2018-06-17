use c_ast::CName;
use std::fmt::{self, Display, Formatter};
use untyped_ast::{Ast, BinOp, NumOp, RelOp};
use util::sequence_to_str;

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
    BinOp(BinOp, Ast<WASMExpr>, Ast<WASMExpr>, WASMType),
    PromoteInt(Ast<WASMExpr>),
    If(Ast<WASMExpr>, Vec<Ast<WASMExpr>>, Vec<Ast<WASMExpr>>),
    GrowMemory,
    Load(WASMType, Ast<WASMExpr>),
    Store(WASMType, Ast<WASMExpr>, Ast<WASMExpr>),
    Call {
        param_type: WASMType,
        ret_type: WASMType,
        function_index: Ast<WASMExpr>,
        arg: Ast<WASMExpr>,
        captures: Ast<WASMExpr>,
    },
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
            WASMExpr::BinOp(ref op, ref left, ref right, ref result_type) => write!(
                f,
                "({}.{} {} {})",
                result_type,
                match op {
                    BinOp::Num(NumOp::Add) => "add",
                    BinOp::Num(NumOp::Sub) => "sub",
                    BinOp::Num(NumOp::Mul) => "mul",
                    BinOp::Num(NumOp::Div) => "div",
                    BinOp::Rel(RelOp::LEq) => "le_s",
                },
                left,
                right
            ),
            WASMExpr::PromoteInt(ref int) => write!(f, "(f64.convert_s/i32 {})", int),
            WASMExpr::If(ref condition, ref consequent, ref alternate) => write!(
                f,
                "(if {} (then {}) (else {}))",
                condition,
                sequence_to_str("", consequent, ""),
                sequence_to_str("", alternate, ""),
            ),
            WASMExpr::GrowMemory => write!(f, "(grow_memory (i32.const 1))"),
            WASMExpr::Load(ref value_type, ref offset) => {
                write!(f, "({}.load {})", value_type, offset)
            }
            WASMExpr::Store(ref value_type, ref offset, ref value) => {
                write!(f, "({}.load {} {})", value_type, offset, value)
            }
            WASMExpr::Call {
                ref param_type,
                ref ret_type,
                ref function_index,
                ref arg,
                ref captures,
            } => write!(
                f,
                "(call_indirect (param {} i32) (result {}) {} {} {})",
                param_type, ret_type, arg, captures, function_index
            ),
        }
    }
}
