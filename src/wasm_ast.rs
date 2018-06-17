use c_ast::CName;
use std::fmt::{self, Display, Formatter};
use untyped_ast::{Ast, BinOp, NumOp, RelOp};

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

fn get_indent(indent_level: u32) -> String {
    let mut indent = String::new();

    for _ in 0..indent_level {
        indent += "  ";
    }

    indent
}

fn sequence_to_str_indented(sequence: &Vec<Ast<WASMExpr>>, indent_level: u32) -> String {
    let mut string = String::new();

    let mut is_first = false;
    for element in sequence {
        if is_first {
            is_first = false;
        } else {
            string += "\n";
        }

        string += &element.get_str_indented(indent_level);
    }

    string
}

impl Ast<WASMExpr> {
    pub fn get_str_indented(&self, indent_level: u32) -> String {
        self.expr.get_str_indented(indent_level)
    }
}

impl WASMExpr {
    pub fn get_str_indented(&self, indent_level: u32) -> String {
        format!(
            "{}{}",
            get_indent(indent_level),
            match self {
                WASMExpr::Const(ref value_type, ref value) => {
                    format!("({}.const {})", value_type, value)
                }
                WASMExpr::GetLocal(ref name) => format!("(get_local ${})", name),
                WASMExpr::SetLocal(ref name, ref value) => format!(
                    "(set_local ${}\n{})",
                    name,
                    value.get_str_indented(indent_level + 1)
                ),
                WASMExpr::BinOp(ref op, ref left, ref right, ref result_type) => format!(
                    "({}.{}\n{}\n{})",
                    result_type,
                    match op {
                        BinOp::Num(NumOp::Add) => "add",
                        BinOp::Num(NumOp::Sub) => "sub",
                        BinOp::Num(NumOp::Mul) => "mul",
                        BinOp::Num(NumOp::Div) => "div",
                        BinOp::Rel(RelOp::LEq) => "le_s",
                    },
                    left.get_str_indented(indent_level + 1),
                    right.get_str_indented(indent_level + 1)
                ),
                WASMExpr::PromoteInt(ref int) => format!(
                    "(f64.convert_s/i32\n{})",
                    int.get_str_indented(indent_level + 1)
                ),
                WASMExpr::If(ref condition, ref consequent, ref alternate) => format!(
                    "(if\n{}\n{}(then {})\n{}(else {}))",
                    condition.get_str_indented(indent_level + 1),
                    get_indent(indent_level + 1),
                    sequence_to_str_indented(consequent, indent_level + 2),
                    get_indent(indent_level + 1),
                    sequence_to_str_indented(alternate, indent_level + 2),
                ),
                WASMExpr::GrowMemory => format!("(grow_memory (i32.const 1))"),
                WASMExpr::Load(ref value_type, ref offset) => format!(
                    "({}.load\n{})",
                    value_type,
                    offset.get_str_indented(indent_level + 1)
                ),
                WASMExpr::Store(ref value_type, ref offset, ref value) => format!(
                    "({}.load\n{}\n{})",
                    value_type,
                    offset.get_str_indented(indent_level + 1),
                    value.get_str_indented(indent_level + 1)
                ),
                WASMExpr::Call {
                    ref param_type,
                    ref ret_type,
                    ref function_index,
                    ref arg,
                    ref captures,
                } => format!(
                    "(call_indirect (param {} i32) (result {})\n{}\n{}\n{})",
                    param_type,
                    ret_type,
                    arg.get_str_indented(indent_level + 1),
                    captures.get_str_indented(indent_level + 1),
                    function_index.get_str_indented(indent_level + 1)
                ),
            }
        )
    }
}
