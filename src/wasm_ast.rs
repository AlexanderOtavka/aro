use c_ast::{CFuncName, CName};
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
pub enum WASMDirectFuncName {
    HeapAlloc,
    StackAlloc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum WASMGlobalName {
    StackPointer,
    Temp,
}

#[derive(Debug, PartialEq, Clone)]
pub enum WASMExpr {
    Const(WASMType, WASMValue),
    GetLocal(CName),
    SetGlobal(WASMGlobalName, Ast<WASMExpr>),
    GetGlobal(WASMGlobalName),
    BinOp(BinOp, Ast<WASMExpr>, Ast<WASMExpr>, WASMType),
    PromoteInt(Ast<WASMExpr>),
    TruncateFloat(Ast<WASMExpr>),
    If(Ast<WASMExpr>, Vec<Ast<WASMExpr>>, Vec<Ast<WASMExpr>>),
    Load(WASMType, Ast<WASMExpr>),
    Store(WASMType, Ast<WASMExpr>, Ast<WASMExpr>),
    Call(WASMDirectFuncName, Vec<Ast<WASMExpr>>),
    CallIndirect {
        param_types: Vec<WASMType>,
        ret_type: WASMType,
        function_index: Ast<WASMExpr>,
        args: Vec<Ast<WASMExpr>>,
    },
    Sequence(Vec<Ast<WASMExpr>>),
}

pub type StackMap = Vec<WASMType>;

#[derive(Debug, PartialEq, Clone)]
pub struct WASMFunc {
    pub name: CFuncName,
    pub params: Vec<(CName, WASMType)>,
    pub result: WASMType,
    pub stack_map: StackMap,
    pub body: Vec<Ast<WASMExpr>>,
}

impl Display for WASMDirectFuncName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            WASMDirectFuncName::HeapAlloc => write!(f, "$_alloc"),
            WASMDirectFuncName::StackAlloc => write!(f, "$_alloc"),
        }
    }
}

impl Display for WASMGlobalName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            WASMGlobalName::StackPointer => write!(f, "$_stack_pointer"),
            WASMGlobalName::Temp => write!(f, "$_temp"),
        }
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
    let mut indent = String::from("\n");

    for _ in 0..indent_level {
        indent += "  ";
    }

    indent
}

fn sequence_to_str_indented(sequence: &Vec<Ast<WASMExpr>>, indent_level: u32) -> String {
    let mut string = String::new();

    for element in sequence {
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
                WASMExpr::GetGlobal(ref name) => format!("(get_global {})", name),
                WASMExpr::SetGlobal(ref name, ref value) => format!(
                    "(set_global {}{})",
                    name,
                    value.get_str_indented(indent_level + 1)
                ),
                WASMExpr::BinOp(ref op, ref left, ref right, ref result_type) => format!(
                    "({}.{}{}{})",
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
                    "(f64.convert_s/i32{})",
                    int.get_str_indented(indent_level + 1)
                ),
                WASMExpr::TruncateFloat(ref float) => format!(
                    "(i32.trunc_s/f64{})",
                    float.get_str_indented(indent_level + 1)
                ),
                WASMExpr::If(ref condition, ref consequent, ref alternate) => format!(
                    "(if{}{}(then{}){}(else{}))",
                    condition.get_str_indented(indent_level + 1),
                    get_indent(indent_level + 1),
                    sequence_to_str_indented(consequent, indent_level + 2),
                    get_indent(indent_level + 1),
                    sequence_to_str_indented(alternate, indent_level + 2),
                ),
                WASMExpr::Load(ref value_type, ref offset) => format!(
                    "({}.load{})",
                    value_type,
                    offset.get_str_indented(indent_level + 1)
                ),
                WASMExpr::Store(ref value_type, ref offset, ref value) => format!(
                    "({}.store{}{})",
                    value_type,
                    offset.get_str_indented(indent_level + 1),
                    value.get_str_indented(indent_level + 1)
                ),
                WASMExpr::Call(ref name, ref args) => format!(
                    "(call {}{})",
                    name,
                    sequence_to_str_indented(args, indent_level + 1)
                ),
                WASMExpr::CallIndirect {
                    ref param_types,
                    ref ret_type,
                    ref function_index,
                    ref args,
                } => format!(
                    "(call_indirect (param {}) (result {}){}{})",
                    param_types
                        .into_iter()
                        .map(|param_type| format!("{}", param_type))
                        .collect::<Vec<String>>()
                        .join(" "),
                    ret_type,
                    sequence_to_str_indented(args, indent_level + 1),
                    function_index.get_str_indented(indent_level + 1)
                ),
                WASMExpr::Sequence(ref exprs) => format!(
                    "(;sequence;){}",
                    sequence_to_str_indented(exprs, indent_level + 1)
                ),
            }
        )
    }
}

impl Display for WASMFunc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "(func ${} {} (result {}){})",
            self.name,
            self.params
                .iter()
                .map(|(name, param_type)| format!("(param ${} {})", name, param_type))
                .collect::<Vec<String>>()
                .join(" "),
            self.result,
            sequence_to_str_indented(&self.body, 1)
        )
    }
}
