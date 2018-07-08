use c_ast::{CFuncName, CName};
use std::fmt::{self, Display, Formatter};
use untyped_ast::{Ast, BinOp, NumOp, RelOp};

#[derive(Debug, PartialEq, Clone)]
pub enum WAsmType {
    I32,
    I64,
    F64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum WAsmValue {
    I32(i64), // Extra space for unsigned integers
    F64(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct WAsmHookName(pub Vec<String>);

#[derive(Debug, PartialEq, Clone)]
pub enum WAsmDirectFuncName {
    HeapAlloc,
    StackAlloc,
    Hook(WAsmHookName),
}

#[derive(Debug, PartialEq, Clone)]
pub enum WAsmGlobalName {
    StackPointer,
    RegisterI32_1,
    RegisterI32_2,
}

#[derive(Debug, PartialEq, Clone)]
pub enum WAsmExpr {
    Const(WAsmType, WAsmValue),
    GetLocal(CName),
    SetGlobal(WAsmGlobalName, Ast<WAsmExpr>),
    GetGlobal(WAsmGlobalName),
    BinOp(BinOp, Ast<WAsmExpr>, Ast<WAsmExpr>, WAsmType),
    PromoteInt(Ast<WAsmExpr>),
    TruncateFloat(Ast<WAsmExpr>),
    If(Ast<WAsmExpr>, Vec<Ast<WAsmExpr>>, Vec<Ast<WAsmExpr>>),
    Load(WAsmType, Ast<WAsmExpr>),
    Store(WAsmType, Ast<WAsmExpr>, Ast<WAsmExpr>),
    Call(WAsmDirectFuncName, Vec<Ast<WAsmExpr>>),
    CallIndirect {
        param_types: Vec<WAsmType>,
        ret_type: WAsmType,
        function_index: Ast<WAsmExpr>,
        args: Vec<Ast<WAsmExpr>>,
    },
    Sequence(Vec<Ast<WAsmExpr>>),
}

pub type StackMap = Vec<WAsmType>;

#[derive(Debug, PartialEq, Clone)]
pub struct WAsmHookImport {
    pub name: WAsmHookName,
    pub params: Vec<WAsmType>,
    pub result: WAsmType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WAsmFunc {
    pub name: CFuncName,
    pub params: Vec<(CName, WAsmType)>,
    pub result: WAsmType,
    pub stack_map: StackMap,
    pub body: Vec<Ast<WAsmExpr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WAsmModule {
    pub hook_imports: Vec<WAsmHookImport>,
    pub table_offset: u32,
    pub table: Vec<WAsmFunc>,
    pub main_init: Vec<Ast<WAsmExpr>>,
    pub main_body: Vec<Ast<WAsmExpr>>,
    pub main_return: Option<(Ast<WAsmExpr>, WAsmType)>,
}

impl Display for WAsmHookName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "${}", self.0.join("."))
    }
}

impl Display for WAsmDirectFuncName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            WAsmDirectFuncName::Hook(ref name) => name.fmt(f),
            WAsmDirectFuncName::HeapAlloc => write!(f, "$_alloc"),
            WAsmDirectFuncName::StackAlloc => write!(f, "$_alloc"),
        }
    }
}

impl Display for WAsmGlobalName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            WAsmGlobalName::StackPointer => write!(f, "$_stack_pointer"),
            WAsmGlobalName::RegisterI32_1 => write!(f, "$_register_i32_1"),
            WAsmGlobalName::RegisterI32_2 => write!(f, "$_register_i32_2"),
        }
    }
}

impl Display for WAsmHookImport {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let WAsmHookName(ref path) = self.name;
        write!(
            f,
            "(import{} (func {}{} (result {})))",
            path.into_iter()
                .map(|name| format!(" \"{}\"", name))
                .collect::<Vec<String>>()
                .join(""),
            self.name,
            self.params
                .iter()
                .map(|param| format!(" (param {})", param))
                .collect::<Vec<String>>()
                .join(""),
            self.result
        )
    }
}

impl Display for WAsmValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            WAsmValue::I32(value) => write!(f, "{:#x}", value),
            WAsmValue::F64(value) => write!(f, "{}", value),
        }
    }
}

impl Display for WAsmType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                WAsmType::I32 => "i32",
                WAsmType::I64 => "i64",
                WAsmType::F64 => "f64",
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

fn sequence_to_str_indented(sequence: &Vec<Ast<WAsmExpr>>, indent_level: u32) -> String {
    let mut string = String::new();

    for element in sequence {
        string += &element.get_str_indented(indent_level);
    }

    string
}

impl Ast<WAsmExpr> {
    pub fn get_str_indented(&self, indent_level: u32) -> String {
        self.expr.get_str_indented(indent_level)
    }
}

impl WAsmExpr {
    pub fn get_str_indented(&self, indent_level: u32) -> String {
        format!(
            "{}{}",
            get_indent(indent_level),
            match self {
                WAsmExpr::Const(ref value_type, ref value) => {
                    format!("({}.const {})", value_type, value)
                }
                WAsmExpr::GetLocal(ref name) => format!("(get_local ${})", name),
                WAsmExpr::GetGlobal(ref name) => format!("(get_global {})", name),
                WAsmExpr::SetGlobal(ref name, ref value) => format!(
                    "(set_global {}{})",
                    name,
                    value.get_str_indented(indent_level + 1)
                ),
                WAsmExpr::BinOp(ref op, ref left, ref right, ref result_type) => format!(
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
                WAsmExpr::PromoteInt(ref int) => format!(
                    "(f64.convert_s/i32{})",
                    int.get_str_indented(indent_level + 1)
                ),
                WAsmExpr::TruncateFloat(ref float) => format!(
                    "(i32.trunc_s/f64{})",
                    float.get_str_indented(indent_level + 1)
                ),
                WAsmExpr::If(ref condition, ref consequent, ref alternate) => format!(
                    "(if{}{}(then{}){}(else{}))",
                    condition.get_str_indented(indent_level + 1),
                    get_indent(indent_level + 1),
                    sequence_to_str_indented(consequent, indent_level + 2),
                    get_indent(indent_level + 1),
                    sequence_to_str_indented(alternate, indent_level + 2),
                ),
                WAsmExpr::Load(ref value_type, ref offset) => format!(
                    "({}.load{})",
                    value_type,
                    offset.get_str_indented(indent_level + 1)
                ),
                WAsmExpr::Store(ref value_type, ref offset, ref value) => format!(
                    "({}.store{}{})",
                    value_type,
                    offset.get_str_indented(indent_level + 1),
                    value.get_str_indented(indent_level + 1)
                ),
                WAsmExpr::Call(ref name, ref args) => format!(
                    "(call {}{})",
                    name,
                    sequence_to_str_indented(args, indent_level + 1)
                ),
                WAsmExpr::CallIndirect {
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
                WAsmExpr::Sequence(ref exprs) => format!(
                    "(;sequence;){}",
                    sequence_to_str_indented(exprs, indent_level + 1)
                ),
            }
        )
    }
}

impl Display for WAsmFunc {
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

impl Display for WAsmModule {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            ";; Generated by {} {}\
             \n{}\
             \n\
             \n(import \"host\" \"memory\" (memory 3))\
             \n(import \"host\" \"print\" (func $host.print (param i32)))\
             \n\
             \n;; Reserve first 4 bytes for stack free pointer\
             \n;; Reserve next 4 bytes for heap free pointer\
             \n(data (i32.const 4) \"\\00\\00\\02\\00\") ;; Memory Page Index 2\
             \n(func $_alloc (param $size i32) (result i32)\
             \n  (i32.load (i32.const 4)) ;; Return value\
             \n  (i32.store (i32.const 4)\
             \n    (i32.add\
             \n      (i32.load (i32.const 4))\
             \n      (get_local $size))))\
             \n\
             \n(global $_stack_pointer (mut i32) (i32.const 0x10_000)) ;; Memory Page Index 1\
             \n(global $_register_i32_1 (mut i32) (i32.const 0))\
             \n(global $_register_i32_2 (mut i32) (i32.const 0))\
             \n\
             \n(table {} anyfunc)\
             \n(elem (i32.const {}){})\
             \n{}\
             \n(func (export \"main\"){}\
             \n  ;; Init{}\
             \n  ;; Body{}\
             \n  ;; Return{})",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION"),
            self.hook_imports
                .iter()
                .map(|hook_import| format!("\n{}", hook_import))
                .collect::<Vec<String>>()
                .join(""),
            self.table_offset as usize + self.table.len(),
            self.table_offset,
            self.table
                .iter()
                .map(|wasm_func| format!("\n    ${}", wasm_func.name))
                .collect::<Vec<String>>()
                .join(""),
            self.table
                .iter()
                .map(|wasm_func| format!("\n{}\n", wasm_func))
                .collect::<Vec<String>>()
                .join(""),
            match self.main_return {
                Some((_, ref ret_type)) => format!(" (result {})", ret_type),
                None => String::from(""),
            },
            self.main_init
                .iter()
                .map(|expr| expr.get_str_indented(1))
                .collect::<Vec<String>>()
                .join(""),
            self.main_body
                .iter()
                .map(|expr| expr.get_str_indented(1))
                .collect::<Vec<String>>()
                .join(""),
            match self.main_return {
                Some((ref ret_value, _)) => ret_value.get_str_indented(1),
                None => String::from(""),
            },
        )
    }
}
