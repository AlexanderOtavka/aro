use std::f64;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use untyped_ast::{Ast, BinOp, NumOp};

#[derive(Debug, PartialEq, Clone)]
pub enum CType {
    Float,
    Int,
    Bool,
    Object,
    Any,
    Ref(Box<CType>),
    Closure { param: Ast<CType>, ret: Ast<CType> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum CValue {
    Float(f64),
    Int(i32),
    Bool(bool),
    Ident(CName, CType),
    DerefBound(CName, CType),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CExpr {
    Value(CValue),
    BinOp(BinOp, Ast<CValue>, Ast<CValue>, CType),
    Call(Ast<CValue>, Ast<CValue>, CType),
    Not(Ast<CExpr>),
    AnyRefGet(Ast<CValue>, CType),
    ObjectAccess {
        object: Ast<CValue>,
        index: usize,
        field_type: CType,
    },
    AnyAccess {
        value: Ast<CValue>,
        value_type: CType,
    },
    Cast {
        value: Ast<CValue>,
        to_type: CType,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum CName {
    Expr(String, u64),
    Ident(String),
    Hook(Vec<String>),
    FuncArg,
    FuncCaptures,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CFuncName {
    Func(u64),
    AdaptorFunc(u64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CDeclaration(pub CType, pub CName);

#[derive(Debug, PartialEq, Clone)]
pub enum CStatement {
    VarAssign(CName, Ast<CExpr>),
    RefAlloc(CName, CType),
    RefAssign(CName, Ast<CExpr>),
    AnyRefAssign(Ast<CValue>, Ast<CValue>),
    AnyAssign(CName, Ast<CValue>),
    ClosureInit {
        name: CName,
        function: CFuncName,
        captures: Vec<Ast<CValue>>,
    },
    ObjectInit {
        name: CName,
        data: Vec<Ast<CValue>>,
    },
    Block(Vec<CDeclaration>, Vec<Ast<CStatement>>),
    If(Ast<CValue>, Ast<CStatement>, Ast<CStatement>),
    While(Ast<CValue>, Ast<CStatement>),
    PrintValue(Ast<CValue>),
    PrintText(String),
}

#[derive(Debug, Clone)]
pub struct CFunc {
    pub name: CFuncName,
    pub param: Ast<CType>,
    pub declarations: Vec<CDeclaration>,
    pub body: Vec<Ast<CStatement>>,
    pub ret: Ast<CValue>,
}

impl Ast<CValue> {
    pub fn to_c_expr(&self) -> Ast<CExpr> {
        self.replace_expr(CExpr::Value(*self.expr.clone()))
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

pub trait WellCTyped {
    fn get_ctype(&self) -> CType;
}

impl WellCTyped for CValue {
    fn get_ctype(&self) -> CType {
        match self {
            &CValue::Int(_) => CType::Int,
            &CValue::Float(_) => CType::Float,
            &CValue::Bool(_) => CType::Bool,
            &CValue::Ident(_, ref ctype) | &CValue::DerefBound(_, ref ctype) => ctype.clone(),
            &CValue::Null => CType::Object,
        }
    }
}

impl WellCTyped for CExpr {
    fn get_ctype(&self) -> CType {
        match self {
            &CExpr::Value(ref value) => value.get_ctype(),
            &CExpr::Not(_) => CType::Bool,
            &CExpr::AnyAccess {
                value_type: ref expr_type,
                ..
            }
            | &CExpr::ObjectAccess {
                field_type: ref expr_type,
                ..
            }
            | &CExpr::Cast {
                to_type: ref expr_type,
                ..
            }
            | &CExpr::BinOp(_, _, _, ref expr_type)
            | &CExpr::Call(_, _, ref expr_type)
            | &CExpr::AnyRefGet(_, ref expr_type) => expr_type.clone(),
        }
    }
}

fn get_hook_c_name(names: &Vec<String>) -> String {
    let mut string = String::from("_aro_hook__");

    for name in &names[..names.len() - 1] {
        string += name;
        string += "__";
    }

    // Replace trailing ! with __
    let last_name = &names[names.len() - 1];
    if &last_name[last_name.len() - 1..] == "!" {
        string += &last_name[..last_name.len() - 1];
        string += "__";
    } else {
        string += last_name;
    }

    string
}

impl Display for CValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &CValue::Float(value) => format!("{}", value),
                &CValue::Int(value) => format!("{}", value),
                &CValue::Bool(value) => format!("{}", value),
                &CValue::Ident(ref name, _) => format!("{}", name),
                &CValue::DerefBound(ref name, _) => format!("(**{})", name),
                &CValue::Null => String::from("NULL"),
            }
        )
    }
}

fn ctype_to_string(ctype: &CType, name: &str) -> String {
    match ctype {
        &CType::Any => format!("_Aro_Any {}", name),
        &CType::Bool => format!("bool {}", name),
        &CType::Float => format!("double {}", name),
        &CType::Int => format!("int {}", name),
        &CType::Object => format!("_Aro_Object {}", name),
        &CType::Closure { .. } => format!("_Aro_Closure {}", name),
        &CType::Ref(ref contained) => format!("{}* {}", ctype_to_string(contained, ""), name),
    }
}

fn ctype_to_union_field(ctype: &CType) -> &'static str {
    match ctype {
        &CType::Int => ".Int",
        &CType::Float => ".Float",
        &CType::Bool => ".Bool",
        &CType::Object => ".Object",
        &CType::Closure { .. } => ".Closure",
        &CType::Ref(_) => ".Ref",
        &CType::Any => "",
    }
}

impl Display for CType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", ctype_to_string(self, ""))
    }
}

impl Display for CExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &CExpr::ObjectAccess {
                    ref object,
                    ref index,
                    ref field_type,
                } => format!(
                    "({}{}[{}]{})",
                    match field_type {
                        &CType::Ref(_) => format!("({})", field_type),
                        _ => String::from(""),
                    },
                    object,
                    index,
                    ctype_to_union_field(field_type)
                ),
                &CExpr::Not(ref value) => format!("(!{})", value),
                &CExpr::AnyRefGet(ref reference, ref value_type) => {
                    format!("((*{}){})", reference, ctype_to_union_field(value_type))
                }
                &CExpr::AnyAccess {
                    ref value,
                    ref value_type,
                } => format!("({}{})", value, ctype_to_union_field(value_type)),
                &CExpr::Value(ref value) => format!("{}", value),
                &CExpr::BinOp(ref op, ref left, ref right, _) => format!(
                    "({}{} {} {})",
                    if op == &BinOp::Num(NumOp::Div) {
                        format!("({}) ", ctype_to_string(&CType::Float, ""))
                    } else {
                        String::from("")
                    },
                    left,
                    op,
                    right
                ),
                &CExpr::Call(ref closure, ref arg, ref ret_type) => format!(
                    "(*({}){}[0].Void_Ptr)({}, &{}[1])",
                    ctype_to_string(
                        &ret_type,
                        &format!(
                            "(*)({}, {})",
                            ctype_to_string(&arg.expr.get_ctype(), ""),
                            ctype_to_string(&CType::Object, ""),
                        )
                    ),
                    closure,
                    arg,
                    closure
                ),
                &CExpr::Cast {
                    ref value,
                    ref to_type,
                } => format!("(({}){})", to_type, value),
            }
        )
    }
}

fn init_array<Element: WellCTyped + Display>(
    name: &CName,
    elements: &Vec<Ast<Element>>,
    start_index: usize,
) -> String {
    let mut assignments = Vec::new();
    for (i, element) in elements.iter().enumerate() {
        assignments.push(format!(
            "{}[{}]{} = {};",
            name,
            i + start_index,
            ctype_to_union_field(&element.expr.get_ctype()),
            element
        ));
    }

    assignments.join(" ")
}

impl Display for CName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &CName::Hook(ref names) => get_hook_c_name(names),
                &CName::Expr(ref name, index) => format!("_aro_expr_{}_{}", name, index),
                &CName::Ident(ref name) => {
                    // Replace trailing ! with __
                    if &name[name.len() - 1..] == "!" {
                        format!("aro_{}__", &name[..name.len() - 1])
                    } else {
                        format!("aro_{}", name)
                    }
                }
                &CName::FuncArg => String::from("_aro_arg"),
                &CName::FuncCaptures => String::from("_aro_captures"),
            }
        )
    }
}

impl Display for CFuncName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &CFuncName::Func(index) => write!(f, "_aro_func_{}", index),
            &CFuncName::AdaptorFunc(index) => write!(f, "_aro_func_adaptor_{}", index),
        }
    }
}

impl Display for CDeclaration {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{};", ctype_to_string(&self.0, &format!("{}", self.1)))
    }
}

impl Display for CStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &CStatement::Block(ref declarations, ref statements) => {
                    sequence_to_str(&sequence_to_str("{ ", declarations, " "), statements, " }")
                }
                &CStatement::VarAssign(ref name, ref value) => format!("{} = {};", name, value),
                &CStatement::RefAlloc(ref name, ref value_type) => format!(
                    "{} = malloc(sizeof({}));",
                    name,
                    ctype_to_string(value_type, "")
                ),
                &CStatement::RefAssign(ref name, ref value) => format!("*{} = {};", name, value),
                &CStatement::AnyRefAssign(ref reference, ref value) => format!(
                    "(*{}){} = {};",
                    reference,
                    ctype_to_union_field(&value.expr.get_ctype()),
                    value
                ),
                &CStatement::AnyAssign(ref name, ref value) => format!(
                    "{}{} = {};",
                    name,
                    ctype_to_union_field(&value.expr.get_ctype()),
                    value
                ),
                &CStatement::ObjectInit { ref name, ref data } => format!(
                    "{} = malloc(sizeof(_Aro_Any) * {}); {}",
                    name,
                    data.len(),
                    init_array(name, data, 0)
                ),
                &CStatement::ClosureInit {
                    ref name,
                    ref function,
                    ref captures,
                } => format!(
                    "{} = malloc(sizeof(_Aro_Any) * {}); \
                     {}[0].Void_Ptr = {}; \
                     {}",
                    name,
                    captures.len() + 1,
                    name,
                    function,
                    init_array(name, captures, 1)
                ),
                &CStatement::If(ref condition, ref consequent, ref alternate) => {
                    format!("if ({}) {} else {}", condition, consequent, alternate)
                }
                &CStatement::While(ref condition, ref body) => {
                    format!("while ({}) {}", condition, body)
                }
                &CStatement::PrintValue(ref value) => match value.expr.get_ctype() {
                    CType::Bool => format!(
                        "if ({}) printf(\"#true()\"); else printf(\"#false()\");",
                        value
                    ),
                    CType::Int => format!("printf(\"%d\", {});", value),
                    CType::Float => format!("printf(\"%lf\", {});", value),
                    CType::Any => format!("printf(\"<Any %p>\", {}.Void_Ptr);", value),
                    CType::Closure { .. } => format!("printf(\"<Closure %p>\", {});", value),
                    CType::Object => format!("printf(\"<Object %p>\", {});", value),
                    CType::Ref(_) => format!("printf(\"<Ref %p>\", {});", value),
                },
                &CStatement::PrintText(ref text) => format!("printf(\"{}\");", text),
            }
        )
    }
}

impl CFunc {
    pub fn get_signature_string(&self) -> String {
        ctype_to_string(
            &self.ret.expr.get_ctype(),
            &format!(
                "{}({}, {})",
                self.name,
                ctype_to_string(&self.param.expr, "_aro_arg"),
                ctype_to_string(&CType::Object, "_aro_captures")
            ),
        )
    }
}

impl Display for CFunc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{} {{ {} {} return {}; }}",
            self.get_signature_string(),
            self.declarations
                .iter()
                .map(|statement| format!("{}", statement))
                .collect::<Vec<String>>()
                .join(" "),
            self.body
                .iter()
                .map(|statement| format!("{}", statement))
                .collect::<Vec<String>>()
                .join(" "),
            self.ret
        )
    }
}
