use std::cell::RefCell;
use std::collections::HashMap;
use std::f64;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Ast<T> {
    pub left_loc: usize,
    pub right_loc: usize,
    pub expr: Box<T>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedAst<Expr> {
    pub left_loc: usize,
    pub right_loc: usize,
    pub expr: Box<Expr>,
    pub expr_type: Box<EvaluatedType>,
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
pub enum TypedExpression {
    Value(TypedValue),
    BinOp(BinOp, TypedAst<TypedExpression>, TypedAst<TypedExpression>),
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
pub enum Pattern {
    Ident(String, Ast<Type>),
    Tuple(Vec<Ast<Pattern>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedPattern {
    Ident(String),
    Tuple(Vec<TypedAst<TypedPattern>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Num,
    Bool,
    Any,
    None,
    Ident(String),
    Func(Ast<Type>, Ast<Type>),
    GenericFunc(String, Ast<Type>, Ast<Type>),
    Tuple(Vec<Ast<Type>>),
    List(Ast<Type>),
    Ref(Ast<Type>),
    Record(HashMap<String, Ast<Type>>),
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

#[derive(Debug, PartialEq, Clone)]
pub enum CType {
    Float,
    Int,
    Bool,
    Object,
    Any,
    VoidPtr,
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
    Not(Ast<CExpr>),
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
    Func(u64),
    AdaptorFunc(u64),
    Ident(String),
    Hook(Vec<String>),
    FuncArg,
    FuncCaptures,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CDeclaration(pub CType, pub CName);

#[derive(Debug, PartialEq, Clone)]
pub enum CStatement {
    VarAssign(CName, Ast<CExpr>),
    RefAlloc(CName, CType),
    RefAssign(CName, Ast<CExpr>),
    AnyAssign(CName, Ast<CValue>),
    ClosureInit {
        name: CName,
        function: Ast<CValue>,
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
    pub name: CName,
    pub param: Ast<CType>,
    pub declarations: Vec<CDeclaration>,
    pub body: Vec<Ast<CStatement>>,
    pub ret: Ast<CValue>,
}

impl<T> Ast<T> {
    pub fn new(left_loc: usize, right_loc: usize, expr: T) -> Ast<T> {
        Ast {
            left_loc,
            right_loc,
            expr: Box::new(expr),
        }
    }

    pub fn replace_expr<U>(&self, expr: U) -> Ast<U> {
        Ast::new(self.left_loc, self.right_loc, expr)
    }

    pub fn to_typed<NewExpr>(&self, expr: NewExpr, expr_type: EvaluatedType) -> TypedAst<NewExpr> {
        TypedAst {
            left_loc: self.left_loc,
            right_loc: self.right_loc,
            expr: Box::new(expr),
            expr_type: Box::new(expr_type),
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

impl Ast<CValue> {
    pub fn to_c_expr(&self) -> Ast<CExpr> {
        self.replace_expr(CExpr::Value(*self.expr.clone()))
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
                    "}",
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
                &Type::None => String::from("None"),
                &Type::Ident(ref name) => format!("({})", name),
                &Type::Func(ref input, ref output) => format!("({} => {})", input, output),
                &Type::GenericFunc(ref name, ref supertype, ref output) => {
                    format!("({}: {} => {})", name, supertype, output)
                }
                &Type::Tuple(ref vec) => sequence_to_str("(", vec, ")"),
                &Type::List(ref element_type) => format!("[{}]", element_type),
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
                    "}",
                ),
            }
        )
    }
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
                &EvaluatedType::Ref(ref value_type) => format!("(Ref <| {})", value_type),
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
            | &CExpr::BinOp(_, _, _, ref expr_type) => expr_type.clone(),
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
        &CType::VoidPtr => format!("void* {}", name),
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
        &CType::VoidPtr => ".Void_Ptr",
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
                &CExpr::AnyAccess {
                    ref value,
                    ref value_type,
                } => format!("({}{})", value, ctype_to_union_field(value_type)),
                &CExpr::Value(ref value) => format!("{}", value),
                &CExpr::BinOp(ref op, ref left, ref right, _) => match op {
                    &BinOp::Add => format!("({} + {})", left, right),
                    &BinOp::Sub => format!("({} - {})", left, right),
                    &BinOp::Mul => format!("({} * {})", left, right),
                    &BinOp::Div => format!(
                        "(({}) {} / {})",
                        ctype_to_string(&CType::Float, ""),
                        left,
                        right
                    ),
                    &BinOp::Call => {
                        if let CType::Closure { ref param, ref ret } = left.expr.get_ctype() {
                            format!(
                                "(*({}){}[0].Void_Ptr)({}, &{}[1])",
                                ctype_to_string(
                                    &ret.expr,
                                    &format!(
                                        "(*)({}, {})",
                                        ctype_to_string(&param.expr, ""),
                                        ctype_to_string(&CType::Object, ""),
                                    )
                                ),
                                left,
                                right,
                                left
                            )
                        } else {
                            panic!(
                                "Cannot do call with {}",
                                ctype_to_string(&left.expr.get_ctype(), "")
                            )
                        }
                    }
                    &BinOp::LEq => format!("({} <= {})", left, right),
                },
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
                &CName::Func(index) => format!("_aro_func_{}", index),
                &CName::AdaptorFunc(index) => format!("_aro_func_adaptor_{}", index),
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
                    CType::VoidPtr => format!("printf(\"%p\", {});", value),
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
