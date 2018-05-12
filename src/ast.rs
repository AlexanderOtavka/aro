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
pub struct TypedAst<Expr, ExprType> {
    pub left_loc: usize,
    pub right_loc: usize,
    pub expr: Box<Expr>,
    pub expr_type: Box<ExprType>,
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
    BinOp(
        BinOp,
        TypedAst<TypedExpression, Type>,
        TypedAst<TypedExpression, Type>,
    ),
    If(
        TypedAst<TypedExpression, Type>,
        TypedAst<TypedExpression, Type>,
        TypedAst<TypedExpression, Type>,
    ),
    Ident(String),
    Let(
        TypedAst<TypedPattern, Type>,
        TypedAst<TypedExpression, Type>,
        TypedAst<TypedExpression, Type>,
    ),
    Sequence(
        TypedAst<TypedExpression, Type>,
        TypedAst<TypedExpression, Type>,
    ),
    RecordAccess(TypedAst<TypedExpression, Type>, String),
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
        TypedAst<TypedPattern, Type>,
        TypedAst<TypedExpression, Type>,
    ),
    Tuple(Vec<TypedAst<TypedExpression, Type>>),
    List(Vec<TypedAst<TypedExpression, Type>>),
    Hook(Vec<String>),
    Record(HashMap<String, TypedAst<TypedExpression, Type>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Ident(String, Ast<Type>),
    Tuple(Vec<Ast<Pattern>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedPattern {
    Ident(String),
    Tuple(Vec<TypedAst<TypedPattern, Type>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Num,
    Bool,
    Any,
    Empty,
    Ident(String, Option<Ast<Type>>),
    Func(Ast<Type>, Ast<Type>),
    GenericFunc(String, Ast<Type>, Ast<Type>),
    Tuple(Vec<Ast<Type>>),
    List(Ast<Type>),
    Ref(Ast<Type>),
    Record(HashMap<String, Ast<Type>>),
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
    Ident(String, CType),
    Deref(String, CType),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CExpr {
    Value(CValue),
    BinOp(BinOp, Ast<CValue>, Ast<CValue>),
    ObjectAccess {
        object: Ast<CExpr>,
        index: usize,
        field_type: Ast<CType>,
    },
    AnyAccess {
        value: Ast<CExpr>,
        value_type: CType,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum CStatement {
    VarDecl(CType, String),
    VarAssign(String, Ast<CExpr>),
    AnyAssign {
        name: String,
        value_type: CType,
        value: Ast<CExpr>,
    },
    RefAlloc(String, CType),
    RefAssign(String, Ast<CExpr>),
    ClosureInit {
        name: String,
        function: Ast<CValue>,
        captures: Vec<Ast<CValue>>,
    },
    ObjectInit {
        name: String,
        data: Vec<Ast<CValue>>,
    },
    Block(Vec<Ast<CStatement>>),
    If(Ast<CExpr>, Ast<CStatement>, Ast<CStatement>),
}

#[derive(Debug, Clone)]
pub struct CFunc {
    pub name: String,
    pub param: Ast<CType>,
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

    pub fn to_typed<NewExpr, NewType>(
        &self,
        expr: NewExpr,
        expr_type: NewType,
    ) -> TypedAst<NewExpr, NewType> {
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

impl<Expr, ExprType> TypedAst<Expr, ExprType>
where
    ExprType: Clone,
{
    pub fn replace_untyped<NewExpr>(&self, expr: NewExpr) -> Ast<NewExpr> {
        Ast {
            left_loc: self.left_loc,
            right_loc: self.right_loc,
            expr: Box::new(expr),
        }
    }

    pub fn to_type_ast(&self) -> Ast<ExprType> {
        Ast::new(self.left_loc, self.right_loc, *self.expr_type.clone())
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
                &Type::Ident(ref name, _) => format!("({})", name),
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

fn ctype_to_string(ctype: &CType, name: &str) -> String {
    match ctype {
        &CType::Any => format!("_Aro_Any {}", name),
        &CType::Bool => format!("bool {}", name),
        &CType::Float => format!("double {}", name),
        &CType::Int => format!("int {}", name),
        &CType::Object | &CType::Closure { .. } => format!("_Aro_Any* {}", name),
        &CType::VoidPtr => format!("void* {}", name),
        &CType::Ref(ref contained) => format!("{}* {}", ctype_to_string(contained, ""), name),
    }
}

impl CValue {
    pub fn get_ctype(&self) -> CType {
        match self {
            &CValue::Int(_) => CType::Int,
            &CValue::Float(_) => CType::Float,
            &CValue::Bool(_) => CType::Bool,
            &CValue::Ident(_, ref ctype) | &CValue::Deref(_, ref ctype) => ctype.clone(),
        }
    }
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
                &CValue::Deref(ref name, _) => format!("(*{})", name),
            }
        )
    }
}

fn ctype_to_union_field(ctype: &CType) -> &'static str {
    match ctype {
        &CType::Int => "Int",
        &CType::Float => "Float",
        &CType::Bool => "Bool",
        &CType::Object | &CType::Closure { .. } => "Any_Ptr",
        &CType::VoidPtr | &CType::Ref(_) => "Void_Ptr",
        &CType::Any => panic!("Can't pull Any out of a union"),
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
                    "(({}){}[{}].{})",
                    ctype_to_string(&field_type.expr, ""),
                    object,
                    index,
                    ctype_to_union_field(&field_type.expr)
                ),
                &CExpr::AnyAccess {
                    ref value,
                    ref value_type,
                } => format!("({}.{})", value, ctype_to_union_field(value_type)),
                &CExpr::Value(ref value) => format!("{}", value),
                &CExpr::BinOp(ref op, ref left, ref right) => match op {
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
            }
        )
    }
}

fn init_array(name: &str, elements: &Vec<Ast<CValue>>, start_index: usize) -> String {
    let mut assignments = Vec::new();
    for (i, element) in elements.iter().enumerate() {
        assignments.push(format!(
            "{}[{}].{} = {};",
            name,
            i + start_index,
            ctype_to_union_field(&element.expr.get_ctype()),
            element
        ));
    }

    assignments.join(" ")
}

impl Display for CStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &CStatement::Block(ref statements) => sequence_to_str("{ ", statements, " }"),
                &CStatement::VarDecl(ref var_type, ref name) => {
                    format!("{};", ctype_to_string(var_type, name))
                }
                &CStatement::VarAssign(ref name, ref value) => format!("{} = {};", name, value),
                &CStatement::AnyAssign {
                    ref name,
                    ref value,
                    ref value_type,
                } => format!("{}.{} = {};", name, ctype_to_union_field(value_type), value),
                &CStatement::RefAlloc(ref name, ref value_type) => format!(
                    "{} = malloc(sizeof({}));",
                    name,
                    ctype_to_string(value_type, "")
                ),
                &CStatement::RefAssign(ref name, ref value) => format!("*{} = {};", name, value),
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
            "{} {{ {} return {}; }}",
            self.get_signature_string(),
            self.body
                .clone()
                .into_iter()
                .map(|statement| format!("{}", statement))
                .collect::<Vec<String>>()
                .join(" "),
            self.ret
        )
    }
}
