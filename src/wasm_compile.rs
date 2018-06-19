use c_ast::{CDeclaration, CExpr, CFunc, CFuncName, CName, CStatement, CType, CValue, WellCTyped};
use untyped_ast::{Ast, BinOp, NumOp};
use wasm_ast::{WASMDirectFuncName, WASMExpr, WASMFunc, WASMLocal, WASMType, WASMValue};

static UNIVERSAL_ALIGN: usize = 8; // The size of a union

pub fn get_func_index(name: &CFuncName) -> u64 {
    match name {
        CFuncName::Func(index) | CFuncName::AdaptorFunc(index) => *index,
    }
}

pub fn c_type_to_wasm(c_type: &CType) -> WASMType {
    match c_type {
        &CType::Int | &CType::Bool | &CType::Closure { .. } | &CType::Object | &CType::Ref(_) => {
            WASMType::I32
        }
        &CType::Any => WASMType::I64,
        &CType::Float => WASMType::F64,
    }
}

pub fn c_value_to_wasm(c_value: &Ast<CValue>) -> Ast<WASMExpr> {
    match &*c_value.expr {
        &CValue::Int(value) => {
            c_value.replace_expr(WASMExpr::Const(WASMType::I32, WASMValue::I32(value as i64)))
        }
        &CValue::Float(value) => {
            c_value.replace_expr(WASMExpr::Const(WASMType::F64, WASMValue::F64(value)))
        }
        &CValue::Ident(ref name, _) => c_value.replace_expr(WASMExpr::GetLocal(name.clone())),
        &CValue::Null => c_value.replace_expr(WASMExpr::Const(WASMType::I32, WASMValue::I32(0))),
        &CValue::DerefBound(ref name, ref value_type) => c_value.replace_expr(WASMExpr::Load(
            c_type_to_wasm(value_type),
            c_value.replace_expr(WASMExpr::Load(
                WASMType::I32,
                c_value.replace_expr(WASMExpr::GetLocal(name.clone())),
            )),
        )),
        _ => panic!("Unhandled value: {}", c_value),
    }
}

fn c_expr_to_wasm(c_expr: &Ast<CExpr>) -> Ast<WASMExpr> {
    match &*c_expr.expr {
        &CExpr::Value(ref value) => c_value_to_wasm(&c_expr.replace_expr(value.clone())),
        &CExpr::BinOp(ref op, ref left, ref right, ref op_type) => {
            let left_wasm_type = c_type_to_wasm(&left.expr.get_ctype());
            let right_wasm_type = c_type_to_wasm(&right.expr.get_ctype());
            let op_wasm_type = c_type_to_wasm(op_type);

            let left_wasm = c_value_to_wasm(left);
            let right_wasm = c_value_to_wasm(right);

            let casted_left = if op_wasm_type == WASMType::F64 && left_wasm_type == WASMType::I32 {
                left.replace_expr(WASMExpr::PromoteInt(left_wasm))
            } else {
                left_wasm
            };
            let casted_right = if op_wasm_type == WASMType::F64 && right_wasm_type == WASMType::I32
            {
                right.replace_expr(WASMExpr::PromoteInt(right_wasm))
            } else {
                right_wasm
            };

            c_expr.replace_expr(WASMExpr::BinOp(
                op.clone(),
                casted_left,
                casted_right,
                op_wasm_type,
            ))
        }
        &CExpr::Call(ref closure, ref arg, ref ret_type) => {
            let closure_wasm = c_value_to_wasm(closure);

            c_expr.replace_expr(WASMExpr::CallIndirect {
                param_types: vec![c_type_to_wasm(&arg.expr.get_ctype()), WASMType::I32],
                ret_type: c_type_to_wasm(ret_type),
                function_index: closure
                    .replace_expr(WASMExpr::Load(WASMType::I32, closure_wasm.clone())),
                args: vec![
                    c_value_to_wasm(arg),
                    closure.replace_expr(WASMExpr::BinOp(
                        BinOp::Num(NumOp::Add),
                        closure_wasm,
                        closure.replace_expr(WASMExpr::Const(
                            WASMType::I32,
                            WASMValue::I32(UNIVERSAL_ALIGN as i64),
                        )),
                        WASMType::I32,
                    )),
                ],
            })
        }
        _ => panic!("Unhandled expr: {}", c_expr),
    }
}

fn flatten_c_statement(
    c_statement: &Ast<CStatement>,
    wasm_locals: &mut Vec<WASMLocal>,
    wasm_exprs: &mut Vec<Ast<WASMExpr>>,
) {
    match &*c_statement.expr {
        CStatement::VarAssign(ref name, ref expr) => {
            wasm_exprs.push(
                c_statement.replace_expr(WASMExpr::SetLocal(name.clone(), c_expr_to_wasm(expr))),
            );
        }
        CStatement::RefAssign(ref name, ref value) => {
            wasm_exprs.push(c_statement.replace_expr(WASMExpr::Store(
                c_type_to_wasm(&value.expr.get_ctype()),
                c_statement.replace_expr(WASMExpr::GetLocal(name.clone())),
                c_expr_to_wasm(value),
            )));
        }
        CStatement::If(ref condition, ref consequent, ref alternate) => {
            let mut consequent_exprs = Vec::new();
            let mut alternate_exprs = Vec::new();

            flatten_c_statement(consequent, wasm_locals, &mut consequent_exprs);
            flatten_c_statement(alternate, wasm_locals, &mut alternate_exprs);

            wasm_exprs.push(c_statement.replace_expr(WASMExpr::If(
                c_value_to_wasm(condition),
                consequent_exprs,
                alternate_exprs,
            )));
        }
        CStatement::Block(ref c_declarations, ref c_statements) => {
            flatten_c_block(c_declarations, c_statements, wasm_locals, wasm_exprs);
        }
        CStatement::ClosureInit {
            ref name,
            ref function,
            ref captures,
        } => {
            // Heap allocate a new buffer and store the address in `name` local
            wasm_exprs.push(c_statement.replace_expr(WASMExpr::SetLocal(
                name.clone(),
                c_statement.replace_expr(WASMExpr::Call(
                    WASMDirectFuncName::HeapAlloc,
                    vec![c_statement.replace_expr(WASMExpr::Const(
                        WASMType::I32,
                        WASMValue::I32(((captures.len() + 1) * UNIVERSAL_ALIGN) as i64),
                    ))],
                )),
            )));

            // Store the function index first in the closure
            wasm_exprs.push(c_statement.replace_expr(WASMExpr::Store(
                WASMType::I32,
                c_statement.replace_expr(WASMExpr::GetLocal(name.clone())),
                c_statement.replace_expr(WASMExpr::Const(
                    WASMType::I32,
                    WASMValue::I32(get_func_index(function) as i64),
                )),
            )));

            // Fill the rest of the closure with captures aligned at
            // UNIVERSAL_ALIGN
            for (index, capture) in captures.into_iter().enumerate() {
                wasm_exprs.push(capture.replace_expr(WASMExpr::Store(
                    c_type_to_wasm(&capture.expr.get_ctype()),
                    capture.replace_expr(WASMExpr::BinOp(
                        BinOp::Num(NumOp::Add),
                        capture.replace_expr(WASMExpr::GetLocal(name.clone())),
                        capture.replace_expr(WASMExpr::Const(
                            WASMType::I32,
                            WASMValue::I32((index * UNIVERSAL_ALIGN) as i64),
                        )),
                        WASMType::I32,
                    )),
                    c_value_to_wasm(capture),
                )));
            }
        }
        &CStatement::RefAlloc(ref name, _) => {
            // Heap allocate a new buffer and store the address in `name` local
            wasm_exprs.push(c_statement.replace_expr(WASMExpr::SetLocal(
                name.clone(),
                c_statement.replace_expr(WASMExpr::Call(
                    WASMDirectFuncName::HeapAlloc,
                    vec![c_statement.replace_expr(WASMExpr::Const(
                        WASMType::I32,
                        WASMValue::I32(UNIVERSAL_ALIGN as i64),
                    ))],
                )),
            )));
        }
        _ => panic!("Unhandled statement: {}", c_statement),
    };
}

pub fn flatten_c_block(
    c_declarations: &Vec<CDeclaration>,
    c_statements: &Vec<Ast<CStatement>>,
    wasm_locals: &mut Vec<WASMLocal>,
    wasm_exprs: &mut Vec<Ast<WASMExpr>>,
) {
    for declaration in c_declarations {
        wasm_locals.push(WASMLocal(
            declaration.1.clone(),
            c_type_to_wasm(&declaration.0),
        ))
    }

    for statement in c_statements {
        flatten_c_statement(statement, wasm_locals, wasm_exprs);
    }
}

pub fn c_func_to_wasm(c_func: &CFunc) -> WASMFunc {
    let mut locals = Vec::new();
    let mut body = Vec::new();

    flatten_c_block(&c_func.declarations, &c_func.body, &mut locals, &mut body);

    body.push(c_value_to_wasm(&c_func.ret));

    WASMFunc {
        name: c_func.name.clone(),
        params: vec![
            (CName::FuncArg, c_type_to_wasm(&c_func.param.expr)),
            (CName::FuncCaptures, WASMType::I32),
        ],
        result: c_type_to_wasm(&c_func.ret.expr.get_ctype()),
        locals,
        body,
    }
}
