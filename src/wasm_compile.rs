use c_ast::{CDeclaration, CExpr, CStatement, CType, CValue};
use untyped_ast::Ast;
use wasm_ast::{WASMExpr, WASMLocal, WASMType, WASMValue};

pub fn c_type_to_wasm(c_type: &CType) -> WASMType {
    match c_type {
        &CType::Int | &CType::Bool | &CType::Closure { .. } | &CType::Object | &CType::Ref(_) => {
            WASMType::I32
        }
        &CType::Any => WASMType::I64,
        &CType::Float => WASMType::F64,
    }
}

pub fn c_value_to_wasm(c_value: &CValue) -> WASMExpr {
    match c_value {
        &CValue::Int(value) => WASMExpr::Const(WASMType::I32, WASMValue::I32(value as i64)),
        &CValue::Float(value) => WASMExpr::Const(WASMType::F64, WASMValue::F64(value)),
        _ => panic!(),
    }
}

fn c_expr_to_wasm(c_expr: &Ast<CExpr>) -> Ast<WASMExpr> {
    c_expr.replace_expr(match &*c_expr.expr {
        &CExpr::Value(ref value) => c_value_to_wasm(value),
        _ => panic!(),
    })
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
        wasm_exprs.push(statement.replace_expr(match &*statement.expr {
            CStatement::VarAssign(ref name, ref expr) => {
                WASMExpr::SetLocal(name.clone(), c_expr_to_wasm(expr))
            }
            _ => panic!(),
        }))
    }
}
