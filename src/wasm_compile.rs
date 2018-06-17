use c_ast::{CDeclaration, CExpr, CStatement, CType, CValue, WellCTyped};
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
        &CValue::Ident(ref name, _) => WASMExpr::GetLocal(name.clone()),
        _ => panic!(),
    }
}

fn c_expr_to_wasm(c_expr: &Ast<CExpr>) -> Ast<WASMExpr> {
    c_expr.replace_expr(match &*c_expr.expr {
        &CExpr::Value(ref value) => c_value_to_wasm(value),
        &CExpr::BinOp(ref op, ref left, ref right, ref op_type) => {
            let left_wasm_type = c_type_to_wasm(&left.expr.get_ctype());
            let right_wasm_type = c_type_to_wasm(&right.expr.get_ctype());
            let op_wasm_type = c_type_to_wasm(op_type);

            let left_wasm = left.replace_expr(c_value_to_wasm(&left.expr));
            let right_wasm = right.replace_expr(c_value_to_wasm(&right.expr));

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

            WASMExpr::BinOp(op.clone(), casted_left, casted_right, op_wasm_type)
        }
        _ => panic!(),
    })
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
        CStatement::If(ref condition, ref consequent, ref alternate) => {
            let mut consequent_exprs = Vec::new();
            let mut alternate_exprs = Vec::new();

            flatten_c_statement(consequent, wasm_locals, &mut consequent_exprs);
            flatten_c_statement(alternate, wasm_locals, &mut alternate_exprs);

            wasm_exprs.push(c_statement.replace_expr(WASMExpr::If(
                condition.replace_expr(c_value_to_wasm(&condition.expr)),
                consequent_exprs,
                alternate_exprs,
            )));
        }
        CStatement::Block(ref c_declarations, ref c_statements) => {
            flatten_c_block(c_declarations, c_statements, wasm_locals, wasm_exprs);
        }
        _ => panic!(),
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