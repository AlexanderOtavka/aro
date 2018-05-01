use ast::{Ast, BinOp, CExpr, CStatement, CType, CValue, Expression, Value};

fn get_expr_name(expr_index: &mut i32) -> String {
    let old_i = *expr_index;
    *expr_index += 1;
    format!("_aro_expr_{}", old_i)
}

pub fn lift_expr(
    ast: &Ast<Expression>,
    scope: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
) -> Ast<CExpr> {
    match &*ast.expr {
        &Expression::Value(ref v) => match v {
            &Value::Num(value) => ast.replace_expr(CExpr::Value(CValue::Float(value))),
            _ => panic!(),
        },
        &Expression::BinOp(ref op, ref left, ref right) => match op {
            &BinOp::Add => {
                let left_c_ast = lift_expr(left, scope, expr_index);
                let right_c_ast = lift_expr(right, scope, expr_index);

                let expr_name = get_expr_name(expr_index);
                scope.push(ast.replace_expr(CStatement::VarDecl(CType::Float, expr_name.clone())));
                scope.push(ast.replace_expr(CStatement::VarAssign(
                    expr_name.clone(),
                    CExpr::BinOp(BinOp::Add, left_c_ast, right_c_ast),
                )));

                ast.replace_expr(CExpr::Ident(expr_name))
            }
            _ => panic!(),
        },
        _ => panic!(),
    }
}

#[cfg(test)]
mod lift_expr {
    use super::*;
    use parse::source_to_ast;

    fn assert_lift(aro_code: &str, expected_scope: Vec<&str>, expected_expr: &str) {
        let mut scope = Vec::new();
        let mut expr_index = 0;
        let actual_expr = lift_expr(
            &source_to_ast(aro_code).unwrap(),
            &mut scope,
            &mut expr_index,
        );
        let mut actual_scope_strs = Vec::new();
        for statement in scope {
            actual_scope_strs.push(format!("{}", statement));
        }

        assert_eq!(actual_scope_strs, expected_scope);
        assert_eq!(format!("{}", actual_expr), expected_expr);
    }

    #[test]
    fn handles_addition() {
        assert_lift(
            "1.0 + 1.0",
            vec!["double _aro_expr_0;", "_aro_expr_0 = (1 + 1);"],
            "_aro_expr_0",
        )
    }
}
