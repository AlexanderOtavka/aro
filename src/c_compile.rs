use ast::{Ast, CExpr, CStatement, CType, CValue, Expression, Type, Value};
use typecheck::typecheck_ast;
use std::collections::HashMap;

fn get_expr_name(expr_index: &mut i32) -> String {
    let old_i = *expr_index;
    *expr_index += 1;
    format!("_aro_expr_{}", old_i)
}

fn get_expr_ctype(ast: &Ast<Expression>) -> CType {
    match typecheck_ast(ast, &HashMap::new()).unwrap() {
        Type::Num => CType::Float,
        Type::Int => CType::Int,
        Type::Bool => CType::Bool,
        _ => panic!(),
    }
}

pub fn lift_expr(
    ast: &Ast<Expression>,
    scope: &mut Vec<Ast<CStatement>>,
    expr_index: &mut i32,
) -> Ast<CExpr> {
    match &*ast.expr {
        &Expression::Value(ref v) => match v {
            &Value::Num(value) => ast.replace_expr(CExpr::Value(CValue::Float(value))),
            &Value::Bool(value) => ast.replace_expr(CExpr::Value(CValue::Bool(value))),
            &Value::Int(value) => ast.replace_expr(CExpr::Value(CValue::Int(value))),
            _ => panic!(),
        },
        &Expression::BinOp(ref op, ref left, ref right) => {
            let left_c_ast = lift_expr(left, scope, expr_index);
            let right_c_ast = lift_expr(right, scope, expr_index);

            let expr_name = get_expr_name(expr_index);
            let expr_type = get_expr_ctype(ast);
            scope.push(ast.replace_expr(CStatement::VarDecl(expr_type, expr_name.clone())));
            scope.push(ast.replace_expr(CStatement::VarAssign(
                expr_name.clone(),
                ast.replace_expr(CExpr::BinOp(op.clone(), left_c_ast, right_c_ast)),
            )));

            ast.replace_expr(CExpr::Ident(expr_name))
        }
        &Expression::If(ref condition, ref consequent, ref alternate) => {
            let condition_c_ast = lift_expr(condition, scope, expr_index);

            let expr_name = get_expr_name(expr_index);
            let expr_type = get_expr_ctype(ast);

            let mut consequent_scope = Vec::new();
            let consequent_c_ast = lift_expr(consequent, &mut consequent_scope, expr_index);
            consequent_scope
                .push(ast.replace_expr(CStatement::VarAssign(expr_name.clone(), consequent_c_ast)));
            let consequent_block = consequent.replace_expr(CStatement::Block(consequent_scope));

            let mut alternate_scope = Vec::new();
            let alternate_c_ast = lift_expr(alternate, &mut alternate_scope, expr_index);
            alternate_scope
                .push(ast.replace_expr(CStatement::VarAssign(expr_name.clone(), alternate_c_ast)));
            let alternate_block = alternate.replace_expr(CStatement::Block(alternate_scope));

            scope.push(ast.replace_expr(CStatement::VarDecl(expr_type, expr_name.clone())));
            scope.push(ast.replace_expr(CStatement::If(
                condition_c_ast,
                consequent_block,
                alternate_block,
            )));

            ast.replace_expr(CExpr::Ident(expr_name))
        }
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
    fn handles_arithmatic() {
        assert_lift(
            "1 + 1",
            vec!["int _aro_expr_0;", "_aro_expr_0 = (1 + 1);"],
            "_aro_expr_0",
        );
        assert_lift(
            "1.0 + 1.0",
            vec!["double _aro_expr_0;", "_aro_expr_0 = (1 + 1);"],
            "_aro_expr_0",
        );
        assert_lift(
            "1.1 - 1",
            vec!["double _aro_expr_0;", "_aro_expr_0 = (1.1 - 1);"],
            "_aro_expr_0",
        );
        assert_lift(
            "1 * 1.1",
            vec!["double _aro_expr_0;", "_aro_expr_0 = (1 * 1.1);"],
            "_aro_expr_0",
        );
        assert_lift(
            "1.3 / 1.1",
            vec!["double _aro_expr_0;", "_aro_expr_0 = ((double) 1.3 / 1.1);"],
            "_aro_expr_0",
        );
        assert_lift(
            "1 / 1",
            vec!["double _aro_expr_0;", "_aro_expr_0 = ((double) 1 / 1);"],
            "_aro_expr_0",
        );
    }

    #[test]
    fn handles_comparison() {
        assert_lift(
            "1.0 <= 1.0",
            vec!["bool _aro_expr_0;", "_aro_expr_0 = (1 <= 1);"],
            "_aro_expr_0",
        );
    }

    #[test]
    fn supports_nested_operations() {
        assert_lift(
            "1 - 1 <= 1.3 + 8 * 3",
            vec![
                "int _aro_expr_0;",
                "_aro_expr_0 = (1 - 1);",
                "int _aro_expr_1;",
                "_aro_expr_1 = (8 * 3);",
                "double _aro_expr_2;",
                "_aro_expr_2 = (1.3 + _aro_expr_1);",
                "bool _aro_expr_3;",
                "_aro_expr_3 = (_aro_expr_0 <= _aro_expr_2);",
            ],
            "_aro_expr_3",
        );
    }

    #[test]
    fn handles_if_statements() {
        assert_lift(
            "
            if 1 <= 2 then
                5.3
            else
                8 * 2
            ",
            vec![
                "bool _aro_expr_0;",
                "_aro_expr_0 = (1 <= 2);",
                "double _aro_expr_1;",
                "if (_aro_expr_0) { _aro_expr_1 = 5.3; } else { int _aro_expr_2; _aro_expr_2 = (8 * 2); _aro_expr_1 = _aro_expr_2; }",
            ],
            "_aro_expr_1",
        );
    }
}
