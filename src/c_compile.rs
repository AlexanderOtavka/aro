use ast::{Ast, CExpr, CStatement, CType, CValue, Expression, Pattern, Type, Value};

fn get_expr_name(expr_index: &mut i32) -> String {
    let old_i = *expr_index;
    *expr_index += 1;
    format!("_aro_expr_{}", old_i)
}

fn type_to_ctype(t: &Type) -> CType {
    match t {
        &Type::Num => CType::Float,
        &Type::Int => CType::Int,
        &Type::Bool => CType::Bool,
        _ => panic!("Unhandled type: {}", t),
    }
}

fn get_expr_ctype(ast: &Ast<Expression>) -> CType {
    match &*ast.expr_type.borrow() {
        &Some(ref expr_type) => type_to_ctype(expr_type),
        &None => panic!("Ast must be typechecked before compilation"),
    }
}

fn get_ident_name(name: &str) -> String {
    format!("aro_{}", name)
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
        &Expression::Ident(ref name) => ast.replace_expr(CExpr::Ident(get_ident_name(name))),
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
        &Expression::Let(ref pattern_ast, ref value, ref body) => match &*pattern_ast.expr {
            &Pattern::Ident(ref name, ref value_type) => {
                let body_name = get_expr_name(expr_index);
                let body_type = get_expr_ctype(body);
                scope.push(body.replace_expr(CStatement::VarDecl(body_type, body_name.clone())));

                let value_name = get_ident_name(name);
                let value_ctype = type_to_ctype(&*value_type.expr);

                let mut body_scope = Vec::new();
                body_scope.push(
                    pattern_ast.replace_expr(CStatement::VarDecl(value_ctype, value_name.clone())),
                );
                let value_c_ast = lift_expr(value, &mut body_scope, expr_index);
                body_scope
                    .push(pattern_ast.replace_expr(CStatement::VarAssign(value_name, value_c_ast)));

                let body_c_ast = lift_expr(body, &mut body_scope, expr_index);
                body_scope
                    .push(body.replace_expr(CStatement::VarAssign(body_name.clone(), body_c_ast)));

                scope.push(ast.replace_expr(CStatement::Block(body_scope)));

                ast.replace_expr(CExpr::Ident(body_name))
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
    use typecheck::typecheck_ast;
    use std::collections::HashMap;

    fn assert_lift(aro_code: &str, expected_scope: &str, expected_expr: &str) {
        let ast = source_to_ast(aro_code).unwrap();
        typecheck_ast(&ast, &HashMap::new()).unwrap();

        let mut scope = Vec::new();
        let mut expr_index = 0;
        let actual_expr = lift_expr(&ast, &mut scope, &mut expr_index);
        let mut actual_scope = String::new();
        for statement in scope {
            actual_scope += &format!("{} ", statement);
        }

        assert_eq!(actual_scope, expected_scope);
        assert_eq!(format!("{}", actual_expr), expected_expr);
    }

    #[test]
    fn handles_arithmatic() {
        assert_lift(
            "1 + 1",
            "int _aro_expr_0; \
             _aro_expr_0 = (1 + 1); ",
            "_aro_expr_0",
        );
        assert_lift(
            "1.0 + 1.0",
            "double _aro_expr_0; \
             _aro_expr_0 = (1 + 1); ",
            "_aro_expr_0",
        );
        assert_lift(
            "1.1 - 1",
            "double _aro_expr_0; \
             _aro_expr_0 = (1.1 - 1); ",
            "_aro_expr_0",
        );
        assert_lift(
            "1 * 1.1",
            "double _aro_expr_0; \
             _aro_expr_0 = (1 * 1.1); ",
            "_aro_expr_0",
        );
        assert_lift(
            "1.3 / 1.1",
            "double _aro_expr_0; \
             _aro_expr_0 = ((double) 1.3 / 1.1); ",
            "_aro_expr_0",
        );
        assert_lift(
            "1 / 1",
            "double _aro_expr_0; \
             _aro_expr_0 = ((double) 1 / 1); ",
            "_aro_expr_0",
        );
    }

    #[test]
    fn handles_comparison() {
        assert_lift(
            "1.0 <= 1.0",
            "bool _aro_expr_0; \
             _aro_expr_0 = (1 <= 1); ",
            "_aro_expr_0",
        );
    }

    #[test]
    fn supports_nested_operations() {
        assert_lift(
            "1 - 1 <= 1.3 + 8 * 3",
            "int _aro_expr_0; \
             _aro_expr_0 = (1 - 1); \
             int _aro_expr_1; \
             _aro_expr_1 = (8 * 3); \
             double _aro_expr_2; \
             _aro_expr_2 = (1.3 + _aro_expr_1); \
             bool _aro_expr_3; \
             _aro_expr_3 = (_aro_expr_0 <= _aro_expr_2); ",
            "_aro_expr_3",
        );
    }

    #[test]
    fn handles_if_expressions() {
        assert_lift(
            "
            if 1 <= 2 then
                5.3
            else
                8 * 2
            ",
            "bool _aro_expr_0; \
             _aro_expr_0 = (1 <= 2); \
             double _aro_expr_1; \
             if (_aro_expr_0) { \
             _aro_expr_1 = 5.3; \
             } else { \
             int _aro_expr_2; \
             _aro_expr_2 = (8 * 2); \
             _aro_expr_1 = _aro_expr_2; \
             } ",
            "_aro_expr_1",
        );
    }

    #[test]
    fn handles_let_expressions() {
        assert_lift(
            "
            let x: Num <- 5
            x + 3
            ",
            "double _aro_expr_0; \
             { \
             double aro_x; \
             aro_x = 5; \
             double _aro_expr_1; \
             _aro_expr_1 = (aro_x + 3); \
             _aro_expr_0 = _aro_expr_1; \
             } ",
            "_aro_expr_0",
        );
    }

    #[test]
    fn handles_let_shadowing() {
        assert_lift(
            "
            let x: Num <- 5
            let x: Int <- 4
            x + 3
            ",
            "int _aro_expr_0; \
             { \
             double aro_x; \
             aro_x = 5; \
             int _aro_expr_1; \
             { \
             int aro_x; \
             aro_x = 4; \
             int _aro_expr_2; \
             _aro_expr_2 = (aro_x + 3); \
             _aro_expr_1 = _aro_expr_2; \
             } \
             _aro_expr_0 = _aro_expr_1; \
             } ",
            "_aro_expr_0",
        );
    }
}
