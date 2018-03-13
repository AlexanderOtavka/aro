use ast::{Ast, BinOp, Expression, Pattern, Type, Value};
use util::Error;
use std::collections::HashMap;
use std::iter::Iterator;

impl Error {
    pub fn type_error(left_loc: usize, right_loc: usize, expected: &Type, actual: &Type) -> Error {
        Error::LRLocated {
            message: format!(
                "Did you have trouble matching shapes as a toddler?\n\
                 Looks like you can't match types either.\n\
                 Expected type `{}` but saw `{}`.",
                expected, actual
            ),
            left_loc,
            right_loc,
        }
    }
}

fn build_env(env: &mut HashMap<String, Type>, pattern: &Ast<Pattern>) {
    match &*pattern.expr {
        &Pattern::Ident(ref name, ref ident_type) => {
            env.insert(name.clone(), *ident_type.expr.clone());
        }
        &Pattern::Tuple(ref vec) => for el in vec {
            build_env(env, el);
        },
    };
}

fn rename_type(ast: &Ast<Type>, name: &str, new_name: &str) -> Ast<Type> {
    match &*ast.expr {
        &Type::Func(ref input, ref output) => Ast::<Type>::new(
            ast.left_loc,
            ast.right_loc,
            Type::Func(
                rename_type(input, name, new_name),
                rename_type(output, name, new_name),
            ),
        ),
        &Type::GenericFunc(ref param_name, ref param_supertype, ref output) => {
            if param_name == name {
                ast.clone()
            } else {
                Ast::<Type>::new(
                    ast.left_loc,
                    ast.right_loc,
                    Type::GenericFunc(
                        param_name.clone(),
                        rename_type(param_supertype, name, new_name),
                        rename_type(output, name, new_name),
                    ),
                )
            }
        }
        &Type::Ident(ref ident_name) => if ident_name == name {
            Ast::<Type>::new(
                ast.left_loc,
                ast.right_loc,
                Type::Ident(String::from(new_name)),
            )
        } else {
            ast.clone()
        },
        &Type::List(ref el_type) => Ast::<Type>::new(
            ast.left_loc,
            ast.right_loc,
            Type::List(rename_type(el_type, name, new_name)),
        ),
        &Type::Tuple(ref vec) => Ast::<Type>::new(
            ast.left_loc,
            ast.right_loc,
            Type::Tuple(
                vec.into_iter()
                    .map(|element| rename_type(element, name, new_name))
                    .collect(),
            ),
        ),
        _ => ast.clone(),
    }
}

impl Type {
    pub fn union(&self, other: &Type, env: &HashMap<String, Type>) -> Type {
        if self.is_sub_type(other, env) {
            other.clone()
        } else if other.is_sub_type(self, env) {
            self.clone()
        } else {
            Type::Any
        }
    }

    pub fn is_sub_type(&self, other: &Type, env: &HashMap<String, Type>) -> bool {
        match (self, other) {
            (&Type::Empty, _)
            | (_, &Type::Any)
            | (&Type::Bool, &Type::Bool)
            | (&Type::Int, &Type::Int)
            | (&Type::Int, &Type::Num)
            | (&Type::Num, &Type::Num) => true,
            (&Type::Ident(ref self_name), &Type::Ident(ref other_name)) => self_name == other_name,
            (&Type::Ident(ref self_name), _) => {
                if let Some(self_supertype) = env.get(self_name) {
                    self_supertype.is_sub_type(other, env)
                } else {
                    false
                }
            }
            // Nothing is a subtype of a generic, because any generic could be empty
            (_, &Type::Ident(_)) => false,
            (&Type::Func(ref self_in, ref self_out), &Type::Func(ref other_in, ref other_out)) => {
                self_out.is_sub_type(other_out, env) && other_in.is_sub_type(self_in, env)
            }
            (
                &Type::GenericFunc(ref self_name, ref self_super, ref self_out),
                &Type::GenericFunc(ref other_name, ref other_super, ref other_out),
            ) => {
                let other_out = &rename_type(other_out, other_name, self_name);
                self_out.is_sub_type(other_out, env) && other_super.is_sub_type(self_super, env)
            }
            (&Type::Tuple(ref self_vec), &Type::Tuple(ref other_vec)) => {
                self_vec.len() == other_vec.len()
                    && Iterator::zip(self_vec.into_iter(), other_vec.into_iter())
                        .all(|(self_el, other_el)| self_el.is_sub_type(other_el, env))
            }
            (&Type::List(ref self_el), &Type::List(ref other_el)) => {
                self_el.is_sub_type(other_el, env)
            }
            _ => false,
        }
    }
}

impl Ast<Type> {
    pub fn from_pattern(pattern: &Ast<Pattern>) -> Ast<Type> {
        match &*pattern.expr {
            &Pattern::Ident(_, ref ident_type) => ident_type.clone(),
            &Pattern::Tuple(ref vec) => Ast::<Type>::new(
                pattern.left_loc,
                pattern.right_loc,
                Type::Tuple(vec.into_iter().map(Ast::from_pattern).collect()),
            ),
        }
    }

    pub fn is_sub_type(&self, other: &Ast<Type>, env: &HashMap<String, Type>) -> bool {
        self.expr.is_sub_type(&other.expr, env)
    }
}

#[cfg(test)]
mod is_sub_type {
    use super::*;

    fn ast(t: Type) -> Ast<Type> {
        Ast::<Type>::new(0, 0, t)
    }

    #[test]
    fn same_type() {
        // let x: [] <== []
        //   same ^^     ^^ same
        // Empty.is_sub_type(Empty) = true  -- so this typechecks (if [] were allowed)
        assert!(ast(Type::Empty).is_sub_type(&ast(Type::Empty), &HashMap::new()));

        assert!(ast(Type::Int).is_sub_type(&ast(Type::Int), &HashMap::new()));
        assert!(ast(Type::Bool).is_sub_type(&ast(Type::Bool), &HashMap::new()));
        assert!(ast(Type::Num).is_sub_type(&ast(Type::Num), &HashMap::new()));
        assert!(ast(Type::Any).is_sub_type(&ast(Type::Any), &HashMap::new()));

        assert!(Type::Int.is_sub_type(&Type::Int, &HashMap::new()));
    }

    #[test]
    fn empty() {
        // An empty list IS A list of integers.  Thus, Empty is subtype of Int
        // let x: [Int..] <== []
        //  super ^^^^^^^     ^^ subtype
        // Empty.is_sub_type(Int) = true  -- so this typechecks
        assert!(ast(Type::Empty).is_sub_type(&ast(Type::Int), &HashMap::new()));

        // let x: [] <== [5]
        //    sub ^^     ^^^ supertype
        // Int.is_sub_type(Empty) = false  -- so this does not typecheck
        assert!(!ast(Type::Int).is_sub_type(&ast(Type::Empty), &HashMap::new()));
    }

    #[test]
    fn any() {
        // Any behaves opposite to empty
        assert!(ast(Type::Int).is_sub_type(&ast(Type::Any), &HashMap::new()));
        assert!(ast(Type::Empty).is_sub_type(&ast(Type::Any), &HashMap::new()));
        assert!(!ast(Type::Any).is_sub_type(&ast(Type::Int), &HashMap::new()));
        assert!(!ast(Type::Any).is_sub_type(&ast(Type::Empty), &HashMap::new()));
    }

    //
    // The general form is:
    // value.is_sub_type(declared) <=> it typechecks
    //

    // It's wierder with functions:
    #[test]
    fn with_functions() {
        // let x: (Int -> [Int..]) <== x: Int -[]-> []
        //  super ^^^^^^^^^^^^^^^^     ^^^^^^^^^^^^^^^ subtype
        // (Int -> []).is_sub_type(Int -> [Int..]) = true  -- so this typechecks
        // because:
        //   value_out.is_sub_type(delcared_out)
        //   <=> [].is_sub_type([Int..])
        //   <=> Empty.is_sub_type(Int)
        assert!(
            ast(Type::Func(ast(Type::Int), ast(Type::Empty))).is_sub_type(
                &ast(Type::Func(ast(Type::Int), ast(Type::Int))),
                &HashMap::new()
            )
        );

        // Reverse them, and it's false
        assert!(
            !ast(Type::Func(ast(Type::Int), ast(Type::Int))).is_sub_type(
                &ast(Type::Func(ast(Type::Int), ast(Type::Empty))),
                &HashMap::new()
            )
        );

        // let x: ([] -> Int) <== x: [Int..] -Int-> 1
        //  super ^^^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^ subtype
        // ([Int..] -> Int).is_sub_type([] -> Int) = true  -- so this typechecks
        // because:
        //   declared_in.is_sub_type(value_in)
        //   <=> [].is_sub_type([Int..])
        //   <=> Empty.is_sub_type(Int)
        assert!(ast(Type::Func(ast(Type::Int), ast(Type::Int))).is_sub_type(
            &ast(Type::Func(ast(Type::Empty), ast(Type::Int)),),
            &HashMap::new()
        ));

        // Again, reverse them, and it's false
        assert!(
            !ast(Type::Func(ast(Type::Empty), ast(Type::Int))).is_sub_type(
                &ast(Type::Func(ast(Type::Int), ast(Type::Int))),
                &HashMap::new()
            )
        );
    }
}

fn substitute_type(ast: &Ast<Type>, name: &str, value: &Ast<Type>) -> Ast<Type> {
    match &*ast.expr {
        &Type::Func(ref input, ref output) => Ast::<Type>::new(
            ast.left_loc,
            ast.right_loc,
            Type::Func(
                substitute_type(input, name, value),
                substitute_type(output, name, value),
            ),
        ),
        &Type::GenericFunc(ref param_name, ref param_supertype, ref output) => Ast::<Type>::new(
            ast.left_loc,
            ast.right_loc,
            Type::GenericFunc(
                param_name.clone(),
                substitute_type(param_supertype, name, value),
                substitute_type(output, name, value),
            ),
        ),
        &Type::Ident(ref ident_name) => if ident_name == name {
            value.clone()
        } else {
            ast.clone()
        },
        &Type::List(ref el_type) => Ast::<Type>::new(
            ast.left_loc,
            ast.right_loc,
            Type::List(substitute_type(el_type, name, value)),
        ),
        &Type::Tuple(ref vec) => Ast::<Type>::new(
            ast.left_loc,
            ast.right_loc,
            Type::Tuple(
                vec.into_iter()
                    .map(|element| substitute_type(element, name, value))
                    .collect(),
            ),
        ),
        _ => ast.clone(),
    }
}

fn evaluate_type(ast: &Ast<Type>, env: &HashMap<String, Type>) -> Result<Type, Error> {
    match &*ast.expr {
        &Type::Func(ref input, ref output) => Ok(Type::Func(
            input.replace_expr(evaluate_type(input, env)?),
            output.replace_expr(evaluate_type(output, env)?),
        )),
        &Type::GenericFunc(ref param_name, ref param_supertype, ref output) => {
            let env = &mut env.clone();
            let param_supertype_type = evaluate_type(param_supertype, env)?;
            env.insert(param_name.clone(), param_supertype_type.clone());
            Ok(Type::GenericFunc(
                param_name.clone(),
                param_supertype.replace_expr(param_supertype_type),
                output.replace_expr(evaluate_type(output, env)?),
            ))
        }
        &Type::Ident(ref ident_name) => if env.contains_key(ident_name) {
            Ok(*ast.expr.clone())
        } else {
            Err(Error::LRLocated {
                message: format!("Wut `{}`?  Dat ain't no type.", ident_name),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            })
        },
        &Type::List(ref el_type) => Ok(Type::List(
            el_type.replace_expr(evaluate_type(el_type, env)?),
        )),
        &Type::Tuple(ref vec) => Ok(Type::Tuple({
            let mut new_vec = Vec::new();
            for element in vec {
                new_vec.push(element.replace_expr(evaluate_type(element, env)?));
            }

            new_vec
        })),
        expr => Ok(expr.clone()),
    }
}

pub fn typecheck_ast(ast: &Ast<Expression>, env: &HashMap<String, Type>) -> Result<Type, Error> {
    let left_loc = ast.left_loc;
    let right_loc = ast.right_loc;

    match &*ast.expr {
        &Expression::Value(ref value) => match value {
            &Value::Hook(_, ref hook_type) => Ok(*hook_type.expr.clone()),
            &Value::Int(_) => Ok(Type::Int),
            &Value::Num(_) => Ok(Type::Num),
            &Value::Bool(_) => Ok(Type::Bool),
            &Value::Tuple(ref vec) => Ok(Type::Tuple({
                let mut type_vec = Vec::new();

                for item in vec {
                    type_vec.push(Ast::<Type>::new(
                        item.left_loc,
                        item.right_loc,
                        typecheck_ast(item, env)?,
                    ))
                }

                type_vec
            })),
            &Value::List(ref vec) => Ok(Type::List(Ast::<Type>::new(left_loc, right_loc, {
                if vec.is_empty() {
                    Type::Empty
                } else {
                    let mut inferred_type = typecheck_ast(&vec[0], env)?;

                    for item in &vec[1..] {
                        let next_type = typecheck_ast(item, env)?;

                        if next_type != inferred_type {
                            return Err(Error::type_error(
                                item.left_loc,
                                item.right_loc,
                                &inferred_type,
                                &next_type,
                            ));
                        }

                        inferred_type = next_type;
                    }

                    inferred_type
                }
            }))),
            &Value::Func(ref pattern, ref body_type_ast, ref body) => {
                let mut body_env = env.clone();
                build_env(&mut body_env, pattern);

                let param_type_ast = Ast::<Type>::from_pattern(pattern);
                let declared_param_type = evaluate_type(&param_type_ast, env)?;
                let declared_body_type = evaluate_type(body_type_ast, env)?;
                let actual_body_type = typecheck_ast(body, &body_env)?;
                if !actual_body_type.is_sub_type(&declared_body_type, env) {
                    Err(Error::type_error(
                        body.left_loc,
                        body.right_loc,
                        &declared_body_type,
                        &actual_body_type,
                    ))
                } else {
                    Ok(Type::Func(
                        param_type_ast.replace_expr(declared_param_type),
                        body_type_ast.clone(),
                    ))
                }
            }
            &Value::GenericFunc(ref type_name, ref supertype, ref body_type_ast, ref body) => {
                let env = &mut env.clone();
                let declared_supertype = evaluate_type(supertype, env)?;
                env.insert(type_name.clone(), declared_supertype.clone());

                let declared_body_type = evaluate_type(body_type_ast, env)?;
                let actual_body_type = typecheck_ast(body, env)?;
                if !actual_body_type.is_sub_type(&declared_body_type, env) {
                    Err(Error::type_error(
                        body.left_loc,
                        body.right_loc,
                        &declared_body_type,
                        &actual_body_type,
                    ))
                } else {
                    Ok(Type::GenericFunc(
                        type_name.clone(),
                        supertype.replace_expr(declared_supertype),
                        body_type_ast.replace_expr(declared_body_type),
                    ))
                }
            }
        },
        &Expression::GenericCall(ref generic_func, ref arg) => {
            if let Type::GenericFunc(ref param, ref supertype, ref body) =
                typecheck_ast(generic_func, env)?
            {
                if arg.is_sub_type(supertype, env) {
                    Ok(*substitute_type(body, param, arg).expr)
                } else {
                    Err(Error::type_error(
                        generic_func.left_loc,
                        generic_func.right_loc,
                        &*supertype.expr,
                        &*arg.expr,
                    ))
                }
            } else {
                Err(Error::LRLocated {
                    message: String::from(
                        "That has to be a generic function if you wanna call it with a type.",
                    ),
                    left_loc: generic_func.left_loc,
                    right_loc: generic_func.right_loc,
                })
            }
        }
        &Expression::Ident(ref name) => if let Some(ident_type) = env.get(name) {
            Ok(ident_type.clone())
        } else {
            Err(Error::LRLocated {
                message: format!(
                    "Am I supposed to read your god damn mind?  What's a `{}`?",
                    name
                ),
                left_loc,
                right_loc,
            })
        },
        &Expression::If(ref condition, ref consequent, ref alternate) => {
            let condition_type = typecheck_ast(condition, env)?;
            if condition_type != Type::Bool {
                Err(Error::type_error(
                    alternate.left_loc,
                    alternate.right_loc,
                    &Type::Bool,
                    &condition_type,
                ))
            } else {
                let consequent_type = typecheck_ast(consequent, env)?;
                let alternate_type = typecheck_ast(alternate, env)?;

                Ok(consequent_type.union(&alternate_type, env))
            }
        }
        &Expression::Let(ref pattern, ref value, ref body) => {
            let mut body_env = env.clone();
            build_env(&mut body_env, &pattern);

            let declared_value_type = evaluate_type(&Ast::<Type>::from_pattern(pattern), env)?;
            let actual_value_type = typecheck_ast(value, &body_env)?;

            if !actual_value_type.is_sub_type(&declared_value_type, env) {
                Err(Error::type_error(
                    value.left_loc,
                    value.right_loc,
                    &declared_value_type,
                    &actual_value_type,
                ))
            } else {
                typecheck_ast(body, &body_env)
            }
        }
        &Expression::BinOp(ref operation, ref left, ref right) => match operation {
            &BinOp::Call => {
                let left_type = typecheck_ast(left, env)?;
                let right_type = typecheck_ast(right, env)?;

                if let Type::Func(ref param_type, ref output_type) = left_type {
                    if !right_type.is_sub_type(&param_type.expr, env) {
                        Err(Error::type_error(
                            right.left_loc,
                            right.right_loc,
                            &*param_type.expr,
                            &right_type,
                        ))
                    } else {
                        Ok(*output_type.expr.clone())
                    }
                } else {
                    Err(Error::LRLocated {
                        message: String::from(
                            "I only call two things on a regular basis: functions, and your mom.\n\
                             That's not a function.",
                        ),
                        left_loc: left.left_loc,
                        right_loc: left.right_loc,
                    })
                }
            }
            &BinOp::Add | &BinOp::Sub | &BinOp::Mul | &BinOp::Div | &BinOp::LEq => {
                let left_type = typecheck_ast(left, env)?;
                let right_type = typecheck_ast(right, env)?;

                if !left_type.is_sub_type(&Type::Int, env)
                    && !left_type.is_sub_type(&Type::Num, env)
                {
                    Err(Error::LRLocated {
                        message: format!(
                            "Did you think this was python?  You can't do math with `{}`s.",
                            left_type
                        ),
                        left_loc: left.left_loc,
                        right_loc: left.right_loc,
                    })
                } else if !right_type.is_sub_type(&Type::Int, env)
                    && !right_type.is_sub_type(&Type::Num, env)
                {
                    Err(Error::LRLocated {
                        message: format!(
                            "Did you think this was python?  You can't do math with `{}`s.",
                            right_type
                        ),
                        left_loc: right.left_loc,
                        right_loc: right.right_loc,
                    })
                } else {
                    match operation {
                        &BinOp::LEq => Ok(Type::Bool),
                        &BinOp::Div => Ok(Type::Num),
                        &BinOp::Add | &BinOp::Sub | &BinOp::Mul => {
                            if (left_type, right_type) == (Type::Int, Type::Int) {
                                Ok(Type::Int)
                            } else {
                                Ok(Type::Num)
                            }
                        }
                        _ => panic!("Cannot typecheck operation"),
                    }
                }
            }
        },
    }
}

#[cfg(test)]
mod typecheck_ast {
    use super::*;
    use parse::source_to_ast;

    fn assert_typecheck_eq(actual: &str, expected: &str) {
        assert_eq!(
            format!(
                "{}",
                typecheck_ast(&source_to_ast(actual).unwrap(), &HashMap::new()).unwrap()
            ),
            expected
        );
    }

    fn assert_typecheck_err(source: &str) {
        assert!(typecheck_ast(&source_to_ast(source).unwrap(), &HashMap::new()).is_err());
    }

    #[test]
    fn checks_an_int_literal() {
        assert_typecheck_eq("5", "Int");
    }

    #[test]
    fn checks_a_bool_literal() {
        assert_typecheck_eq("#true ()", "Bool");
        assert_typecheck_eq("#false ()", "Bool");
    }

    #[test]
    fn doesnt_like_unknown_identifiers() {
        assert_typecheck_err("foo");
    }

    #[test]
    fn checks_global_identifiers() {
        let mut globals = HashMap::new();
        globals.insert(String::from("global_val"), Type::Int);

        assert_eq!(
            format!(
                "{}",
                typecheck_ast(&source_to_ast("global_val").unwrap(), &globals).unwrap()
            ),
            "Int"
        );
    }

    #[test]
    fn checks_arithmatic() {
        assert_typecheck_eq("1 + 2", "Int");
        assert_typecheck_eq("1 - 2", "Int");
        assert_typecheck_eq("1 * 2", "Int");
        assert_typecheck_eq("1 / 2", "Num");
        assert_typecheck_eq("1.0 + 2.0", "Num");
        assert_typecheck_eq("-1.0 - -2", "Num");
        assert_typecheck_eq("1.0 * 2.0", "Num");
        assert_typecheck_eq("-1.0 / -2", "Num");
        assert_typecheck_err("1.0 + #true()");
        assert_typecheck_err("#true() + #true()");
    }

    #[test]
    fn checks_nested_arithmatic() {
        assert_typecheck_eq("1 + (2 * 3)", "Int");
        assert_typecheck_err("1 + (#false() * 3)");
        assert_typecheck_err("#true() + (#false() * #true())");
    }

    #[test]
    fn checks_leq() {
        assert_typecheck_eq("3 <= 2", "Bool");
        assert_typecheck_eq("3.1 <= 2", "Bool");
        assert_typecheck_eq("3.1 <= 2.3", "Bool");
        assert_typecheck_eq("3 <= 2.3", "Bool");
        assert_typecheck_err("#false() <= #true()");
        assert_typecheck_err("#false() <= 3");
    }

    #[test]
    fn checks_an_if() {
        assert_typecheck_eq("if 1 <= 1 then 1 else 2", "Int");
        assert_typecheck_eq("if #false() then 1.1 else 2.0", "Num");
        assert_typecheck_eq("if #false() then 1.1 else 2", "Num");
        assert_typecheck_eq("if #false() then 1 else 2.0", "Num");
        assert_typecheck_eq("if #false() then 2.1 else []", "Any");
        assert_typecheck_err("if 1 then 2 else 3");
    }

    #[test]
    fn checks_a_nested_function_call() {
        assert_typecheck_eq(
            "(inc: (Int -> Int) -Int-> inc <| 5) <| (x: Int -Int-> x + 1)",
            "Int",
        );
        assert_typecheck_eq(
            "(inc: (Int -> Num) -Num-> inc <| 5) <| (x: Int -Num-> x + 1.0)",
            "Num",
        );
        assert_typecheck_eq(
            "(func: (Bool -> Bool) -Bool-> func <| #true()) <| (x: Bool -Bool-> x)",
            "Bool",
        );
        assert_typecheck_err("(func: (Int -> Bool) -Int-> func <| 5) <| (x: Int -Int-> x)");
        assert_typecheck_err("(func: (Int -> Bool) -Bool-> func <| 5) <| (x: Int -Int-> x)");
        assert_typecheck_err(
            "(func: (Bool -> Bool) -Int-> func <| #true()) <| (x: Bool -Bool-> x)",
        );
    }

    #[test]
    fn checks_a_let_expression() {
        assert_typecheck_eq(
            "
            let added_val: Int <== 5
            ((inc: (Int -> Int) -Int-> inc <| added_val) <| (x: Int -Int-> x + 1))
            ",
            "Int",
        );
    }

    #[test]
    fn checks_nested_let_expression() {
        assert_typecheck_eq(
            "
            let x: Int <== 5
            let y: Int <== x + 1
            y
            ",
            "Int",
        );
    }

    #[test]
    fn checks_list_subtype_assignments() {
        assert_typecheck_eq(
            "
            let x: [Int..] <== []
            x
            ",
            "[Int..]",
        );
        assert_typecheck_eq("[] |> (x: [Int..] -[Int..]-> x)", "[Int..]");
    }

    #[test]
    fn checks_function_body_subtyping() {
        assert_typecheck_eq("x: Int -[Int..]-> []", "(Int -> [Int..])");
        assert_typecheck_eq("x: Int -[Int..]-> [x]", "(Int -> [Int..])");
        assert_typecheck_err("x: Int -[Empty..]-> [x]");
    }

    #[test]
    fn checks_tuples_with_identifiers() {
        assert_typecheck_eq(
            "
            let x: Int <== 5
            let tup: (Int Num) <== (x 2.3)
            tup
            ",
            "(Int Num)",
        );
    }

    #[test]
    fn checks_destructured_tuples() {
        assert_typecheck_eq(
            "
            let (x: Int y: Num) <== (2 2.3)
            x + y
            ",
            "Num",
        );
        assert_typecheck_eq(
            "
            fn (x: Int  y: Bool  z: Num) -Num->
                if y then
                    x * 1.0
                else
                    z
            ",
            "((Int Bool Num) -> Num)",
        );
        assert_typecheck_eq(
            "
            fn () -Num-> 5.0 + 1
            ",
            "(() -> Num)",
        );
    }

    #[test]
    fn supports_recursion_in_the_let_expression() {
        assert_typecheck_eq(
            "
            let factorial: (Int -> Int) <== n: Int -Int->
                if n <= 0 then
                    1
                else
                    n * (factorial <| n - 1)
            factorial <| 5
            ",
            "Int",
        );
    }

    #[test]
    fn checks_iifes() {
        assert_typecheck_eq(
            "(x: Int -(Int -> Int)-> y: Int -Int-> x + y) <| 5",
            "(Int -> Int)",
        );
    }

    #[test]
    fn doesnt_like_unbound_type_names() {
        assert_typecheck_err("let foo: [Foo..] <== []  foo");
        assert_typecheck_err("x: Foo -Int-> 5");
        assert_typecheck_err("x: Int -[Foo..]-> []");
        assert_typecheck_err("T: Any -(Foo -> Int)-> x: Foo -Int-> 5");
        assert_typecheck_err("T: Foo -(Int -> Int)-> x: Int -Int-> 5");
    }

    #[test]
    fn checks_generic_functions() {
        assert_typecheck_eq(
            "
            T: Int -(T -> T -> [T..])->
            x: T -(T -> [T..])->
            y: T -[T..]->
                [x  y]
            ",
            "(T: Int -> ((T) -> ((T) -> [(T)..])))",
        );
        assert_typecheck_eq(
            "
            T: Any -(T -> T -> [T..])->
            x: T -(T -> [T..])->
            y: T -[T..]->
                [x  y]
            ",
            "(T: Any -> ((T) -> ((T) -> [(T)..])))",
        );
    }

    #[test]
    fn checks_immediately_invoked_generic_functions() {
        assert_typecheck_eq(
            "
            type Int |> (
                T: Int -(T -> T -> [T..])->
                x: T -(T -> [T..])->
                y: T -[T..]->
                    [x  y]
            )
            ",
            "(Int -> (Int -> [Int..]))",
        );
        assert_typecheck_eq(
            "
            type Int |> (
                T: Any -(T -> T -> [T..])->
                x: T -(T -> [T..])->
                y: T -[T..]->
                    [x  y]
            )
            ",
            "(Int -> (Int -> [Int..]))",
        );
        assert_typecheck_err(
            "
            type Bool |> (
                T: Int -(T -> T -> [T..])->
                x: T -(T -> [T..])->
                y: T -[T..]->
                    [x  y]
            )
            ",
        );
        assert_typecheck_err(
            "
            type Num |> (
                T: Int -(T -> T -> [T..])->
                x: T -(T -> [T..])->
                y: T -[T..]->
                    [x  y]
            )
            ",
        );
    }

    #[test]
    fn checks_generic_function_calls() {
        assert_typecheck_eq(
            "
            let make_two_list: (El: Any -> El -> El-> [El..]) <==
                T: Any -(T -> T -> [T..])->
                x: T -(T -> [T..])->
                y: T -[T..]->
                    [x  y]

            make_two_list <| type Bool
            ",
            "(Bool -> (Bool -> [Bool..]))",
        );
        assert_typecheck_eq(
            "
            let make_two_list: (El: Bool -> El -> El-> [El..]) <==
                T: Any -(T -> T -> [T..])->
                x: T -(T -> [T..])->
                y: T -[T..]->
                    [x  y]

            make_two_list <| type Bool
            ",
            "(Bool -> (Bool -> [Bool..]))",
        );
        assert_typecheck_err(
            "
            let make_two_list: (El: Bool -> El -> El-> [El..]) <==
                T: Any -(T -> T -> [T..])->
                x: T -(T -> [T..])->
                y: T -[T..]->
                    [x  y]

            make_two_list <| type Int
            ",
        );
        assert_typecheck_err(
            "
            let make_two_list: (El: Any -> El -> El-> [El..]) <==
                T: Bool -(T -> T -> [T..])->
                x: T -(T -> [T..])->
                y: T -[T..]->
                    [x  y]

            make_two_list <| type Bool
            ",
        );
    }

    #[test]
    fn checks_generic_subtyping() {
        assert_typecheck_eq(
            "
            let add: (T: Num -> T -> T -> Num) <==
                T: Num -(T -> T -> Num)->
                x: T -(T -> Num)->
                y: T -Num->
                    x + y

            add <| type Num
            ",
            "(Num -> (Num -> Num))",
        );
        assert_typecheck_err(
            "
            let add: (T: Any -> T -> T -> Num) <==
                T: Any -(T -> T -> Num)->
                x: T -(T -> Num)->
                y: T -Num->
                    x + y

            add <| type Num
            ",
        );
        assert_typecheck_err(
            "
            let add: (T: Num -> T -> T -> T) <==
                T: Num -(T -> T -> T)->
                x: T -(T -> T)->
                y: T -T->
                    x + y

            add <| type Num
            ",
        );
    }
}
