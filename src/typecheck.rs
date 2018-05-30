use ast::{Ast, BinOp, EvaluatedType, Expression, Pattern, Type, TypedAst, TypedExpression,
          TypedPattern, TypedValue, Value};
use util::Error;
use std::collections::HashMap;
use std::iter::Iterator;

impl Error {
    pub fn type_error(
        left_loc: usize,
        right_loc: usize,
        expected: &EvaluatedType,
        actual: &EvaluatedType,
    ) -> Error {
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

fn build_env(
    env: &mut HashMap<String, EvaluatedType>,
    pattern: &Ast<Pattern>,
) -> Result<(), Error> {
    match &*pattern.expr {
        &Pattern::Ident(ref name, ref ident_type) => {
            let ident_type = evaluate_type(ident_type, env)?;
            env.insert(name.clone(), ident_type);
        }
        &Pattern::Tuple(ref vec) => for el in vec {
            build_env(env, el)?;
        },
    };

    Ok(())
}

fn rename_type(ast: &Ast<EvaluatedType>, name: &str, new_name: &str) -> Ast<EvaluatedType> {
    match &*ast.expr {
        &EvaluatedType::Any
        | &EvaluatedType::Bool
        | &EvaluatedType::Empty
        | &EvaluatedType::Int
        | &EvaluatedType::Num => ast.clone(),
        &EvaluatedType::Func(ref input, ref output) => ast.replace_expr(EvaluatedType::Func(
            rename_type(input, name, new_name),
            rename_type(output, name, new_name),
        )),
        &EvaluatedType::GenericFunc {
            ref param_name,
            ref param_supertype,
            ref output,
            ref substituted_output,
        } => {
            if param_name == name {
                ast.clone()
            } else {
                ast.replace_expr(EvaluatedType::GenericFunc {
                    param_name: param_name.clone(),
                    param_supertype: rename_type(param_supertype, name, new_name),
                    output: rename_type(output, name, new_name),
                    substituted_output: substituted_output.clone(),
                })
            }
        }
        &EvaluatedType::Ident(ref ident_name, ref supertype) => if ident_name == name {
            ast.replace_expr(EvaluatedType::Ident(
                String::from(new_name),
                supertype.clone(),
            ))
        } else {
            ast.clone()
        },
        &EvaluatedType::List(ref el_type) => {
            ast.replace_expr(EvaluatedType::List(rename_type(el_type, name, new_name)))
        }
        &EvaluatedType::Tuple(ref vec) => ast.replace_expr(EvaluatedType::Tuple(
            vec.into_iter()
                .map(|element| rename_type(element, name, new_name))
                .collect(),
        )),
        &EvaluatedType::Record(ref map) => ast.replace_expr(EvaluatedType::Record(
            map.into_iter()
                .map(|(name, element)| (name.clone(), rename_type(element, name, new_name)))
                .collect(),
        )),
        &EvaluatedType::Ref(ref value_type) => {
            ast.replace_expr(EvaluatedType::Ref(rename_type(value_type, name, new_name)))
        }
    }
}

impl EvaluatedType {
    // This version isn't terribly smart, but it is guranteed that both input
    // types will be suptypes of the result.
    pub fn union(&self, other: &EvaluatedType) -> EvaluatedType {
        if self.is_sub_type(other) {
            other.clone()
        } else if other.is_sub_type(self) {
            self.clone()
        } else {
            EvaluatedType::Any
        }
    }

    pub fn is_sub_type(&self, other: &EvaluatedType) -> bool {
        match (self, other) {
            (&EvaluatedType::Empty, _)
            | (_, &EvaluatedType::Any)
            | (&EvaluatedType::Bool, &EvaluatedType::Bool)
            | (&EvaluatedType::Int, &EvaluatedType::Int)
            | (&EvaluatedType::Int, &EvaluatedType::Num)
            | (&EvaluatedType::Num, &EvaluatedType::Num) => true,
            (&EvaluatedType::Ident(ref self_name, _), &EvaluatedType::Ident(ref other_name, _)) => {
                self_name == other_name
            }
            (&EvaluatedType::Ident(_, ref self_supertype), _) => {
                self_supertype.expr.is_sub_type(other)
            }
            // Nothing is a subtype of a generic (except empty and the generic
            // itself), because any generic could be empty
            (_, &EvaluatedType::Ident(_, _)) => false,
            (&EvaluatedType::Record(ref self_map), &EvaluatedType::Record(ref other_map)) => {
                other_map.iter().all(|(name, other_entry_type)| {
                    if let Some(self_entry_type) = self_map.get(name) {
                        self_entry_type.is_sub_type(other_entry_type)
                    } else {
                        false
                    }
                })
            }
            (&EvaluatedType::Ref(ref self_type), &EvaluatedType::Ref(ref other_type)) => {
                self_type.is_sub_type(other_type) && other_type.is_sub_type(self_type)
            }
            (
                &EvaluatedType::Func(ref self_in, ref self_out),
                &EvaluatedType::Func(ref other_in, ref other_out),
            ) => self_out.is_sub_type(other_out) && other_in.is_sub_type(self_in),
            (
                &EvaluatedType::GenericFunc {
                    param_name: ref self_name,
                    param_supertype: ref self_super,
                    output: ref self_out,
                    ..
                },
                &EvaluatedType::GenericFunc {
                    param_name: ref other_name,
                    param_supertype: ref other_super,
                    output: ref other_out,
                    ..
                },
            ) => {
                let other_out = &rename_type(other_out, other_name, self_name);
                self_out.is_sub_type(other_out) && other_super.is_sub_type(self_super)
            }
            (&EvaluatedType::Tuple(ref self_vec), &EvaluatedType::Tuple(ref other_vec)) => {
                self_vec.len() == other_vec.len()
                    && Iterator::zip(self_vec.into_iter(), other_vec.into_iter())
                        .all(|(self_el, other_el)| self_el.is_sub_type(other_el))
            }
            (&EvaluatedType::List(ref self_el), &EvaluatedType::List(ref other_el)) => {
                self_el.is_sub_type(other_el)
            }
            _ => false,
        }
    }
}

impl Ast<EvaluatedType> {
    pub fn is_sub_type(&self, other: &Ast<EvaluatedType>) -> bool {
        self.expr.is_sub_type(&other.expr)
    }
}

#[cfg(test)]
mod is_sub_type {
    use super::*;

    fn ast(t: EvaluatedType) -> Ast<EvaluatedType> {
        Ast::<EvaluatedType>::new(0, 0, t)
    }

    #[test]
    fn same_type() {
        // let x: [] <- []
        //   same ^^     ^^ same
        // Empty.is_sub_type(Empty) = true  -- so this typechecks (if [] were allowed)
        assert!(ast(EvaluatedType::Empty).is_sub_type(&ast(EvaluatedType::Empty)));

        assert!(ast(EvaluatedType::Int).is_sub_type(&ast(EvaluatedType::Int)));
        assert!(ast(EvaluatedType::Bool).is_sub_type(&ast(EvaluatedType::Bool)));
        assert!(ast(EvaluatedType::Num).is_sub_type(&ast(EvaluatedType::Num)));
        assert!(ast(EvaluatedType::Any).is_sub_type(&ast(EvaluatedType::Any)));

        assert!(EvaluatedType::Int.is_sub_type(&EvaluatedType::Int));
    }

    #[test]
    fn empty() {
        // An empty list IS A list of integers.  Thus, Empty is subtype of Int
        // let x: [Int..] <- []
        //  super ^^^^^^^     ^^ subtype
        // Empty.is_sub_type(Int) = true  -- so this typechecks
        assert!(ast(EvaluatedType::Empty).is_sub_type(&ast(EvaluatedType::Int)));

        // let x: [] <- [5]
        //    sub ^^     ^^^ supertype
        // Int.is_sub_type(Empty) = false  -- so this does not typecheck
        assert!(!ast(EvaluatedType::Int).is_sub_type(&ast(EvaluatedType::Empty)));
    }

    #[test]
    fn any() {
        // Any behaves opposite to empty
        assert!(ast(EvaluatedType::Int).is_sub_type(&ast(EvaluatedType::Any)));
        assert!(ast(EvaluatedType::Empty).is_sub_type(&ast(EvaluatedType::Any)));
        assert!(!ast(EvaluatedType::Any).is_sub_type(&ast(EvaluatedType::Int)));
        assert!(!ast(EvaluatedType::Any).is_sub_type(&ast(EvaluatedType::Empty)));
    }

    //
    // The general form is:
    // value.is_sub_type(declared) <=> it typechecks
    //

    // It's weirder with functions:
    #[test]
    fn with_functions() {
        // let x: (Int -> [Int..]) <- x: Int -[]-> []
        //  super ^^^^^^^^^^^^^^^^     ^^^^^^^^^^^^^^^ subtype
        // (Int -> []).is_sub_type(Int -> [Int..]) = true  -- so this typechecks
        // because:
        //   value_out.is_sub_type(delcared_out)
        //   <=> [].is_sub_type([Int..])
        //   <=> Empty.is_sub_type(Int)
        assert!(
            ast(EvaluatedType::Func(
                ast(EvaluatedType::Int),
                ast(EvaluatedType::Empty)
            )).is_sub_type(&ast(EvaluatedType::Func(
                ast(EvaluatedType::Int),
                ast(EvaluatedType::Int)
            )))
        );

        // Reverse them, and it's false
        assert!(!ast(EvaluatedType::Func(
            ast(EvaluatedType::Int),
            ast(EvaluatedType::Int)
        )).is_sub_type(&ast(EvaluatedType::Func(
            ast(EvaluatedType::Int),
            ast(EvaluatedType::Empty)
        ))));

        // let x: ([] -> Int) <- x: [Int..] -Int-> 1
        //  super ^^^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^ subtype
        // ([Int..] -> Int).is_sub_type([] -> Int) = true  -- so this typechecks
        // because:
        //   declared_in.is_sub_type(value_in)
        //   <=> [].is_sub_type([Int..])
        //   <=> Empty.is_sub_type(Int)
        assert!(
            ast(EvaluatedType::Func(
                ast(EvaluatedType::Int),
                ast(EvaluatedType::Int)
            )).is_sub_type(&ast(EvaluatedType::Func(
                ast(EvaluatedType::Empty),
                ast(EvaluatedType::Int)
            )))
        );

        // Again, reverse them, and it's false
        assert!(!ast(EvaluatedType::Func(
            ast(EvaluatedType::Empty),
            ast(EvaluatedType::Int)
        )).is_sub_type(&ast(EvaluatedType::Func(
            ast(EvaluatedType::Int),
            ast(EvaluatedType::Int)
        ))));
    }
}

fn typecheck_pattern(
    pattern: &Ast<Pattern>,
    env: &HashMap<String, EvaluatedType>,
) -> Result<TypedAst<TypedPattern>, Error> {
    Ok(match &*pattern.expr {
        &Pattern::Ident(ref name, ref ident_type) => pattern.to_typed(
            TypedPattern::Ident(name.clone()),
            evaluate_type(ident_type, env)?,
        ),
        &Pattern::Tuple(ref vec) => {
            let mut new_types = Vec::new();
            let mut new_subpatterns = Vec::new();
            for element in vec {
                let typed_ast = typecheck_pattern(element, env)?;
                new_types.push(typed_ast.to_type_ast());
                new_subpatterns.push(typed_ast);
            }

            pattern.to_typed(
                TypedPattern::Tuple(new_subpatterns),
                EvaluatedType::Tuple(new_types),
            )
        }
    })
}

fn substitute_evaluated_type(
    ast: &Ast<EvaluatedType>,
    name: &str,
    value: &Ast<EvaluatedType>,
) -> Ast<EvaluatedType> {
    match &*ast.expr {
        &EvaluatedType::Any
        | &EvaluatedType::Bool
        | &EvaluatedType::Empty
        | &EvaluatedType::Int
        | &EvaluatedType::Num => ast.clone(),
        &EvaluatedType::Func(ref input, ref output) => ast.replace_expr(EvaluatedType::Func(
            substitute_evaluated_type(input, name, value),
            substitute_evaluated_type(output, name, value),
        )),
        &EvaluatedType::GenericFunc {
            ref param_name,
            ref param_supertype,
            ref output,
            ref substituted_output,
        } => ast.replace_expr(EvaluatedType::GenericFunc {
            param_name: param_name.clone(),
            param_supertype: substitute_evaluated_type(param_supertype, name, value),
            output: if param_name == name {
                output.clone()
            } else {
                substitute_evaluated_type(output, name, value)
            },
            substituted_output: substitute_evaluated_type(substituted_output, name, value),
        }),
        &EvaluatedType::Ident(ref ident_name, _) => if ident_name == name {
            value.clone()
        } else {
            ast.clone()
        },
        &EvaluatedType::List(ref el_type) => ast.replace_expr(EvaluatedType::List(
            substitute_evaluated_type(el_type, name, value),
        )),
        &EvaluatedType::Tuple(ref vec) => ast.replace_expr(EvaluatedType::Tuple(
            vec.into_iter()
                .map(|element| substitute_evaluated_type(element, name, value))
                .collect(),
        )),
        &EvaluatedType::Record(ref map) => ast.replace_expr(EvaluatedType::Record(
            map.iter()
                .map(|(entry_name, entry_value)| {
                    (
                        entry_name.clone(),
                        substitute_evaluated_type(entry_value, name, value),
                    )
                })
                .collect(),
        )),
        &EvaluatedType::Ref(ref value_type) => ast.replace_expr(EvaluatedType::Ref(
            substitute_evaluated_type(value_type, name, value),
        )),
    }
}

fn substitute_raw_type(ast: &Ast<Type>, name: &str, value: &Ast<Type>) -> Ast<Type> {
    match &*ast.expr {
        &Type::Any | &Type::Bool | &Type::Empty | &Type::Int | &Type::Num => ast.clone(),
        &Type::Func(ref input, ref output) => ast.replace_expr(Type::Func(
            substitute_raw_type(input, name, value),
            substitute_raw_type(output, name, value),
        )),
        &Type::GenericFunc(ref param_name, ref param_supertype, ref output) => {
            ast.replace_expr(Type::GenericFunc(
                param_name.clone(),
                substitute_raw_type(param_supertype, name, value),
                if param_name == name {
                    output.clone()
                } else {
                    substitute_raw_type(output, name, value)
                },
            ))
        }
        &Type::Ident(ref ident_name) => if ident_name == name {
            value.clone()
        } else {
            ast.clone()
        },
        &Type::List(ref el_type) => {
            ast.replace_expr(Type::List(substitute_raw_type(el_type, name, value)))
        }
        &Type::Tuple(ref vec) => ast.replace_expr(Type::Tuple(
            vec.into_iter()
                .map(|element| substitute_raw_type(element, name, value))
                .collect(),
        )),
        &Type::Record(ref map) => ast.replace_expr(Type::Record(
            map.iter()
                .map(|(entry_name, entry_value)| {
                    (
                        entry_name.clone(),
                        substitute_raw_type(entry_value, name, value),
                    )
                })
                .collect(),
        )),
        &Type::Ref(ref value_type) => {
            ast.replace_expr(Type::Ref(substitute_raw_type(value_type, name, value)))
        }
    }
}

fn substitute_pattern(ast: &Ast<Pattern>, name: &str, value: &Ast<Type>) -> Ast<Pattern> {
    match &*ast.expr {
        &Pattern::Ident(ref ident_name, ref ident_type) => ast.replace_expr(Pattern::Ident(
            ident_name.clone(),
            substitute_raw_type(ident_type, name, value),
        )),
        &Pattern::Tuple(ref vec) => ast.replace_expr(Pattern::Tuple(
            vec.into_iter()
                .map(|element| substitute_pattern(element, name, value))
                .collect(),
        )),
    }
}

fn substitute_expr(ast: &Ast<Expression>, name: &str, value: &Ast<Type>) -> Ast<Expression> {
    match &*ast.expr {
        &Expression::GenericCall(ref expr, ref arg) => ast.replace_expr(Expression::GenericCall(
            substitute_expr(expr, name, value),
            substitute_raw_type(arg, name, value),
        )),
        &Expression::BinOp(ref op, ref left, ref right) => ast.replace_expr(Expression::BinOp(
            op.clone(),
            substitute_expr(left, name, value),
            substitute_expr(right, name, value),
        )),
        &Expression::Sequence(ref side_effect, ref result) => {
            ast.replace_expr(Expression::Sequence(
                substitute_expr(side_effect, name, value),
                substitute_expr(result, name, value),
            ))
        }
        &Expression::RecordAccess(ref record, ref field) => ast.replace_expr(
            Expression::RecordAccess(substitute_expr(record, name, value), field.clone()),
        ),
        &Expression::If(ref c, ref t, ref e) => ast.replace_expr(Expression::If(
            substitute_expr(c, name, value),
            substitute_expr(t, name, value),
            substitute_expr(e, name, value),
        )),
        &Expression::Let(ref bind_pattern, ref bind_value, ref body) => {
            ast.replace_expr(Expression::Let(
                substitute_pattern(bind_pattern, name, value),
                substitute_expr(bind_value, name, value),
                substitute_expr(body, name, value),
            ))
        }
        &Expression::TypeLet(ref bind_name, ref bind_value, ref body) => {
            if bind_name == name {
                ast.clone()
            } else {
                ast.replace_expr(Expression::TypeLet(
                    bind_name.clone(),
                    substitute_raw_type(bind_value, name, value),
                    substitute_expr(body, name, value),
                ))
            }
        }
        &Expression::Value(ref ast_value) => ast.replace_expr(Expression::Value(match ast_value {
            &Value::Bool(_)
            | &Value::Int(_)
            | &Value::Num(_)
            | &Value::Hook(_, _)
            | &Value::Ref(_) => ast_value.clone(),
            &Value::Func(ref param_pattern, ref body_type, ref body) => Value::Func(
                substitute_pattern(param_pattern, name, value),
                substitute_raw_type(body_type, name, value),
                substitute_expr(body, name, value),
            ),
            &Value::GenericFunc(ref param_name, ref param_supertype, ref body_type, ref body) => {
                Value::GenericFunc(
                    param_name.clone(),
                    substitute_raw_type(param_supertype, name, value),
                    substitute_raw_type(body_type, name, value),
                    substitute_expr(body, name, value),
                )
            }
            &Value::Tuple(ref vec) => Value::Tuple(
                vec.into_iter()
                    .map(|element| substitute_expr(element, name, value))
                    .collect(),
            ),
            &Value::Record(ref map) => Value::Record(
                map.iter()
                    .map(|(entry_name, entry_value)| {
                        (
                            entry_name.clone(),
                            substitute_expr(entry_value, name, value),
                        )
                    })
                    .collect(),
            ),
            &Value::List(ref vec) => Value::List(
                vec.into_iter()
                    .map(|element| substitute_expr(element, name, value))
                    .collect(),
            ),
        })),
        &Expression::Ident(_) => ast.clone(),
    }
}

fn evaluate_type(
    ast: &Ast<Type>,
    env: &HashMap<String, EvaluatedType>,
) -> Result<EvaluatedType, Error> {
    match &*ast.expr {
        &Type::Any => Ok(EvaluatedType::Any),
        &Type::Empty => Ok(EvaluatedType::Empty),
        &Type::Int => Ok(EvaluatedType::Int),
        &Type::Num => Ok(EvaluatedType::Num),
        &Type::Bool => Ok(EvaluatedType::Bool),
        &Type::Func(ref input, ref output) => Ok(EvaluatedType::Func(
            input.replace_expr(evaluate_type(input, env)?),
            output.replace_expr(evaluate_type(output, env)?),
        )),
        &Type::GenericFunc(ref param_name, ref param_supertype, ref output) => {
            let param_supertype_type = evaluate_type(param_supertype, env)?;
            let env = &mut env.clone();
            env.insert(param_name.clone(), param_supertype_type.clone());
            let output = output.replace_expr(evaluate_type(output, env)?);
            let param_supertype = param_supertype.replace_expr(param_supertype_type);
            let substituted_output =
                substitute_evaluated_type(&output, param_name, &param_supertype);
            Ok(EvaluatedType::GenericFunc {
                param_name: param_name.clone(),
                param_supertype,
                output,
                substituted_output,
            })
        }
        &Type::Ident(ref ident_name) => if let Some(ident_type) = env.get(ident_name) {
            Ok(EvaluatedType::Ident(
                ident_name.clone(),
                ast.replace_expr(ident_type.clone()),
            ))
        } else {
            Err(Error::LRLocated {
                message: format!("Wut `{}`?  Dat ain't no type.", ident_name),
                left_loc: ast.left_loc,
                right_loc: ast.right_loc,
            })
        },
        &Type::List(ref el_type) => Ok(EvaluatedType::List(
            el_type.replace_expr(evaluate_type(el_type, env)?),
        )),
        &Type::Tuple(ref vec) => Ok(EvaluatedType::Tuple({
            let mut new_vec = Vec::new();
            for element in vec {
                new_vec.push(element.replace_expr(evaluate_type(element, env)?));
            }

            new_vec
        })),
        &Type::Record(ref map) => Ok(EvaluatedType::Record({
            let mut new_map = HashMap::new();
            for (name, value_type) in map {
                new_map.insert(
                    name.clone(),
                    value_type.replace_expr(evaluate_type(value_type, env)?),
                );
            }

            new_map
        })),
        &Type::Ref(ref value_type) => Ok(EvaluatedType::Ref(
            value_type.replace_expr(evaluate_type(value_type, env)?),
        )),
    }
}

fn typecheck_numeric_bin_op<TypeGetter>(
    ast: &Ast<Expression>,
    operation: &BinOp,
    left: &Ast<Expression>,
    right: &Ast<Expression>,
    env: &HashMap<String, EvaluatedType>,
    get_type: TypeGetter,
) -> Result<TypedAst<TypedExpression>, Error>
where
    TypeGetter: Fn(&EvaluatedType, &EvaluatedType) -> EvaluatedType,
{
    let typechecked_left = typecheck_ast(left, env)?;
    let typechecked_right = typecheck_ast(right, env)?;
    if !typechecked_left.expr_type.is_sub_type(&EvaluatedType::Num) {
        Err(Error::LRLocated {
            message: format!(
                "Did you think this was python?  You can't do math with `{}`s.",
                typechecked_left.expr_type
            ),
            left_loc: left.left_loc,
            right_loc: left.right_loc,
        })
    } else if !typechecked_right.expr_type.is_sub_type(&EvaluatedType::Num) {
        Err(Error::LRLocated {
            message: format!(
                "Did you think this was python?  You can't do math with `{}`s.",
                typechecked_right.expr_type
            ),
            left_loc: right.left_loc,
            right_loc: right.right_loc,
        })
    } else {
        let result_type = get_type(&typechecked_left.expr_type, &typechecked_right.expr_type);
        Ok(ast.to_typed(
            TypedExpression::BinOp(operation.clone(), typechecked_left, typechecked_right),
            result_type,
        ))
    }
}

pub fn typecheck_ast(
    ast: &Ast<Expression>,
    env: &HashMap<String, EvaluatedType>,
) -> Result<TypedAst<TypedExpression>, Error> {
    let left_loc = ast.left_loc;
    let right_loc = ast.right_loc;

    match &*ast.expr {
        &Expression::Value(ref value) => match value {
            &Value::Hook(ref name, ref hook_type) => Ok(ast.to_typed(
                TypedExpression::Value(TypedValue::Hook(name.clone())),
                evaluate_type(hook_type, env)?,
            )),
            &Value::Int(value) => Ok(ast.to_typed(
                TypedExpression::Value(TypedValue::Int(value)),
                EvaluatedType::Int,
            )),
            &Value::Num(value) => Ok(ast.to_typed(
                TypedExpression::Value(TypedValue::Num(value)),
                EvaluatedType::Num,
            )),
            &Value::Bool(value) => Ok(ast.to_typed(
                TypedExpression::Value(TypedValue::Bool(value)),
                EvaluatedType::Bool,
            )),
            &Value::Ref(_) => panic!("Cannot typecheck raw pointer"),
            &Value::Tuple(ref vec) => {
                let mut type_vec = Vec::new();
                let mut value_vec = Vec::new();

                for item in vec {
                    let typed_ast = typecheck_ast(item, env)?;
                    type_vec.push(typed_ast.to_type_ast());
                    value_vec.push(typed_ast);
                }

                Ok(ast.to_typed(
                    TypedExpression::Value(TypedValue::Tuple(value_vec)),
                    EvaluatedType::Tuple(type_vec),
                ))
            }
            &Value::Record(ref map) => {
                let mut type_map = HashMap::new();
                let mut value_map = HashMap::new();

                for (name, value) in map {
                    let typed_ast = typecheck_ast(value, env)?;
                    type_map.insert(name.clone(), typed_ast.to_type_ast());
                    value_map.insert(name.clone(), typed_ast);
                }

                Ok(ast.to_typed(
                    TypedExpression::Value(TypedValue::Record(value_map)),
                    EvaluatedType::Record(type_map),
                ))
            }
            &Value::List(ref vec) => {
                let mut union_type = EvaluatedType::Empty;
                let mut value_vec = Vec::new();

                for item in vec {
                    let typed_ast = typecheck_ast(item, env)?;
                    union_type = union_type.union(&typed_ast.expr_type);
                    value_vec.push(typed_ast);
                }

                Ok(ast.to_typed(
                    TypedExpression::Value(TypedValue::List(value_vec)),
                    EvaluatedType::List(ast.replace_expr(union_type)),
                ))
            }
            &Value::Func(ref pattern, ref body_type_ast, ref body) => {
                let mut body_env = env.clone();
                build_env(&mut body_env, pattern)?;

                let typechecked_param = typecheck_pattern(pattern, env)?;
                let param_type_ast = typechecked_param.to_type_ast();

                let declared_body_type = evaluate_type(body_type_ast, &body_env)?;
                let declared_body_type_ast = body_type_ast.replace_expr(declared_body_type.clone());

                let typechecked_body = typecheck_ast(body, &body_env)?;
                let actual_body_type_ast = typechecked_body.to_type_ast();

                if !actual_body_type_ast.is_sub_type(&declared_body_type_ast) {
                    Err(Error::type_error(
                        body.left_loc,
                        body.right_loc,
                        &declared_body_type,
                        &actual_body_type_ast.expr,
                    ))
                } else {
                    Ok(ast.to_typed(
                        TypedExpression::Value(TypedValue::Func(
                            typechecked_param,
                            typechecked_body,
                        )),
                        EvaluatedType::Func(param_type_ast, declared_body_type_ast),
                    ))
                }
            }
            &Value::GenericFunc(
                ref param_name,
                ref param_supertype,
                ref body_type_ast,
                ref body,
            ) => {
                let env = &mut env.clone();
                let evaluated_supertype = evaluate_type(param_supertype, env)?;
                env.insert(param_name.clone(), evaluated_supertype.clone());

                let declared_body_type = evaluate_type(body_type_ast, env)?;
                let declared_body_type_ast = body_type_ast.replace_expr(declared_body_type.clone());

                let typechecked_body = typecheck_ast(body, env)?;
                let actual_body_type_ast = typechecked_body.to_type_ast();

                if !actual_body_type_ast.is_sub_type(&declared_body_type_ast) {
                    Err(Error::type_error(
                        body.left_loc,
                        body.right_loc,
                        &declared_body_type,
                        &actual_body_type_ast.expr,
                    ))
                } else {
                    let evaluated_supertype_ast = param_supertype.replace_expr(evaluated_supertype);
                    let substituted_output = substitute_evaluated_type(
                        &declared_body_type_ast,
                        param_name,
                        &evaluated_supertype_ast,
                    );
                    Ok(ast.to_typed(
                        TypedExpression::Cast(
                            typechecked_body.replace_type(
                                *substitute_evaluated_type(
                                    &actual_body_type_ast,
                                    param_name,
                                    &evaluated_supertype_ast,
                                ).expr,
                            ),
                            substituted_output.clone(),
                        ),
                        EvaluatedType::GenericFunc {
                            param_name: param_name.clone(),
                            param_supertype: evaluated_supertype_ast,
                            output: declared_body_type_ast,
                            substituted_output,
                        },
                    ))
                }
            }
        },
        &Expression::Sequence(ref side_effect, ref result) => {
            let typechecked_side_effect = typecheck_ast(side_effect, env)?;
            let typechecked_result = typecheck_ast(result, env)?;
            let result_type = *typechecked_result.expr_type.clone();
            Ok(ast.to_typed(
                TypedExpression::Sequence(typechecked_side_effect, typechecked_result),
                result_type,
            ))
        }
        &Expression::RecordAccess(ref record, ref field) => {
            let typechecked_record = typecheck_ast(record, env)?;
            let record_type = *typechecked_record.expr_type.clone();
            let try_access_record = |object_type| {
                if let &EvaluatedType::Record(ref map) = object_type {
                    if let Some(field_type) = map.get(field) {
                        Ok(ast.to_typed(
                            TypedExpression::RecordAccess(typechecked_record, field.clone()),
                            *field_type.expr.clone(),
                        ))
                    } else {
                        Err(Error::LRLocated {
                            message: format!(
                                "Why would you even try to access a `{}` field of a `{}`?",
                                field, typechecked_record.expr_type
                            ),
                            left_loc: record.left_loc,
                            right_loc: record.right_loc,
                        })
                    }
                } else {
                    Err(Error::LRLocated {
                        message: format!(
                            "Why would you even try to access a field of a `{}`?",
                            typechecked_record.expr_type
                        ),
                        left_loc: record.left_loc,
                        right_loc: record.right_loc,
                    })
                }
            };

            if let EvaluatedType::Ident(_, ref supertype) = record_type {
                try_access_record(&supertype.expr)
            } else {
                try_access_record(&record_type)
            }
        }
        &Expression::GenericCall(ref generic_func, ref arg) => {
            let typechecked_generic_func = typecheck_ast(generic_func, env)?;
            if let EvaluatedType::GenericFunc {
                ref param_name,
                ref param_supertype,
                ref output,
                ref substituted_output,
            } = *typechecked_generic_func.expr_type.clone()
            {
                let arg_type = evaluate_type(arg, env)?;
                if arg_type.is_sub_type(&param_supertype.expr) {
                    let evaluated_arg = arg.replace_expr(arg_type);
                    let return_type = substitute_evaluated_type(output, param_name, &evaluated_arg);
                    Ok(ast.to_typed(
                        TypedExpression::Cast(
                            typechecked_generic_func.replace_type(*substituted_output.expr.clone()),
                            return_type.clone(),
                        ),
                        *return_type.expr,
                    ))
                } else {
                    Err(Error::type_error(
                        generic_func.left_loc,
                        generic_func.right_loc,
                        &param_supertype.expr,
                        &arg_type,
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
            Ok(ast.to_typed(TypedExpression::Ident(name.clone()), ident_type.clone()))
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
            let typechecked_condition = typecheck_ast(condition, env)?;
            if *typechecked_condition.expr_type != EvaluatedType::Bool {
                Err(Error::type_error(
                    alternate.left_loc,
                    alternate.right_loc,
                    &EvaluatedType::Bool,
                    &typechecked_condition.expr_type,
                ))
            } else {
                let typechecked_consequent = typecheck_ast(consequent, env)?;
                let typechecked_alternate = typecheck_ast(alternate, env)?;
                let result_type = typechecked_consequent
                    .expr_type
                    .union(&typechecked_alternate.expr_type);

                Ok(ast.to_typed(
                    TypedExpression::If(
                        typechecked_condition,
                        typechecked_consequent,
                        typechecked_alternate,
                    ),
                    result_type,
                ))
            }
        }
        &Expression::Let(ref pattern, ref value, ref body) => {
            let mut body_env = env.clone();
            build_env(&mut body_env, &pattern)?;

            let typechecked_pattern = typecheck_pattern(pattern, env)?;
            let declared_value_type = typechecked_pattern.to_type_ast();

            let typechecked_value = typecheck_ast(value, &body_env)?;
            let actual_value_type = typechecked_value.to_type_ast();

            if !actual_value_type.is_sub_type(&declared_value_type) {
                Err(Error::type_error(
                    value.left_loc,
                    value.right_loc,
                    &declared_value_type.expr,
                    &actual_value_type.expr,
                ))
            } else {
                let typechecked_body = typecheck_ast(body, &body_env)?;
                let body_type = *typechecked_body.expr_type.clone();
                Ok(ast.to_typed(
                    TypedExpression::Let(typechecked_pattern, typechecked_value, typechecked_body),
                    body_type,
                ))
            }
        }
        &Expression::TypeLet(ref name, ref value, ref body) => {
            typecheck_ast(&substitute_expr(body, name, value), env)
        }
        &Expression::BinOp(ref operation, ref left, ref right) => match operation {
            &BinOp::Call => {
                let typechecked_left = typecheck_ast(left, env)?;
                let typechecked_right = typecheck_ast(right, env)?;

                if let EvaluatedType::Func(ref param_type, ref output_type) =
                    *typechecked_left.expr_type.clone()
                {
                    if !typechecked_right.expr_type.is_sub_type(&param_type.expr) {
                        Err(Error::type_error(
                            right.left_loc,
                            right.right_loc,
                            &param_type.expr,
                            &typechecked_right.expr_type,
                        ))
                    } else {
                        Ok(ast.to_typed(
                            TypedExpression::BinOp(
                                operation.clone(),
                                typechecked_left,
                                typechecked_right,
                            ),
                            *output_type.expr.clone(),
                        ))
                    }
                } else {
                    Err(Error::LRLocated {
                        message: format!(
                            "I only call two things on a regular basis: functions, and your mom.\n\
                             `{}` is not a function.",
                            typechecked_left.expr_type
                        ),
                        left_loc: left.left_loc,
                        right_loc: left.right_loc,
                    })
                }
            }
            &BinOp::LEq => typecheck_numeric_bin_op(ast, operation, left, right, env, |_, _| {
                EvaluatedType::Bool
            }),
            &BinOp::Div => typecheck_numeric_bin_op(ast, operation, left, right, env, |_, _| {
                EvaluatedType::Num
            }),
            &BinOp::Add | &BinOp::Sub | &BinOp::Mul => typecheck_numeric_bin_op(
                ast,
                operation,
                left,
                right,
                env,
                |left_type, right_type| match (left_type, right_type) {
                    (&EvaluatedType::Int, &EvaluatedType::Int) => EvaluatedType::Int,
                    (&EvaluatedType::Int, &EvaluatedType::Ident(ref name, ref supertype))
                    | (&EvaluatedType::Ident(ref name, ref supertype), &EvaluatedType::Int) => {
                        EvaluatedType::Ident(name.clone(), supertype.clone())
                    }
                    (
                        &EvaluatedType::Ident(ref left_name, ref left_supertype),
                        &EvaluatedType::Ident(ref right_name, ref right_supertype),
                    ) => {
                        if left_name == right_name {
                            assert!(left_supertype == right_supertype);
                            left_type.clone()
                        } else if &*left_supertype.expr == &EvaluatedType::Int
                            && &*right_supertype.expr == &EvaluatedType::Int
                        {
                            EvaluatedType::Int
                        } else {
                            EvaluatedType::Num
                        }
                    }
                    _ => EvaluatedType::Num,
                },
            ),
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
                typecheck_ast(&source_to_ast(actual).unwrap(), &HashMap::new())
                    .unwrap()
                    .expr_type
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
        globals.insert(String::from("global_val"), EvaluatedType::Int);

        assert_eq!(
            format!(
                "{}",
                typecheck_ast(&source_to_ast("global_val").unwrap(), &globals)
                    .unwrap()
                    .expr_type
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
    fn checks_sequenced_operations() {
        assert_typecheck_eq("5; 3", "Int");
        assert_typecheck_eq("#true(); 3", "Int");
        assert_typecheck_eq("(); #true()", "Bool");
        assert_typecheck_err("1 + #true(); 5");
    }

    #[test]
    fn checks_a_nested_function_call() {
        assert_typecheck_eq(
            "(inc: (Int => Int) =Int=> inc <| 5) <| (x: Int =Int=> x + 1)",
            "Int",
        );
        assert_typecheck_eq(
            "(inc: (Int => Num) =Num=> inc <| 5) <| (x: Int =Num=> x + 1.0)",
            "Num",
        );
        assert_typecheck_eq(
            "(func: (Bool => Bool) =Bool=> func <| #true()) <| (x: Bool =Bool=> x)",
            "Bool",
        );
        assert_typecheck_err("(func: (Int => Bool) =Int=> func <| 5) <| (x: Int =Int=> x)");
        assert_typecheck_err("(func: (Int => Bool) =Bool=> func <| 5) <| (x: Int =Int=> x)");
        assert_typecheck_err(
            "(func: (Bool => Bool) =Int=> func <| #true()) <| (x: Bool =Bool=> x)",
        );
    }

    #[test]
    fn checks_a_let_expression() {
        assert_typecheck_eq(
            "
            let added_val: Int <- 5
            ((inc: (Int => Int) =Int=> inc <| added_val) <| (x: Int =Int=> x + 1))
            ",
            "Int",
        );
    }

    #[test]
    fn checks_let_subtype_annotations() {
        assert_typecheck_eq(
            "
            let func: (Int => Num) <- x: Any =Int=> 5
            func
            ",
            "(Int => Num)",
        );
    }

    #[test]
    fn checks_nested_let_expression() {
        assert_typecheck_eq(
            "
            let x: Int <- 5
            let y: Int <- x + 1
            y
            ",
            "Int",
        );
    }

    #[test]
    fn checks_list_subtype_assignments() {
        assert_typecheck_eq(
            "
            let x: [Int..] <- []
            x
            ",
            "[Int..]",
        );
        assert_typecheck_eq("[] |> (x: [Int..] =[Int..]=> x)", "[Int..]");
    }

    #[test]
    fn checks_record_subtype_assignments() {
        assert_typecheck_eq(
            "
            let x: {a: Num} <- {a <- 5}
            x
            ",
            "{a: Num}",
        );
        assert_typecheck_eq(
            "
            let x: {a: Int} <- {a <- 5  b <- #false()}
            x
            ",
            "{a: Int}",
        );
        assert_typecheck_err(
            "
            let x: {a: Int} <- {a <- 5.1}
            x
            ",
        );
        assert_typecheck_err(
            "
            let x: {a: Int  b: Bool} <- {a <- 5}
            x
            ",
        );
    }

    #[test]
    fn checks_function_body_subtyping() {
        assert_typecheck_eq("x: Int =[Int..]=> []", "(Int => [Int..])");
        assert_typecheck_eq("x: Int =[Int..]=> [x]", "(Int => [Int..])");
        assert_typecheck_err("x: Int =[Empty..]=> [x]");
    }

    #[test]
    fn checks_tuples_with_identifiers() {
        assert_typecheck_eq(
            "
            let x: Int <- 5
            (x 2.3)
            ",
            "(Int Num)",
        );
    }

    #[test]
    fn checks_destructured_tuples() {
        assert_typecheck_eq(
            "
            let (x: Int y: Num) <- (2 2.3)
            x + y
            ",
            "Num",
        );
        assert_typecheck_eq(
            "
            fn (x: Int  y: Bool  z: Num) =Num=>
                if y then
                    x * 1.0
                else
                    z
            ",
            "((Int Bool Num) => Num)",
        );
        assert_typecheck_eq(
            "
            fn () =Num=> 5.0 + 1
            ",
            "(() => Num)",
        );
    }

    #[test]
    fn unions_list_element_types() {
        assert_typecheck_eq("[1 1.2]", "[Num..]");
        assert_typecheck_eq("[1.2 1]", "[Num..]");
        assert_typecheck_eq("[#true() 1]", "[Any..]");
    }

    #[test]
    fn checks_lists_with_identifiers() {
        assert_typecheck_eq(
            "
            let x: Int <- 5
            [x 2]
            ",
            "[Int..]",
        );
    }

    #[test]
    fn checks_records_with_identifiers() {
        assert_typecheck_eq(
            "
            let x: Int <- 5
            {foo <- x}
            ",
            "{foo: Int}",
        );
    }

    #[test]
    fn supports_recursion_in_the_let_expression() {
        assert_typecheck_eq(
            "
            let factorial: (Int => Int) <- n: Int =Int=>
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
            "(x: Int =(Int => Int)=> y: Int =Int=> x + y) <| 5",
            "(Int => Int)",
        );
    }

    #[test]
    fn doesnt_like_unbound_type_names() {
        assert_typecheck_err("let foo: [Foo..] <- []  foo");
        assert_typecheck_err("x: Foo =Int=> 5");
        assert_typecheck_err("x: Int =[Foo..]=> []");
        assert_typecheck_err("T: Any =(Foo => Int)=> x: Foo =Int=> 5");
        assert_typecheck_err("T: Foo =(Int => Int)=> x: Int =Int=> 5");
    }

    #[test]
    fn checks_type_let() {
        assert_typecheck_eq(
            "
            let I <- Int
            let i: I <- 5
            i
            ",
            "Int",
        );
        assert_typecheck_eq(
            "
            let F <- Int => Int
            let f: F <- x: Int =Int=> x + 1
            f
            ",
            "(Int => Int)",
        );
        assert_typecheck_eq(
            "
            let I <- Int
            let F <- I => I
            let f: F <- x: I =I=> x + 1
            f
            ",
            "(Int => Int)",
        );
        assert_typecheck_eq(
            "
            let I <- Int
            let F <- T: I => T => T
            let f: F <- T: I =(T => T)=> x: T =T=> x
            f
            ",
            "(T: Int => ((T <: Int) => (T <: Int)))",
        );
    }

    #[test]
    fn checks_generic_functions() {
        assert_typecheck_eq(
            "
            T: Int =(T => T => [T..])=>
            x: T =(T => [T..])=>
            y: T =[T..]=>
                [x  y]
            ",
            "(T: Int => ((T <: Int) => ((T <: Int) => [(T <: Int)..])))",
        );
        assert_typecheck_eq(
            "
            T: Any =(T => T => [T..])=>
            x: T =(T => [T..])=>
            y: T =[T..]=>
                [x  y]
            ",
            "(T: Any => ((T <: Any) => ((T <: Any) => [(T <: Any)..])))",
        );
    }

    #[test]
    fn checks_immediately_invoked_generic_functions() {
        assert_typecheck_eq(
            "
            type Int |> (
                T: Int =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]
            )
            ",
            "(Int => (Int => [Int..]))",
        );
        assert_typecheck_eq(
            "
            type Int |> (
                T: Num =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]
            )
            ",
            "(Int => (Int => [Int..]))",
        );
        assert_typecheck_eq(
            "
            type Int |> (
                T: Any =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]
            )
            ",
            "(Int => (Int => [Int..]))",
        );
        assert_typecheck_err(
            "
            type Bool |> (
                T: Int =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]
            )
            ",
        );
        assert_typecheck_err(
            "
            type Num |> (
                T: Int =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]
            )
            ",
        );
    }

    #[test]
    fn supports_generic_function_shadowing() {
        assert_typecheck_eq(
            "
            type Int |> (
                T: Any =(T: Any => T => T => [T..])=>
                T: Any =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]
            )
            ",
            "(T: Any => ((T <: Any) => ((T <: Any) => [(T <: Any)..])))",
        );
    }

    #[test]
    fn checks_generic_function_calls() {
        assert_typecheck_eq(
            "
            let make_two_list: (El: Any => El => El=> [El..]) <-
                T: Any =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]

            make_two_list <| type Bool
            ",
            "(Bool => (Bool => [Bool..]))",
        );
        assert_typecheck_eq(
            "
            let make_two_list: (El: Bool => El => El=> [El..]) <-
                T: Any =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]

            make_two_list <| type Bool
            ",
            "(Bool => (Bool => [Bool..]))",
        );
        assert_typecheck_err(
            "
            let make_two_list: (El: Bool => El => El=> [El..]) <-
                T: Any =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]

            make_two_list <| type Int
            ",
        );
        assert_typecheck_err(
            "
            let make_two_list: (El: Any => El => El=> [El..]) <-
                T: Bool =(T => T => [T..])=>
                x: T =(T => [T..])=>
                y: T =[T..]=>
                    [x  y]

            make_two_list <| type Bool
            ",
        );
    }

    #[test]
    fn checks_generic_subtyping() {
        assert_typecheck_eq(
            "
            let add: (T: Num => T => T => Num) <-
                T: Num =(T => T => Num)=>
                x: T =(T => Num)=>
                y: T =Num=>
                    x + y

            add <| type Num
            ",
            "(Num => (Num => Num))",
        );
        assert_typecheck_err(
            "
            let add: (T: Any => T => T => Num) <-
                T: Any =(T => T => Num)=>
                x: T =(T => Num)=>
                y: T =Num=>
                    x + y

            add <| type Num
            ",
        );
        assert_typecheck_err(
            "
            let add: (T: Num => T => T => T) <-
                T: Num =(T => T => T)=>
                x: T =(T => T)=>
                y: T =T=>
                    x / y

            add <| type Num
            ",
        );
    }

    #[test]
    fn generic_functions_can_return_data_structures() {
        assert_typecheck_eq(
            "
            let math: (T: Num => {
                plus: T => T => T
                minus: T => T => T
            }) <-
                T: Num ={
                    plus: T => T => T
                    minus: T => T => T
                }=> {
                    plus <-
                        x: T =(T => T)=>
                        y: T =T=>
                            x + y
                    minus <-
                        x: T =(T => T)=>
                        y: T =T=>
                            x - y
                }

            let int_math: {
                plus: Int => Int => Int
                minus: Int => Int => Int
            } <-
                math <| type Int

            (2 |> int_math.plus <| 3) |> int_math.minus <| 1
            ",
            "Int",
        );
    }

    #[test]
    fn lets_you_do_things_in_generic_functions() {
        assert_typecheck_eq(
            "
            let plus: (T: Num => T => T => T) <-
                T: Num =(T => T => T)=>
                x: T =(T => T)=>
                y: T =T=>
                    x + y
            2 |> (plus <| type Int) <| 3
            ",
            "Int",
        );
        assert_typecheck_eq(
            "
            let access_record: (T: Any => U: {foo: T} => U => (T U)) <-
                T: Any =(U: {foo: T} => U => (T U))=>
                U: {foo: T} =(U => (T U))=>
                record: U =(T U)=>
                    (record.foo  record)
            access_record <| type Num <| type {foo: Num  bar: Int} <| {
                foo <- 3
                bar <- 5
            }
            ",
            "(Num {bar: Int foo: Num})",
        );
    }
}
