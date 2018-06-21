use c_ast::{CDeclaration, CExpr, CFunc, CFuncName, CName, CStatement, CType, CValue, WellCTyped};
use std::collections::HashMap;
use std::u32;
use untyped_ast::{Ast, BinOp, NumOp};
use wasm_ast::{
    StackMap, WASMDirectFuncName, WASMExpr, WASMFunc, WASMGlobalName, WASMType, WASMValue,
};

const UNIVERSAL_ALIGN: usize = 8; // The size of a union
const STACK_FRAME_HEADER_SIZE: usize = 2 * UNIVERSAL_ALIGN;
const FRAME_POINTER_OFFSET: usize = 0 * UNIVERSAL_ALIGN;
const FUNCTION_INDEX_OFFSET: usize = 1 * UNIVERSAL_ALIGN;
const NULL: u32 = u32::MAX;

pub struct Locals {
    map: HashMap<CName, (i64, WASMType)>,
    current_offset: i64,
}

impl Locals {
    pub fn new() -> Locals {
        Locals {
            map: HashMap::new(),
            current_offset: STACK_FRAME_HEADER_SIZE as i64,
        }
    }

    pub fn init_stack_frame(
        &self,
        left_loc: usize,
        right_loc: usize,
        function_index: Option<i64>,
    ) -> Ast<WASMExpr> {
        Ast::new(
            left_loc,
            right_loc,
            WASMExpr::Sequence(vec![
                // Save the current call stack pointer
                Ast::new(
                    left_loc,
                    right_loc,
                    WASMExpr::SetGlobal(
                        WASMGlobalName::Temp,
                        Ast::new(
                            left_loc,
                            right_loc,
                            WASMExpr::GetGlobal(WASMGlobalName::StackPointer),
                        ),
                    ),
                ),
                // Allocate a new stack frame and jump the stack pointer
                Ast::new(
                    left_loc,
                    right_loc,
                    WASMExpr::SetGlobal(
                        WASMGlobalName::StackPointer,
                        Ast::new(
                            left_loc,
                            right_loc,
                            WASMExpr::Call(
                                WASMDirectFuncName::StackAlloc,
                                vec![Ast::new(
                                    left_loc,
                                    right_loc,
                                    WASMExpr::Const(
                                        WASMType::I32,
                                        WASMValue::I32(self.get_frame_size() as i64),
                                    ),
                                )],
                            ),
                        ),
                    ),
                ),
                // Get the stack pointer we pushed above and set it as a frame pointer
                Ast::new(
                    left_loc,
                    right_loc,
                    WASMExpr::Store(
                        WASMType::I32,
                        Locals::get_offset(left_loc, right_loc, FRAME_POINTER_OFFSET as i64),
                        Ast::new(
                            left_loc,
                            right_loc,
                            WASMExpr::GetGlobal(WASMGlobalName::Temp),
                        ),
                    ),
                ),
                // Save the current function index so the stack map can be looked up later
                Ast::new(
                    left_loc,
                    right_loc,
                    WASMExpr::Store(
                        WASMType::I32,
                        Locals::get_offset(left_loc, right_loc, FUNCTION_INDEX_OFFSET as i64),
                        Ast::new(
                            left_loc,
                            right_loc,
                            WASMExpr::Const(
                                WASMType::I32,
                                WASMValue::I32(function_index.unwrap_or(NULL as i64)),
                            ),
                        ),
                    ),
                ),
            ]),
        )
    }

    fn get_frame_size(&self) -> usize {
        assert_eq!(
            self.map.len() * UNIVERSAL_ALIGN + STACK_FRAME_HEADER_SIZE,
            self.current_offset as usize
        );
        self.current_offset as usize
    }

    fn to_stack_map(self) -> StackMap {
        let mut values = self.map
            .into_iter()
            .map(|(_, value)| value)
            .collect::<Vec<(i64, WASMType)>>();

        values.sort_unstable_by_key(|(index, _)| *index);

        values.into_iter().map(|(_, wasm_type)| wasm_type).collect()
    }

    fn push(&mut self, name: CName, local_type: WASMType) {
        self.map.insert(name, (self.current_offset, local_type));
        self.current_offset += UNIVERSAL_ALIGN as i64;
    }

    fn get_offset(left_loc: usize, right_loc: usize, offset: i64) -> Ast<WASMExpr> {
        Ast::new(
            left_loc,
            right_loc,
            WASMExpr::BinOp(
                BinOp::Num(NumOp::Add),
                Ast::new(
                    left_loc,
                    right_loc,
                    WASMExpr::Const(WASMType::I32, WASMValue::I32(offset)),
                ),
                Ast::new(
                    left_loc,
                    right_loc,
                    WASMExpr::GetGlobal(WASMGlobalName::StackPointer),
                ),
                WASMType::I32,
            ),
        )
    }

    fn load(&self, left_loc: usize, right_loc: usize, name: &CName) -> Ast<WASMExpr> {
        match name {
            &CName::FuncArg | &CName::FuncCaptures => {
                Ast::new(left_loc, right_loc, WASMExpr::GetLocal(name.clone()))
            }
            _ => {
                let (offset, local_type) = self.map
                    .get(name)
                    .expect(&format!("Couldn't find local with name {}", name));

                Ast::new(
                    left_loc,
                    right_loc,
                    WASMExpr::Load(
                        local_type.clone(),
                        Locals::get_offset(left_loc, right_loc, *offset),
                    ),
                )
            }
        }
    }

    fn store(
        &self,
        left_loc: usize,
        right_loc: usize,
        name: &CName,
        value: Ast<WASMExpr>,
    ) -> Ast<WASMExpr> {
        let (offset, local_type) = self.map.get(name).expect(&format!(
            "Couldn't find local in stack map with name {}",
            name
        ));

        Ast::new(
            left_loc,
            right_loc,
            WASMExpr::Store(
                local_type.clone(),
                Locals::get_offset(left_loc, right_loc, *offset),
                value,
            ),
        )
    }
}

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

pub fn c_value_to_wasm(c_value: &Ast<CValue>, locals: &Locals) -> Ast<WASMExpr> {
    match &*c_value.expr {
        &CValue::Int(value) => {
            c_value.replace_expr(WASMExpr::Const(WASMType::I32, WASMValue::I32(value as i64)))
        }
        &CValue::Float(value) => {
            c_value.replace_expr(WASMExpr::Const(WASMType::F64, WASMValue::F64(value)))
        }
        &CValue::Ident(ref name, _) => locals.load(c_value.left_loc, c_value.right_loc, name),
        &CValue::Null => {
            c_value.replace_expr(WASMExpr::Const(WASMType::I32, WASMValue::I32(NULL as i64)))
        }
        &CValue::DerefBound(ref name, ref value_type) => c_value.replace_expr(WASMExpr::Load(
            c_type_to_wasm(value_type),
            c_value.replace_expr(WASMExpr::Load(
                WASMType::I32,
                locals.load(c_value.left_loc, c_value.right_loc, name),
            )),
        )),
        _ => panic!("Unhandled value: {}", c_value),
    }
}

fn c_expr_to_wasm(c_expr: &Ast<CExpr>, locals: &Locals) -> Ast<WASMExpr> {
    match &*c_expr.expr {
        &CExpr::Value(ref value) => c_value_to_wasm(&c_expr.replace_expr(value.clone()), locals),
        &CExpr::BinOp(ref op, ref left, ref right, ref op_type) => {
            let left_wasm_type = c_type_to_wasm(&left.expr.get_ctype());
            let right_wasm_type = c_type_to_wasm(&right.expr.get_ctype());
            let op_wasm_type = c_type_to_wasm(op_type);

            let left_wasm = c_value_to_wasm(left, locals);
            let right_wasm = c_value_to_wasm(right, locals);

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
            let closure_wasm = c_value_to_wasm(closure, locals);

            c_expr.replace_expr(WASMExpr::CallIndirect {
                param_types: vec![c_type_to_wasm(&arg.expr.get_ctype()), WASMType::I32],
                ret_type: c_type_to_wasm(ret_type),
                function_index: closure
                    .replace_expr(WASMExpr::Load(WASMType::I32, closure_wasm.clone())),
                args: vec![
                    c_value_to_wasm(arg, locals),
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
    wasm_locals: &mut Locals,
    wasm_exprs: &mut Vec<Ast<WASMExpr>>,
) {
    match &*c_statement.expr {
        CStatement::VarAssign(ref name, ref expr) => {
            wasm_exprs.push(wasm_locals.store(
                c_statement.left_loc,
                c_statement.right_loc,
                name,
                c_expr_to_wasm(expr, wasm_locals),
            ));
        }
        CStatement::RefAssign(ref name, ref value) => {
            wasm_exprs.push(c_statement.replace_expr(WASMExpr::Store(
                c_type_to_wasm(&value.expr.get_ctype()),
                wasm_locals.load(c_statement.left_loc, c_statement.right_loc, name),
                c_expr_to_wasm(value, wasm_locals),
            )));
        }
        CStatement::If(ref condition, ref consequent, ref alternate) => {
            let mut consequent_exprs = Vec::new();
            let mut alternate_exprs = Vec::new();

            flatten_c_statement(consequent, wasm_locals, &mut consequent_exprs);
            flatten_c_statement(alternate, wasm_locals, &mut alternate_exprs);

            wasm_exprs.push(c_statement.replace_expr(WASMExpr::If(
                c_value_to_wasm(condition, wasm_locals),
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
            wasm_exprs.push(wasm_locals.store(
                c_statement.left_loc,
                c_statement.right_loc,
                name,
                c_statement.replace_expr(WASMExpr::Call(
                    WASMDirectFuncName::HeapAlloc,
                    vec![c_statement.replace_expr(WASMExpr::Const(
                        WASMType::I32,
                        WASMValue::I32(((captures.len() + 1) * UNIVERSAL_ALIGN) as i64),
                    ))],
                )),
            ));

            // Store the function index first in the closure
            wasm_exprs.push(c_statement.replace_expr(WASMExpr::Store(
                WASMType::I32,
                wasm_locals.load(c_statement.left_loc, c_statement.right_loc, name),
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
                        wasm_locals.load(capture.left_loc, capture.right_loc, name),
                        capture.replace_expr(WASMExpr::Const(
                            WASMType::I32,
                            WASMValue::I32((index * UNIVERSAL_ALIGN) as i64),
                        )),
                        WASMType::I32,
                    )),
                    c_value_to_wasm(capture, wasm_locals),
                )));
            }
        }
        &CStatement::RefAlloc(ref name, _) => {
            // Heap allocate a new buffer and store the address in `name` local
            wasm_exprs.push(wasm_locals.store(
                c_statement.left_loc,
                c_statement.right_loc,
                name,
                c_statement.replace_expr(WASMExpr::Call(
                    WASMDirectFuncName::HeapAlloc,
                    vec![c_statement.replace_expr(WASMExpr::Const(
                        WASMType::I32,
                        WASMValue::I32(UNIVERSAL_ALIGN as i64),
                    ))],
                )),
            ));
        }
        _ => panic!("Unhandled statement: {}", c_statement),
    };
}

pub fn flatten_c_block(
    c_declarations: &Vec<CDeclaration>,
    c_statements: &Vec<Ast<CStatement>>,
    wasm_locals: &mut Locals,
    wasm_exprs: &mut Vec<Ast<WASMExpr>>,
) {
    for declaration in c_declarations {
        wasm_locals.push(declaration.1.clone(), c_type_to_wasm(&declaration.0))
    }

    for statement in c_statements {
        flatten_c_statement(statement, wasm_locals, wasm_exprs);
    }
}

pub fn c_func_to_wasm(c_func: &Ast<CFunc>) -> WASMFunc {
    let mut locals = Locals::new();
    let mut body = Vec::new();

    // TODO: can parameters always be located by GC in the calling stack frame?

    flatten_c_block(
        &c_func.expr.declarations,
        &c_func.expr.body,
        &mut locals,
        &mut body,
    );

    body.push(c_value_to_wasm(&c_func.expr.ret, &locals));

    // Must be inserted at beginning of function before any stack accesses
    body.insert(
        0,
        locals.init_stack_frame(
            c_func.left_loc,
            c_func.right_loc,
            Some(get_func_index(&c_func.expr.name) as i64),
        ),
    );

    // Return. Must be done at end of function after stack accesses
    body.push(c_func.replace_expr(WASMExpr::SetGlobal(
        WASMGlobalName::StackPointer,
        c_func.replace_expr(WASMExpr::Load(
            WASMType::I32,
            Locals::get_offset(
                c_func.left_loc,
                c_func.right_loc,
                FRAME_POINTER_OFFSET as i64,
            ),
        )),
    )));

    // TODO: free the old stack frame

    WASMFunc {
        name: c_func.expr.name.clone(),
        params: vec![
            (CName::FuncArg, c_type_to_wasm(&c_func.expr.param.expr)),
            (CName::FuncCaptures, WASMType::I32),
        ],
        result: c_type_to_wasm(&c_func.expr.ret.expr.get_ctype()),
        stack_map: locals.to_stack_map(),
        body,
    }
}
