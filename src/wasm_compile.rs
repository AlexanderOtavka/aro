use c_ast::{
    CDeclaration, CExpr, CFunc, CFuncName, CHookDeclaration, CHookName, CName, CStatement, CType,
    CValue, WellCTyped,
};
use std::collections::HashMap;
use std::u32;
use untyped_ast::{Ast, BinOp, NumOp};
use wasm_ast::{
    StackMap, WAsmDirectFuncName, WAsmExpr, WAsmFunc, WAsmGlobalName, WAsmHookImport, WAsmHookName,
    WAsmModule, WAsmType, WAsmValue,
};

const UNIVERSAL_ALIGN: usize = 8; // The size of a union
const STACK_FRAME_HEADER_SIZE: usize = 2 * UNIVERSAL_ALIGN;
const FRAME_POINTER_OFFSET: usize = 0 * UNIVERSAL_ALIGN;
const FUNCTION_INDEX_OFFSET: usize = 1 * UNIVERSAL_ALIGN;
const NULL: u32 = u32::MAX;
const GLOBALS_SECTION_LOCATION: usize = 64;

struct Locals {
    map: HashMap<CName, (i64, WAsmType)>,
    current_offset: i64,
}

impl Locals {
    fn new() -> Locals {
        Locals {
            map: HashMap::new(),
            current_offset: STACK_FRAME_HEADER_SIZE as i64,
        }
    }

    fn init_stack_frame(
        &self,
        left_loc: usize,
        right_loc: usize,
        function_index: Option<i64>,
    ) -> Ast<WAsmExpr> {
        Ast::new(
            left_loc,
            right_loc,
            WAsmExpr::Sequence(vec![
                // Save the current call stack pointer
                Ast::new(
                    left_loc,
                    right_loc,
                    WAsmExpr::SetGlobal(
                        WAsmGlobalName::RegisterI32_1,
                        Ast::new(
                            left_loc,
                            right_loc,
                            WAsmExpr::GetGlobal(WAsmGlobalName::StackPointer),
                        ),
                    ),
                ),
                // Allocate a new stack frame and jump the stack pointer
                Ast::new(
                    left_loc,
                    right_loc,
                    WAsmExpr::SetGlobal(
                        WAsmGlobalName::StackPointer,
                        Ast::new(
                            left_loc,
                            right_loc,
                            WAsmExpr::Call(
                                WAsmDirectFuncName::StackAlloc,
                                vec![Ast::new(
                                    left_loc,
                                    right_loc,
                                    WAsmExpr::Const(
                                        WAsmType::I32,
                                        WAsmValue::I32(self.get_frame_size() as i64),
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
                    WAsmExpr::Store(
                        WAsmType::I32,
                        Locals::get_offset(left_loc, right_loc, FRAME_POINTER_OFFSET as i64),
                        Ast::new(
                            left_loc,
                            right_loc,
                            WAsmExpr::GetGlobal(WAsmGlobalName::RegisterI32_1),
                        ),
                    ),
                ),
                // Save the current function index so the stack map can be looked up later
                Ast::new(
                    left_loc,
                    right_loc,
                    WAsmExpr::Store(
                        WAsmType::I32,
                        Locals::get_offset(left_loc, right_loc, FUNCTION_INDEX_OFFSET as i64),
                        Ast::new(
                            left_loc,
                            right_loc,
                            WAsmExpr::Const(
                                WAsmType::I32,
                                WAsmValue::I32(function_index.unwrap_or(NULL as i64)),
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
            .collect::<Vec<(i64, WAsmType)>>();

        values.sort_unstable_by_key(|(index, _)| *index);

        values.into_iter().map(|(_, wasm_type)| wasm_type).collect()
    }

    fn push(&mut self, name: CName, local_type: WAsmType) {
        self.map.insert(name, (self.current_offset, local_type));
        self.current_offset += UNIVERSAL_ALIGN as i64;
    }

    fn get_offset(left_loc: usize, right_loc: usize, offset: i64) -> Ast<WAsmExpr> {
        Ast::new(
            left_loc,
            right_loc,
            WAsmExpr::BinOp(
                BinOp::Num(NumOp::Add),
                Ast::new(
                    left_loc,
                    right_loc,
                    WAsmExpr::Const(WAsmType::I32, WAsmValue::I32(offset)),
                ),
                Ast::new(
                    left_loc,
                    right_loc,
                    WAsmExpr::GetGlobal(WAsmGlobalName::StackPointer),
                ),
                WAsmType::I32,
            ),
        )
    }

    fn load(&self, left_loc: usize, right_loc: usize, name: &CName) -> Option<Ast<WAsmExpr>> {
        let (offset, local_type) = self.map.get(name)?;

        Some(Ast::new(
            left_loc,
            right_loc,
            WAsmExpr::Load(
                local_type.clone(),
                Locals::get_offset(left_loc, right_loc, *offset),
            ),
        ))
    }

    fn store(
        &self,
        left_loc: usize,
        right_loc: usize,
        name: &CName,
        value: Ast<WAsmExpr>,
    ) -> Option<Ast<WAsmExpr>> {
        let (offset, local_type) = self.map.get(name)?;

        Some(Ast::new(
            left_loc,
            right_loc,
            WAsmExpr::Store(
                local_type.clone(),
                Locals::get_offset(left_loc, right_loc, *offset),
                value,
            ),
        ))
    }
}

struct Globals {
    map: HashMap<String, (i64, WAsmType)>,
}

impl Globals {
    fn from_map(map: HashMap<String, CType>) -> Globals {
        Globals {
            map: map.into_iter()
                .enumerate()
                .map(|(index, (name, c_type))| {
                    (
                        name,
                        ((index * UNIVERSAL_ALIGN) as i64, c_type_to_wasm(&c_type)),
                    )
                })
                .collect(),
        }
    }

    fn init(
        &self,
        left_loc: usize,
        right_loc: usize,
        values: &HashMap<String, Ast<WAsmExpr>>,
    ) -> Ast<WAsmExpr> {
        Ast::new(
            left_loc,
            right_loc,
            WAsmExpr::Sequence(
                values
                    .into_iter()
                    .map(|(name, value)| {
                        let (offset, global_type) = self.map
                            .get(name)
                            .expect(&format!("Couldn't init unknown global: {}", name));

                        let ast = |expr| Ast::new(left_loc, right_loc, expr);

                        ast(WAsmExpr::Sequence(vec![
                            // Allocate space for the value wrapper
                            ast(WAsmExpr::SetGlobal(
                                WAsmGlobalName::RegisterI32_1,
                                ast(WAsmExpr::Call(
                                    WAsmDirectFuncName::HeapAlloc,
                                    vec![ast(WAsmExpr::Const(
                                        WAsmType::I32,
                                        WAsmValue::I32(UNIVERSAL_ALIGN as i64),
                                    ))],
                                )),
                            )),
                            // Put the value in the wrapper
                            ast(WAsmExpr::Store(
                                global_type.clone(),
                                ast(WAsmExpr::GetGlobal(WAsmGlobalName::RegisterI32_1)),
                                value.clone(),
                            )),
                            // Allocate space for the wrapper wrapper
                            ast(WAsmExpr::SetGlobal(
                                WAsmGlobalName::RegisterI32_2,
                                ast(WAsmExpr::Call(
                                    WAsmDirectFuncName::HeapAlloc,
                                    vec![ast(WAsmExpr::Const(
                                        WAsmType::I32,
                                        WAsmValue::I32(UNIVERSAL_ALIGN as i64),
                                    ))],
                                )),
                            )),
                            // Put the wrapper in the wrapper wrapper
                            ast(WAsmExpr::Store(
                                WAsmType::I32,
                                ast(WAsmExpr::GetGlobal(WAsmGlobalName::RegisterI32_2)),
                                ast(WAsmExpr::GetGlobal(WAsmGlobalName::RegisterI32_1)),
                            )),
                            // Put the wrapper wrapper at the offset for the global
                            ast(WAsmExpr::Store(
                                WAsmType::I32,
                                Globals::get_offset(left_loc, right_loc, *offset),
                                ast(WAsmExpr::GetGlobal(WAsmGlobalName::RegisterI32_2)),
                            )),
                        ]))
                    })
                    .collect(),
            ),
        )
    }

    fn get_offset(left_loc: usize, right_loc: usize, offset: i64) -> Ast<WAsmExpr> {
        Ast::new(
            left_loc,
            right_loc,
            WAsmExpr::Const(
                WAsmType::I32,
                WAsmValue::I32(offset + GLOBALS_SECTION_LOCATION as i64),
            ),
        )
    }

    fn load(&self, left_loc: usize, right_loc: usize, name: &str) -> Option<Ast<WAsmExpr>> {
        let (offset, _) = self.map.get(name)?;

        Some(Ast::new(
            left_loc,
            right_loc,
            WAsmExpr::Load(
                // Since globals are all double indirected, we are really
                // loading a pointer. The DerefBound will take care of loading
                // it in the correct type.
                WAsmType::I32,
                Globals::get_offset(left_loc, right_loc, *offset),
            ),
        ))
    }
}

fn load_name(
    locals: &Locals,
    globals: &Globals,
    left_loc: usize,
    right_loc: usize,
    name: &CName,
) -> Ast<WAsmExpr> {
    if name == &CName::FuncArg || name == &CName::FuncCaptures {
        Ast::new(left_loc, right_loc, WAsmExpr::GetLocal(name.clone()))
    } else if let Some(expr) = locals.load(left_loc, right_loc, name) {
        expr
    } else {
        let error_message = format!("Couldn't find name: {}", name);
        if let &CName::Ident(ref name_string) = name {
            globals
                .load(left_loc, right_loc, name_string)
                .expect(&error_message)
        } else {
            panic!(error_message)
        }
    }
}

fn store_name(
    locals: &Locals,
    left_loc: usize,
    right_loc: usize,
    name: &CName,
    value: Ast<WAsmExpr>,
) -> Ast<WAsmExpr> {
    locals
        .store(left_loc, right_loc, name, value.clone())
        .expect(&format!("Couldn't find name: {}", name))
}

fn get_func_index(name: &CFuncName) -> u64 {
    match name {
        CFuncName::Func(index) | CFuncName::AdaptorFunc(index) => *index,
    }
}

fn c_type_to_wasm(c_type: &CType) -> WAsmType {
    match c_type {
        &CType::Int | &CType::Bool | &CType::Closure { .. } | &CType::Object | &CType::Ref(_) => {
            WAsmType::I32
        }
        &CType::Any => WAsmType::I64,
        &CType::Float => WAsmType::F64,
    }
}

fn c_value_to_wasm(c_value: &Ast<CValue>, locals: &Locals, globals: &Globals) -> Ast<WAsmExpr> {
    match &*c_value.expr {
        &CValue::Int(value) => {
            c_value.replace_expr(WAsmExpr::Const(WAsmType::I32, WAsmValue::I32(value as i64)))
        }
        &CValue::Float(value) => {
            c_value.replace_expr(WAsmExpr::Const(WAsmType::F64, WAsmValue::F64(value)))
        }
        &CValue::Bool(value) => c_value.replace_expr(WAsmExpr::Const(
            WAsmType::I32,
            WAsmValue::I32(if value { 1 } else { 0 } as i64),
        )),
        &CValue::Ident(ref name, _) => {
            load_name(locals, globals, c_value.left_loc, c_value.right_loc, name)
        }
        &CValue::Null => {
            c_value.replace_expr(WAsmExpr::Const(WAsmType::I32, WAsmValue::I32(NULL as i64)))
        }
        &CValue::DerefBound(ref name, ref value_type) => c_value.replace_expr(WAsmExpr::Load(
            c_type_to_wasm(value_type),
            c_value.replace_expr(WAsmExpr::Load(
                WAsmType::I32,
                load_name(locals, globals, c_value.left_loc, c_value.right_loc, name),
            )),
        )),
    }
}

fn c_expr_to_wasm(c_expr: &Ast<CExpr>, locals: &Locals, globals: &Globals) -> Ast<WAsmExpr> {
    match &*c_expr.expr {
        &CExpr::Value(ref value) => {
            c_value_to_wasm(&c_expr.replace_expr(value.clone()), locals, globals)
        }
        &CExpr::BinOp(ref op, ref left, ref right, ref op_type) => {
            let left_wasm_type = c_type_to_wasm(&left.expr.get_ctype());
            let right_wasm_type = c_type_to_wasm(&right.expr.get_ctype());
            let op_wasm_type = c_type_to_wasm(op_type);

            let left_wasm = c_value_to_wasm(left, locals, globals);
            let right_wasm = c_value_to_wasm(right, locals, globals);

            let casted_left = if op_wasm_type == WAsmType::F64 && left_wasm_type == WAsmType::I32 {
                left.replace_expr(WAsmExpr::PromoteInt(left_wasm))
            } else {
                left_wasm
            };
            let casted_right = if op_wasm_type == WAsmType::F64 && right_wasm_type == WAsmType::I32
            {
                right.replace_expr(WAsmExpr::PromoteInt(right_wasm))
            } else {
                right_wasm
            };

            c_expr.replace_expr(WAsmExpr::BinOp(
                op.clone(),
                casted_left,
                casted_right,
                op_wasm_type,
            ))
        }
        &CExpr::ClosureCall(ref closure, ref arg, ref ret_type) => {
            let closure_wasm = c_value_to_wasm(closure, locals, globals);

            c_expr.replace_expr(WAsmExpr::CallIndirect {
                param_types: vec![c_type_to_wasm(&arg.expr.get_ctype()), WAsmType::I32],
                ret_type: c_type_to_wasm(ret_type),
                function_index: closure
                    .replace_expr(WAsmExpr::Load(WAsmType::I32, closure_wasm.clone())),
                args: vec![
                    c_value_to_wasm(arg, locals, globals),
                    closure.replace_expr(WAsmExpr::BinOp(
                        BinOp::Num(NumOp::Add),
                        closure_wasm,
                        closure.replace_expr(WAsmExpr::Const(
                            WAsmType::I32,
                            WAsmValue::I32(UNIVERSAL_ALIGN as i64),
                        )),
                        WAsmType::I32,
                    )),
                ],
            })
        }
        &CExpr::HookCall(CHookName(ref path), ref args, _) => c_expr.replace_expr(WAsmExpr::Call(
            WAsmDirectFuncName::Hook(WAsmHookName(path.clone())),
            args.into_iter()
                .map(|value| c_value_to_wasm(value, locals, globals))
                .collect(),
        )),
        &CExpr::ObjectAccess {
            ref object,
            index,
            ref field_type,
        } => c_expr.replace_expr(WAsmExpr::Load(
            c_type_to_wasm(field_type),
            c_expr.replace_expr(WAsmExpr::BinOp(
                BinOp::Num(NumOp::Add),
                c_value_to_wasm(object, locals, globals),
                c_expr.replace_expr(WAsmExpr::Const(
                    WAsmType::I32,
                    WAsmValue::I32((index * UNIVERSAL_ALIGN) as i64),
                )),
                WAsmType::I32,
            )),
        )),
        CExpr::Cast {
            ref value,
            ref to_type,
        } => {
            let from_wasm_type = c_type_to_wasm(&value.expr.get_ctype());
            let to_wasm_type = c_type_to_wasm(to_type);
            let wasm_value = c_value_to_wasm(value, locals, globals);

            match (from_wasm_type, to_wasm_type) {
                (WAsmType::I32, WAsmType::F64) => {
                    c_expr.replace_expr(WAsmExpr::PromoteInt(wasm_value))
                }
                (WAsmType::F64, WAsmType::I32) => {
                    c_expr.replace_expr(WAsmExpr::TruncateFloat(wasm_value))
                }
                // TODO: explictly handle all possibilities?
                _ => wasm_value,
            }
        }
        _ => panic!("Unhandled expr: {}", c_expr),
    }
}

fn flatten_c_statement(
    c_statement: &Ast<CStatement>,
    wasm_locals: &mut Locals,
    wasm_globals: &Globals,
    wasm_exprs: &mut Vec<Ast<WAsmExpr>>,
) {
    match &*c_statement.expr {
        CStatement::VarAssign(ref name, ref expr) => {
            wasm_exprs.push(store_name(
                wasm_locals,
                c_statement.left_loc,
                c_statement.right_loc,
                name,
                c_expr_to_wasm(expr, wasm_locals, wasm_globals),
            ));
        }
        CStatement::RefAssign(ref name, ref value) => {
            wasm_exprs.push(c_statement.replace_expr(WAsmExpr::Store(
                c_type_to_wasm(&value.expr.get_ctype()),
                load_name(
                    wasm_locals,
                    wasm_globals,
                    c_statement.left_loc,
                    c_statement.right_loc,
                    name,
                ),
                c_expr_to_wasm(value, wasm_locals, wasm_globals),
            )));
        }
        CStatement::If(ref condition, ref consequent, ref alternate) => {
            let mut consequent_exprs = Vec::new();
            let mut alternate_exprs = Vec::new();

            flatten_c_statement(consequent, wasm_locals, wasm_globals, &mut consequent_exprs);
            flatten_c_statement(alternate, wasm_locals, wasm_globals, &mut alternate_exprs);

            wasm_exprs.push(c_statement.replace_expr(WAsmExpr::If(
                c_value_to_wasm(condition, wasm_locals, wasm_globals),
                consequent_exprs,
                alternate_exprs,
            )));
        }
        CStatement::Block(ref c_declarations, ref c_statements) => {
            flatten_c_block(
                c_declarations,
                c_statements,
                wasm_locals,
                wasm_globals,
                wasm_exprs,
            );
        }
        CStatement::ClosureInit {
            ref name,
            ref function,
            ref captures,
        } => {
            // Heap allocate a new buffer and store the address in `name` local
            wasm_exprs.push(store_name(
                wasm_locals,
                c_statement.left_loc,
                c_statement.right_loc,
                name,
                c_statement.replace_expr(WAsmExpr::Call(
                    WAsmDirectFuncName::HeapAlloc,
                    vec![c_statement.replace_expr(WAsmExpr::Const(
                        WAsmType::I32,
                        WAsmValue::I32(((captures.len() + 1) * UNIVERSAL_ALIGN) as i64),
                    ))],
                )),
            ));

            // Store the function index first in the closure
            wasm_exprs.push(c_statement.replace_expr(WAsmExpr::Store(
                WAsmType::I32,
                load_name(
                    wasm_locals,
                    wasm_globals,
                    c_statement.left_loc,
                    c_statement.right_loc,
                    name,
                ),
                c_statement.replace_expr(WAsmExpr::Const(
                    WAsmType::I32,
                    WAsmValue::I32(get_func_index(function) as i64),
                )),
            )));

            // Fill the rest of the closure with captures aligned at
            // UNIVERSAL_ALIGN
            for (index, capture) in captures.into_iter().enumerate() {
                wasm_exprs.push(capture.replace_expr(WAsmExpr::Store(
                    c_type_to_wasm(&capture.expr.get_ctype()),
                    capture.replace_expr(WAsmExpr::BinOp(
                        BinOp::Num(NumOp::Add),
                        load_name(
                            wasm_locals,
                            wasm_globals,
                            capture.left_loc,
                            capture.right_loc,
                            name,
                        ),
                        capture.replace_expr(WAsmExpr::Const(
                            WAsmType::I32,
                            WAsmValue::I32(((index + 1) * UNIVERSAL_ALIGN) as i64),
                        )),
                        WAsmType::I32,
                    )),
                    c_value_to_wasm(capture, wasm_locals, wasm_globals),
                )));
            }
        }
        CStatement::ObjectInit { ref name, ref data } => {
            // Heap allocate a new buffer and store the address in `name` local
            wasm_exprs.push(store_name(
                wasm_locals,
                c_statement.left_loc,
                c_statement.right_loc,
                name,
                c_statement.replace_expr(WAsmExpr::Call(
                    WAsmDirectFuncName::HeapAlloc,
                    vec![c_statement.replace_expr(WAsmExpr::Const(
                        WAsmType::I32,
                        WAsmValue::I32((data.len() * UNIVERSAL_ALIGN) as i64),
                    ))],
                )),
            ));

            // Fill the buffer with data aligned at UNIVERSAL_ALIGN
            for (index, capture) in data.into_iter().enumerate() {
                wasm_exprs.push(capture.replace_expr(WAsmExpr::Store(
                    c_type_to_wasm(&capture.expr.get_ctype()),
                    capture.replace_expr(WAsmExpr::BinOp(
                        BinOp::Num(NumOp::Add),
                        load_name(
                            wasm_locals,
                            wasm_globals,
                            capture.left_loc,
                            capture.right_loc,
                            name,
                        ),
                        capture.replace_expr(WAsmExpr::Const(
                            WAsmType::I32,
                            WAsmValue::I32((index * UNIVERSAL_ALIGN) as i64),
                        )),
                        WAsmType::I32,
                    )),
                    c_value_to_wasm(capture, wasm_locals, wasm_globals),
                )));
            }
        }
        &CStatement::RefAlloc(ref name, _) => {
            // Heap allocate a new buffer and store the address in `name` local
            wasm_exprs.push(store_name(
                wasm_locals,
                c_statement.left_loc,
                c_statement.right_loc,
                name,
                c_statement.replace_expr(WAsmExpr::Call(
                    WAsmDirectFuncName::HeapAlloc,
                    vec![c_statement.replace_expr(WAsmExpr::Const(
                        WAsmType::I32,
                        WAsmValue::I32(UNIVERSAL_ALIGN as i64),
                    ))],
                )),
            ));
        }
        _ => panic!("Unhandled statement: {}", c_statement),
    };
}

fn flatten_c_block(
    c_declarations: &Vec<CDeclaration>,
    c_statements: &Vec<Ast<CStatement>>,
    wasm_locals: &mut Locals,
    wasm_globals: &Globals,
    wasm_exprs: &mut Vec<Ast<WAsmExpr>>,
) {
    for declaration in c_declarations {
        wasm_locals.push(declaration.1.clone(), c_type_to_wasm(&declaration.0))
    }

    for statement in c_statements {
        flatten_c_statement(statement, wasm_locals, wasm_globals, wasm_exprs);
    }
}

fn c_func_to_wasm(c_func: &Ast<CFunc>, wasm_globals: &Globals) -> WAsmFunc {
    let mut locals = Locals::new();
    let mut body = Vec::new();

    // TODO: can parameters always be located by GC in the calling stack frame?

    flatten_c_block(
        &c_func.expr.declarations,
        &c_func.expr.body,
        &mut locals,
        wasm_globals,
        &mut body,
    );

    body.push(c_value_to_wasm(&c_func.expr.ret, &locals, wasm_globals));

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
    body.push(c_func.replace_expr(WAsmExpr::SetGlobal(
        WAsmGlobalName::StackPointer,
        c_func.replace_expr(WAsmExpr::Load(
            WAsmType::I32,
            Locals::get_offset(
                c_func.left_loc,
                c_func.right_loc,
                FRAME_POINTER_OFFSET as i64,
            ),
        )),
    )));

    // TODO: free the old stack frame

    WAsmFunc {
        name: c_func.expr.name.clone(),
        params: vec![
            (CName::FuncArg, c_type_to_wasm(&c_func.expr.param.expr)),
            (CName::FuncCaptures, WAsmType::I32),
        ],
        result: c_type_to_wasm(&c_func.expr.ret.expr.get_ctype()),
        stack_map: locals.to_stack_map(),
        body,
    }
}

fn c_hook_declaration_to_wasm(c_hook_declaration: &CHookDeclaration) -> WAsmHookImport {
    let &CHookDeclaration(ref ret_type, CHookName(ref path), ref args) = c_hook_declaration;

    WAsmHookImport {
        name: WAsmHookName(path.clone()),
        params: args.into_iter().map(c_type_to_wasm).collect(),
        result: c_type_to_wasm(ret_type),
    }
}

impl WAsmModule {
    pub fn from_c(
        c_externs: &Vec<CHookDeclaration>,
        table_offset: u32,
        c_functions: &Vec<Ast<CFunc>>,
        c_globals_init_declarations: &Vec<CDeclaration>,
        c_globals_init_statements: &Vec<Ast<CStatement>>,
        c_main_declarations: &Vec<CDeclaration>,
        c_main_statements: &Vec<Ast<CStatement>>,
        c_value: Option<&Ast<CValue>>,
        c_globals: &HashMap<String, Ast<CValue>>,
    ) -> WAsmModule {
        let mut main_locals = Locals::new();
        let mut main_init = Vec::new();
        let mut main_body = Vec::new();

        let wasm_globals = Globals::from_map(
            c_globals
                .into_iter()
                .map(|(name, value)| (name.clone(), value.expr.get_ctype()))
                .collect(),
        );

        flatten_c_block(
            &c_globals_init_declarations,
            &c_globals_init_statements,
            &mut main_locals,
            &wasm_globals,
            &mut main_init,
        );

        flatten_c_block(
            &c_main_declarations,
            &c_main_statements,
            &mut main_locals,
            &wasm_globals,
            &mut main_body,
        );

        // The stack allocation has to come first (even before globals are
        // initialized), but we don't know how much we need until we have
        // flattened the block and initialized the globals
        main_init.insert(0, main_locals.init_stack_frame(0, 0, None));

        main_init.push(Ast::new(0, 0, WAsmExpr::Sequence(vec![])));
        main_init.push(Ast::new(0, 0, WAsmExpr::Sequence(vec![])));

        // Should be done before the rest of the body but we need the
        // declarations to be bound first
        main_init.push(
            wasm_globals.init(
                0,
                0,
                &c_globals
                    .into_iter()
                    .map(|(name, value)| {
                        (
                            name.clone(),
                            c_value_to_wasm(value, &main_locals, &wasm_globals),
                        )
                    })
                    .collect(),
            ),
        );

        let mut table = c_functions
            .into_iter()
            .map(|c_func| c_func_to_wasm(&c_func, &wasm_globals))
            .collect::<Vec<WAsmFunc>>();

        table.sort_unstable_by_key(|wasm_func| get_func_index(&wasm_func.name));

        let mut c_externs = c_externs.clone();
        c_externs.sort_by_key(|CHookDeclaration(_, CHookName(path), _)| path.clone());
        c_externs.dedup();

        WAsmModule {
            hook_imports: c_externs
                .into_iter()
                .map(|hook_declaration| c_hook_declaration_to_wasm(&hook_declaration))
                .collect(),
            table_offset,
            table,
            main_init,
            main_body,
            main_return: c_value.map(|value| {
                (
                    c_value_to_wasm(value, &main_locals, &wasm_globals),
                    c_type_to_wasm(&value.expr.get_ctype()),
                )
            }),
        }
    }
}
