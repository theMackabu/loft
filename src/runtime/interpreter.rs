mod r#enum;
mod r#let;
mod r#macro;

mod assign;
mod cast;
mod environment;
mod loops;
mod methods;
mod pattern;
mod pointer;
mod structs;

use crate::{
    compare::{compare_values, value_equals},
    impl_binary_ops, impl_promote_to_type, inner_val,
    models::Either,
    parser::{ast::*, lexer::*},
    unbind,
    util::unwrap_value,
    val,
};

use super::value::*;
use environment::Environment;

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    io::{self, Write},
    rc::Rc,
};

type Macro = (MacroDelimiter, Vec<TokenInfo>, Vec<r#macro::MacroBranch>);

pub struct Interpreter<'st> {
    env: Environment,
    fnc: HashMap<String, &'st Stmt>,
    mcs: HashMap<String, Macro>,
}

impl<'st> Interpreter<'st> {
    pub fn new(ast: &'st [Stmt]) -> Result<Self, String> {
        let function_count = ast.iter().filter(|stmt| matches!(stmt, Stmt::Function { .. })).count();
        let mut fnc = HashMap::with_capacity(function_count);

        for stmt in ast {
            if let Stmt::Function { name, .. } = stmt {
                fnc.insert(name.clone(), stmt);
            }
        }

        let mut intrp = Self {
            fnc,
            mcs: HashMap::new(),
            env: Environment::new(),
        };

        intrp.env.scope_resolver.resolve_program(ast)?;

        intrp.declare_globals(ast)?;
        intrp.import_prelude()?;

        Ok(intrp)
    }

    pub fn start_main(&mut self) -> Result<Value, String> {
        let main_func = self.fnc.get("main").ok_or("No main function found")?;
        let result = self.execute_statement(&main_func.to_owned())?;

        let result_inner = {
            let borrowed = result.borrow();
            borrowed.inner()
        };

        match result_inner {
            ValueType::Enum { enum_type, variant, data } if enum_type == "Result" => match variant.as_str() {
                "Ok" => match data {
                    Some(values) if !values.is_empty() => Ok(values[0].clone()),
                    _ => Ok(val!(ValueType::Unit)),
                },

                "Err" => match data {
                    Some(values) if !values.is_empty() => Err(values[0].borrow().to_string()),
                    _ => Err("Unknown error".to_string()),
                },

                _ => Ok(val!(ValueType::Enum {
                    enum_type: enum_type.clone(),
                    variant: variant.clone(),
                    data: data.clone(),
                })),
            },

            _ => Ok(result),
        }
    }

    pub fn execute(&mut self, statements: &[Stmt]) -> Result<Value, String> {
        let mut last_value = val!(ValueType::Unit);

        for stmt in statements {
            let result = self.execute_statement(stmt)?;

            let inner_value = {
                let borrowed = result.borrow();
                borrowed.inner()
            };

            match inner_value {
                ValueType::Return(val) => {
                    return Ok(val.clone());
                }

                ValueType::Break(_, _) => {
                    return Err("break statement not allowed outside loop".into());
                }

                ValueType::Continue(_) => {
                    return Err("continue statement not allowed outside loop".into());
                }

                _ => last_value = result,
            }
        }

        Ok(last_value)
    }

    fn import_prelude(&mut self) -> Result<(), String> {
        self.env.register_global_variant("Some", "Option")?;
        self.env.register_global_variant("None", "Option")?;
        self.env.register_global_variant("Ok", "Result")?;
        self.env.register_global_variant("Err", "Result")?;

        Ok(())
    }

    fn declare_globals(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for stmt in statements {
            match stmt {
                Stmt::Impl { target, items, .. } => {
                    self.handle_impl_block(target, items)?;
                }

                Stmt::Struct { path, fields, .. } => {
                    self.handle_struct_def(path, fields.to_owned())?;
                }

                Stmt::Enum { name, variants, .. } => {
                    self.handle_enum_def(name, variants.to_owned())?;
                }

                Stmt::MacroDefinition { name, tokens, .. } => {
                    self.handle_macro_definition(name, tokens)?;
                }

                Stmt::Module { name, body, .. } => {
                    self.env.scope_resolver.declare_module(name)?;
                    self.env.enter_scope();
                    self.declare_globals(body)?;
                    self.env.exit_scope();
                }

                Stmt::Const {
                    name, initializer, type_annotation, ..
                } => {
                    let value = self.evaluate_expression(initializer)?;
                    let value = self.perform_cast(value.clone(), type_annotation.as_ref().expect("expected op level types")).unwrap_or(value);

                    self.env.scope_resolver.declare_variable(name, false);
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                Stmt::Static {
                    name, initializer, type_annotation, ..
                } => {
                    let value = self.evaluate_expression(initializer)?;
                    let value = self.perform_cast(value.clone(), type_annotation.as_ref().expect("expected op level types")).unwrap_or(value);

                    self.env.scope_resolver.declare_variable(name, false);
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                _ => {}
            }
        }
        Ok(())
    }

    fn execute_statement(&mut self, stmt: &Stmt) -> Result<Value, String> {
        match stmt {
            Stmt::ExpressionValue(expr) => self.evaluate_expression(expr),

            Stmt::Continue(label) => Ok(val!(ValueType::Continue(label.clone()))),

            Stmt::ExpressionStmt(expr) => {
                let result = self.evaluate_expression(expr)?;

                let is_control_flow = {
                    let borrowed = result.borrow();
                    matches!(borrowed.inner(), ValueType::Break(_, _) | ValueType::Continue(_) | ValueType::Return(_))
                };

                if is_control_flow { Ok(result) } else { Ok(ValueEnum::unit()) }
            }

            Stmt::Return(expr) => {
                let res = match expr {
                    Some(e) => self.evaluate_expression(e)?,
                    None => ValueEnum::unit(),
                };

                Ok(res.into_return())
            }

            Stmt::Break(label, value) => {
                let break_value = if let Some(expr) = value { Some(self.evaluate_expression(expr)?) } else { None };
                Ok(val!(ValueType::Break(label.clone(), break_value)))
            }

            Stmt::Struct { path, fields, .. } => {
                self.handle_struct_def(path, fields.to_owned())?;
                Ok(ValueEnum::unit())
            }

            Stmt::Enum { name, variants, .. } => {
                self.handle_enum_def(name, variants.to_owned())?;
                Ok(ValueEnum::unit())
            }

            Stmt::MacroDefinition { name, tokens, .. } => {
                self.handle_macro_definition(name, tokens)?;
                Ok(ValueEnum::unit())
            }

            Stmt::Module { body, .. } => {
                self.env.enter_scope();
                let result = self.execute(body)?;
                self.env.exit_scope();
                Ok(result)
            }

            Stmt::Const {
                name, initializer, type_annotation, ..
            } => {
                let value = self.evaluate_expression(initializer)?;
                let value = self.perform_cast(value.clone(), type_annotation.as_ref().expect("expected op level types")).unwrap_or(value);

                self.env.scope_resolver.declare_variable(name, false);
                if let Some(scope) = self.env.scopes.first_mut() {
                    scope.insert(name.clone(), value);
                }

                Ok(ValueEnum::unit())
            }

            Stmt::Static {
                name, initializer, type_annotation, ..
            } => {
                let value = self.evaluate_expression(initializer)?;
                let value = self.perform_cast(value.clone(), type_annotation.as_ref().expect("expected op level types")).unwrap_or(value);

                self.env.scope_resolver.declare_variable(name, false);
                if let Some(scope) = self.env.scopes.first_mut() {
                    scope.insert(name.clone(), value);
                }

                Ok(ValueEnum::unit())
            }

            attr @ Stmt::Let { .. } => self.parse_let_statement(attr),

            Stmt::Function { name, params, body, return_type, .. } => {
                if name == "main" {
                    if !params.is_empty() {
                        return Err("main function cannot have parameters".into());
                    }

                    if let Some(ret_type) = return_type {
                        match ret_type {
                            Type::Path(path)
                                if matches! { path.segments.as_slice(),
                                    [seg] if seg.ident == "i32" && seg.generics.is_empty()
                                } => {}

                            Type::Path(path)
                                if matches! { path.segments.as_slice(),
                                    [seg] if seg.ident == "Result" && seg.generics.len() == 2
                                } => {}

                            _ => return Err("main function can only return i32, Result<T, E>, or nothing".into()),
                        }
                    }
                }

                self.env.scope_resolver.declare_function(name)?;
                self.env.enter_function_scope();

                for (pat, ty) in params {
                    if let Pattern::Identifier { name, mutable } = pat {
                        match &ty {
                            Type::Reference { mutable: ref_mutable, .. } => {
                                self.env.scope_resolver.declare_reference(name, *ref_mutable);
                            }
                            _ => {
                                self.env.scope_resolver.declare_variable(name, *mutable);
                            }
                        }
                    }
                }
                let result = self.execute(body)?;

                let return_val = {
                    let borrowed = result.borrow();
                    match borrowed.inner() {
                        ValueType::Return(val) => val.clone(),
                        _ => result.clone(),
                    }
                };

                self.env.exit_function_scope();
                Ok(return_val)
            }

            _ => Ok(ValueEnum::unit()),
        }
    }

    fn evaluate_expression(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Path(path) => {
                if path.segments.len() == 2 {
                    let type_name = &path.segments[0].ident;
                    let method_name = &path.segments[1].ident;

                    if let Some(value) = self.env.get_variable(type_name) {
                        match value.borrow().inner() {
                            ValueType::StructDef { methods, .. } => {
                                if let Some(function) = methods.get(method_name) {
                                    return Ok(val!(ValueType::StaticMethod {
                                        struct_name: type_name.to_string(),
                                        method: method_name.to_string(),
                                        function: function.clone(),
                                    }));
                                }
                            }
                            _ => {}
                        }
                    }

                    Ok(val!(ValueType::Enum {
                        enum_type: type_name.clone(),
                        variant: method_name.clone(),
                        data: None,
                    }))
                } else {
                    let name = &path.segments[0].ident;
                    if let Some(value) = self.env.get_variable(name) {
                        Ok(value.clone())
                    } else {
                        Err(format!("Undefined symbol: {}", name))
                    }
                }
            }

            Expr::MacroInvocation { name, delimiter, tokens } => {
                let expanded = self.expand_macro(name, delimiter, tokens, 0)?;
                self.evaluate_expression(&expanded)
            }

            Expr::StructInit { path, fields } => {
                return self.evaluate_struct_init(path, fields.to_owned());
            }

            Expr::MethodCall { object, method, arguments } => {
                let obj_value = self.evaluate_expression(object)?;
                self.evaluate_method_call(obj_value, method, arguments)
            }

            Expr::Cast { expr, target_type } => {
                let value = self.evaluate_expression(expr)?;
                self.perform_cast(value, target_type)
            }

            Expr::String(value) => Ok(ValueEnum::new_str(value)),

            Expr::Boolean(b) => Ok(val!(ValueType::Boolean(*b))),

            Expr::Loop { label, body } => self.handle_loop(label, body),

            Expr::While { label, condition, body } => self.handle_while(label, condition, body),

            Expr::For { label, pattern, iterable, body } => self.handle_for(label, pattern, iterable, body),

            Expr::Range { start, end, inclusive } => {
                let start_val = match start {
                    Some(s) => self.evaluate_expression(s)?,
                    None => val!(ValueType::Unbounded),
                };

                let end_val = match end {
                    Some(e) => self.evaluate_expression(e)?,
                    None => val!(ValueType::Unbounded),
                };

                Ok(val!(ValueType::Range {
                    start: start_val,
                    end: end_val,
                    inclusive: *inclusive
                }))
            }

            Expr::Integer(value, ty) => Ok(val!(match ty.to_owned().unwrap_or(NumericType::I32) {
                NumericType::I8 => ValueType::I8(*value as i8),
                NumericType::I16 => ValueType::I16(*value as i16),
                NumericType::I32 => ValueType::I32(*value as i32),
                NumericType::I64 => ValueType::I64(*value),

                // implement numerical lowering
                NumericType::I128 => ValueType::I128(*value as i128),
                NumericType::ISize => ValueType::ISize(*value as isize),

                NumericType::U8 => ValueType::U8(*value as u8),
                NumericType::U16 => ValueType::U16(*value as u16),
                NumericType::U32 => ValueType::U32(*value as u32),
                NumericType::U64 => ValueType::U64(*value as u64),

                // implement numerical lowering
                NumericType::U128 => ValueType::U128(*value as u128),
                NumericType::USize => ValueType::USize(*value as usize),

                // fix??
                NumericType::F32 => ValueType::F32(*value as f32),
                NumericType::F64 => ValueType::F64(*value as f64),
            })),

            Expr::Float(value, ty) => Ok(val!(match ty.to_owned().unwrap_or(NumericType::F64) {
                NumericType::F32 => ValueType::F32(*value as f32),
                NumericType::F64 => ValueType::F64(*value),

                _ => unreachable!(), // cannot hit this
            })),

            Expr::Tuple(expressions) => {
                let mut values = Vec::new();
                for expr in expressions {
                    values.push(self.evaluate_expression(expr)?);
                }
                Ok(val!(ValueType::Tuple(values)))
            }

            Expr::Identifier(name) => {
                if let Some((_scope_index, value)) = self.env.find_variable(name) {
                    Ok(value.clone())
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }

            Expr::Block { statements, value, .. } => {
                self.env.enter_scope();

                for stmt in statements {
                    let result = self.execute_statement(stmt)?;
                    if matches!(result.borrow().inner(), ValueType::Return(_) | ValueType::Break(_, _) | ValueType::Continue(_)) {
                        self.env.exit_scope();
                        return Ok(result);
                    }
                }

                let result = match value {
                    Some(expr) => self.evaluate_expression(expr)?,
                    None => ValueEnum::unit(),
                };

                self.env.exit_scope();
                Ok(result)
            }

            // make the code DRY
            Expr::Dereference { operand } => match &**operand {
                Expr::MethodCall { object, method, arguments } => {
                    let obj_value = self.evaluate_expression(object)?;
                    self.evaluate_method_call(obj_value, method, arguments)
                }

                Expr::Assignment { target, value } => match target.as_ref() {
                    Expr::Identifier(identifier) => {
                        let new_value = self.evaluate_expression(value)?;
                        let var_ref = self.env.get_variable(identifier).ok_or_else(|| format!("Variable '{}' not found", identifier))?;

                        let (source_name, source_scope) = {
                            let borrow = var_ref.borrow();
                            match borrow.inner() {
                                ValueType::Reference { source_name, source_scope, .. } => {
                                    if !var_ref.borrow().is_mutable() {
                                        return Err(format!("Cannot assign through immutable reference '{}'", identifier));
                                    }
                                    (
                                        source_name.clone().ok_or_else(|| format!("Reference for '{}' is missing its source name", identifier))?,
                                        source_scope.clone().ok_or_else(|| format!("Reference for '{}' is missing its source scope", identifier))?,
                                    )
                                }
                                _ => return Err(format!("Variable '{}' is not a reference", identifier)),
                            }
                        };

                        self.env.update_scoped_variable(&source_name, new_value, source_scope)?;
                        Ok(ValueEnum::unit())
                    }

                    Expr::MemberAccess { object, member } => {
                        let new_value = self.evaluate_expression(value)?;
                        let ref_value = self.evaluate_expression(object)?;

                        if let Ok(index) = member.parse::<usize>() {
                            if !matches!(ref_value.borrow().inner(), ValueType::Reference { .. }) {
                                return Err("Cannot dereference non-reference value".to_string());
                            }
                            if !ref_value.borrow().is_mutable() {
                                return Err("Cannot dereference immutable reference".to_string());
                            }

                            let target_ptr = {
                                let borrow = ref_value.borrow();
                                match borrow.inner() {
                                    ValueType::Reference { original_ptr, .. } => {
                                        if original_ptr.is_null() {
                                            return Err("Reference contains null pointer".to_string());
                                        }
                                        original_ptr
                                    }
                                    _ => return Err("Invalid reference".to_string()),
                                }
                            };

                            let target_inner = unsafe {
                                let cell_ref = &*target_ptr;
                                cell_ref.borrow().inner()
                            };

                            match target_inner {
                                ValueType::Tuple(elements) => {
                                    if index < elements.len() {
                                        return Ok(elements[index].clone());
                                    } else {
                                        return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                                    }
                                }
                                _ => return Err("Cannot access tuple element through non-tuple reference".to_string()),
                            }
                        }

                        if !matches!(ref_value.borrow().inner(), ValueType::Reference { .. }) {
                            return Err("Cannot dereference non-reference value".to_string());
                        }
                        if !ref_value.borrow().is_mutable() {
                            return Err("Cannot assign through immutable reference".to_string());
                        }

                        let target_ptr = {
                            let borrow = ref_value.borrow();
                            match borrow.inner() {
                                ValueType::Reference { original_ptr, .. } => {
                                    if original_ptr.is_null() {
                                        return Err("Reference contains null pointer".to_string());
                                    }
                                    original_ptr
                                }
                                _ => return Err("Invalid reference".to_string()),
                            }
                        };

                        let target_inner = unsafe {
                            let cell_ref = &*target_ptr;
                            cell_ref.borrow().inner()
                        };

                        match target_inner {
                            ValueType::Struct { fields, .. } => {
                                let field_ref = fields.get(member).ok_or_else(|| format!("Field '{}' not found", member))?;
                                *field_ref.borrow_mut() = new_value.borrow().clone();
                                Ok(ValueEnum::unit())
                            }
                            _ => Err("Cannot assign to field of non-struct reference".to_string()),
                        }
                    }

                    _ => Err("Invalid assignment target".to_string()),
                },

                Expr::CompoundAssignment { target, operator, value } => {
                    let identifier = if let Expr::Identifier(name) = target.as_ref() {
                        name.clone()
                    } else {
                        return Err("Invalid assignment target".to_string());
                    };

                    let right_val = self.evaluate_expression(value)?;

                    let ref_info = if let Some(symbol_info) = self.env.scope_resolver.resolve(&identifier) {
                        if let Some((scope_index, current_value)) = self.env.find_variable(&identifier) {
                            match current_value.borrow().inner() {
                                ValueType::Reference { source_name, source_scope, .. } => {
                                    if !current_value.borrow().is_mutable() {
                                        return Err(format!("Cannot assign to immutable reference '{}'", identifier));
                                    }

                                    let source_name = source_name.clone().ok_or_else(|| format!("Reference for '{}' is missing its source name", identifier))?;

                                    let source_scope = source_scope.clone().ok_or_else(|| format!("Reference for '{}' is missing its source scope", identifier))?;

                                    if let Some(scope) = self.env.scopes.get(source_scope) {
                                        if let Some(inner_value) = scope.get(&source_name) {
                                            Some(Either::Left((source_name, source_scope, inner_value.clone())))
                                        } else {
                                            return Err(format!("Reference source '{}' not found", source_name));
                                        }
                                    } else {
                                        return Err(format!("Reference scope {} not found", source_scope));
                                    }
                                }
                                _ => {
                                    if !symbol_info.mutable && !matches!(current_value.borrow().inner(), ValueType::Unit) {
                                        return Err(format!("Cannot assign to immutable variable '{}'", identifier));
                                    }
                                    Some(Either::Right((scope_index, current_value.clone())))
                                }
                            }
                        } else {
                            return Err(format!("Variable '{}' not found", identifier));
                        }
                    } else {
                        return Err(format!("Variable '{}' not found", identifier));
                    };

                    let result_value = match &ref_info {
                        Some(Either::Left((_, _, left_val))) => self.evaluate_compound_assignment(left_val, operator, &right_val)?,
                        Some(Either::Right((_, left_val))) => self.evaluate_compound_assignment(left_val, operator, &right_val)?,
                        None => return Err("Invalid reference".to_string()),
                    };

                    match ref_info {
                        Some(Either::Left((ref_source_name, ref_source_scope, _))) => {
                            self.env.update_scoped_variable(&ref_source_name, result_value, ref_source_scope)?;
                        }
                        Some(Either::Right((scope_index, _))) => {
                            self.env.update_scoped_variable(&identifier, result_value, scope_index)?;
                        }
                        None => return Err("Invalid reference".to_string()),
                    }

                    Ok(ValueEnum::unit())
                }

                other => {
                    let reference = self.evaluate_expression(other)?;

                    let reference_inner = {
                        let borrowed = reference.borrow();
                        borrowed.inner()
                    };

                    match reference_inner {
                        ValueType::Reference {
                            source_name: Some(name),
                            source_scope: Some(scope_index),
                            ..
                        } => {
                            if let Some(scope) = self.env.scopes.get(scope_index) {
                                if let Some(value) = scope.get(&name) {
                                    Ok(value.clone())
                                } else {
                                    Err(format!("Variable '{}' not found in scope {}", name, scope_index))
                                }
                            } else {
                                Err(format!("Scope {} not found", scope_index))
                            }
                        }

                        ValueType::Reference { original_ptr, .. } => {
                            if original_ptr.is_null() {
                                Err("Reference contains null pointer".to_string())
                            } else {
                                unsafe { Ok(Rc::from_raw(original_ptr)) }
                            }
                        }

                        ValueType::Pointer(pointer) => {
                            if pointer.is_null() {
                                Err("Reference contains null pointer".to_string())
                            } else {
                                unsafe { Ok(Rc::from_raw(pointer)) }
                            }
                        }

                        _ => Err("Cannot dereference a non-reference value".to_string()),
                    }
                }
            },

            Expr::Reference { mutable, operand } => match &**operand {
                Expr::Identifier(name) => {
                    if let Some((scope_index, existing)) = self.env.find_variable(name) {
                        if matches!(existing.borrow().inner(), ValueType::Reference { .. }) {
                            return Ok(existing.clone());
                        }

                        let reference = ValueType::Reference {
                            source_name: Some(name.clone()),
                            source_scope: Some(scope_index),
                            original_ptr: Rc::as_ptr(existing),
                            _undropped: existing.clone(),
                        };

                        Ok(if *mutable { val!(mut reference) } else { val!(reference) })
                    } else {
                        Err(format!("Variable '{}' not found", name))
                    }
                }

                other => {
                    let value = self.evaluate_expression(&*other)?;
                    if matches!(value.borrow().inner(), ValueType::Reference { .. }) {
                        return Ok(value.clone());
                    }

                    let reference = ValueType::Reference {
                        source_name: None,
                        source_scope: None,
                        original_ptr: Rc::as_ptr(&value),
                        _undropped: value.clone(),
                    };

                    Ok(if *mutable { val!(mut reference) } else { val!(reference) })
                }
            },

            Expr::Binary { left, operator, right } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;

                let result = impl_binary_ops!(
                    (left_val, operator, right_val),
                    [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize, F32, F64],
                    [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize],
                    [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize, F32, F64, Str, Pointer]
                );

                if result.is_ok() {
                    return result;
                }

                if let Ok(promoted) = impl_promote_to_type! {
                    (right_val, left_val),
                    (I16, i16),
                    (U16, u16),
                    (I32, i32),
                    (U32, u32),
                    (I64, i64),
                    (U64, u64),
                    (ISize, isize),
                    (USize, usize)
                } {
                    return impl_binary_ops! {
                        (left_val, operator, promoted),
                        [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize, F32, F64],
                        [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize],
                        [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize, F32, F64, Str, Pointer]
                    };
                }

                if let Ok(promoted) = impl_promote_to_type! {
                    (left_val, right_val),
                    (I16, i16),
                    (U16, u16),
                    (I32, i32),
                    (U32, u32),
                    (I64, i64),
                    (U64, u64),
                    (ISize, isize),
                    (USize, usize)
                } {
                    return impl_binary_ops! {
                        (promoted, operator, right_val),
                        [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize, F32, F64],
                        [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize],
                        [I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, ISize, USize, F32, F64, Str, Pointer]
                    };
                }

                match operator {
                    Token::Equals => match value_equals(&left_val, &right_val) {
                        Ok(equals) => return Ok(val!(ValueType::Boolean(equals))),
                        Err(err) => return Err(err),
                    },

                    Token::NotEquals => match value_equals(&left_val, &right_val) {
                        Ok(equals) => return Ok(val!(ValueType::Boolean(!equals))),
                        Err(err) => return Err(err),
                    },

                    Token::LeftAngle => match compare_values(&left_val, &right_val) {
                        Ok(ordering) => return Ok(val!(ValueType::Boolean(ordering == Ordering::Less))),
                        Err(err) => return Err(err),
                    },

                    Token::RightAngle => match compare_values(&left_val, &right_val) {
                        Ok(ordering) => return Ok(val!(ValueType::Boolean(ordering == Ordering::Greater))),
                        Err(err) => return Err(err),
                    },

                    Token::LessEquals => match compare_values(&left_val, &right_val) {
                        Ok(ordering) => return Ok(val!(ValueType::Boolean(ordering != Ordering::Greater))),
                        Err(err) => return Err(err),
                    },

                    Token::GreaterEquals => match compare_values(&left_val, &right_val) {
                        Ok(ordering) => return Ok(val!(ValueType::Boolean(ordering != Ordering::Less))),
                        Err(err) => return Err(err),
                    },

                    _ => {}
                }

                Err(format!("Cannot perform operation between {} and {}", left_val.borrow(), right_val.borrow()))
            }

            Expr::Unary { operator, operand } => {
                let operand_value = self.evaluate_expression(operand)?;

                match operator {
                    Token::Not => {
                        let operand_inner = operand_value.borrow().inner();
                        let bool_value = operand_inner.to_bool()?;
                        Ok(val!(ValueType::Boolean(!bool_value)))
                    }

                    Token::BitNot => {
                        let operand_inner = operand_value.borrow().inner();
                        match operand_inner {
                            ValueType::I8(v) => Ok(val!(ValueType::I8(!v))),
                            ValueType::I16(v) => Ok(val!(ValueType::I16(!v))),
                            ValueType::I32(v) => Ok(val!(ValueType::I32(!v))),
                            ValueType::I64(v) => Ok(val!(ValueType::I64(!v))),
                            ValueType::I128(v) => Ok(val!(ValueType::I128(!v))),
                            ValueType::ISize(v) => Ok(val!(ValueType::ISize(!v))),
                            ValueType::U8(v) => Ok(val!(ValueType::U8(!v))),
                            ValueType::U16(v) => Ok(val!(ValueType::U16(!v))),
                            ValueType::U32(v) => Ok(val!(ValueType::U32(!v))),
                            ValueType::U64(v) => Ok(val!(ValueType::U64(!v))),
                            ValueType::U128(v) => Ok(val!(ValueType::U128(!v))),
                            ValueType::USize(v) => Ok(val!(ValueType::USize(!v))),
                            _ => Err("Bitwise NOT (~) can only be applied to integer values".to_string()),
                        }
                    }

                    _ => Err(format!("Unsupported unary operator: {}", operator)),
                }
            }

            Expr::If { condition, then_branch, else_branch } => {
                let cond_value = self.evaluate_expression(condition)?;

                let cond_bool = {
                    let borrowed = cond_value.borrow();
                    borrowed.inner().to_bool()?
                };

                if cond_bool {
                    self.evaluate_expression(then_branch)
                } else if let Some(else_expr) = else_branch {
                    self.evaluate_expression(else_expr)
                } else {
                    Ok(ValueEnum::unit())
                }
            }

            Expr::IfLet {
                pattern,
                value,
                then_branch,
                else_branch,
            } => {
                let match_value = self.evaluate_expression(value)?;

                if self.pattern_matches(pattern, &match_value)? {
                    self.evaluate_expression(then_branch)
                } else if let Some(else_expr) = else_branch {
                    self.evaluate_expression(else_expr)
                } else {
                    Ok(ValueEnum::unit())
                }
            }

            Expr::Match { value, arms } => {
                let match_value = self.evaluate_expression(value)?;

                for arm in arms {
                    if self.pattern_matches(&arm.pattern, &match_value)? {
                        if let Some(guard) = &arm.guard {
                            if !self.evaluate_guard(guard)? {
                                continue;
                            }
                        }
                        return self.evaluate_expression(&arm.body);
                    }
                }

                Err("No matching pattern found".to_string())
            }

            Expr::Array(elements) => {
                let mut evaluated_elements = Vec::new();
                let mut element_type = None;

                for element in elements {
                    let value = self.evaluate_expression(element)?;

                    if element_type.is_none() {
                        let borrowed = value.borrow();
                        // check to only allow matched elements
                        element_type = Some(borrowed.inner());
                    }

                    evaluated_elements.push(value);
                }

                let len = evaluated_elements.len();
                let element_type_box = Box::new(element_type.unwrap_or(ValueType::Unit));

                Ok(val!(ValueType::Array {
                    len,
                    ty: element_type_box,
                    el: evaluated_elements,
                }))
            }

            Expr::ArrayRepeat { value, count } => {
                let value_val = self.evaluate_expression(value)?;
                let count_val = self.evaluate_expression(count)?;

                let count_int = match count_val.borrow().inner() {
                    ValueType::I8(i) => i as usize,
                    ValueType::I16(i) => i as usize,
                    ValueType::I32(i) => i as usize,
                    ValueType::I64(i) => i as usize,
                    ValueType::I128(i) => i as usize,
                    ValueType::ISize(i) => i as usize,
                    ValueType::U8(i) => i as usize,
                    ValueType::U16(i) => i as usize,
                    ValueType::U32(i) => i as usize,
                    ValueType::U64(i) => i as usize,
                    ValueType::U128(i) => i as usize,
                    ValueType::USize(i) => i,
                    _ => return Err("Array repeat count must be an integer".to_string()),
                };

                if count_int > 10000 {
                    return Err(format!("Array size too large: {}", count_int));
                }

                let mut elements = Vec::with_capacity(count_int);
                let element_type = {
                    let borrowed = value_val.borrow();
                    borrowed.inner()
                };

                for _ in 0..count_int {
                    elements.push(value_val.clone());
                }

                Ok(val!(ValueType::Array {
                    len: count_int,
                    ty: Box::new(element_type),
                    el: elements,
                }))
            }

            // allow to work with references
            // allow assignment
            Expr::Index { array, index } => {
                let array_value = self.evaluate_expression(array)?;

                if let Expr::Range { ref start, ref end, ref inclusive } = **index {
                    let start_idx = if let Some(start_expr) = start {
                        let start_val = self.evaluate_expression(start_expr)?;
                        get_index_as_usize(&start_val)?
                    } else {
                        0
                    };

                    inner_val!(array_value);

                    match array_value {
                        ValueType::Array { el, len, ty } => {
                            let end_idx = if let Some(end_expr) = end {
                                let end_val = self.evaluate_expression(end_expr)?;
                                let mut idx = get_index_as_usize(&end_val)?;
                                if *inclusive {
                                    idx += 1;
                                }
                                idx
                            } else {
                                len
                            };

                            if start_idx > end_idx || end_idx > len {
                                return Err(format!("Invalid range: {}..{} (array length: {})", start_idx, end_idx, len));
                            }

                            let slice_elements: Vec<Value> = el[start_idx..end_idx].to_vec();
                            Ok(val!(ValueType::Slice {
                                ty: Box::new(*ty.clone()),
                                el: slice_elements,
                            }))
                        }

                        ValueType::Slice { el, ty } => {
                            let end_idx = if let Some(end_expr) = end {
                                let end_val = self.evaluate_expression(end_expr)?;
                                let mut idx = get_index_as_usize(&end_val)?;
                                if *inclusive {
                                    idx += 1;
                                }
                                idx
                            } else {
                                el.len()
                            };

                            if start_idx > end_idx || end_idx > el.len() {
                                return Err(format!("Invalid range: {}..{} (slice length: {})", start_idx, end_idx, el.len()));
                            }

                            let slice_elements: Vec<Value> = el[start_idx..end_idx].to_vec();
                            Ok(val!(ValueType::Slice {
                                ty: Box::new(*ty.clone()),
                                el: slice_elements,
                            }))
                        }

                        _ => Err("Cannot slice non-array/slice value".to_string()),
                    }
                } else {
                    let index_value = self.evaluate_expression(index)?;
                    let idx = get_index_as_usize(&index_value)?;

                    inner_val!(array_value);

                    match array_value {
                        ValueType::Array { el, len, .. } => {
                            if idx >= len {
                                return Err(format!("Index out of bounds: {} (array length: {})", idx, len));
                            }
                            Ok(el[idx].clone())
                        }

                        ValueType::Slice { el, .. } => {
                            if idx >= el.len() {
                                return Err(format!("Index out of bounds: {} (slice length: {})", idx, el.len()));
                            }
                            Ok(el[idx].clone())
                        }

                        _ => Err("Cannot index non-array/slice value".to_string()),
                    }
                }
            }

            Expr::Assignment { target, value } => match target.as_ref() {
                Expr::MethodCall { object, method, arguments } => {
                    let obj_value = self.evaluate_expression(object)?;
                    self.evaluate_method_call(obj_value, method, arguments)
                }

                Expr::Identifier(name) => {
                    if let Some(symbol_info) = self.env.scope_resolver.resolve(name) {
                        let can_assign = symbol_info.mutable || !symbol_info.initialized;

                        if !can_assign {
                            return Err(format!("Cannot assign to immutable variable '{}'", name));
                        }

                        let right_val = self.evaluate_expression(value)?;
                        self.env.set_variable(name, right_val)?;
                        Ok(ValueEnum::unit())
                    } else {
                        Err(format!("Variable '{}' not found", name))
                    }
                }

                Expr::MemberAccess { object, member } => {
                    let obj_value = self.evaluate_expression(object)?;

                    if let Ok(index) = member.parse::<usize>() {
                        let obj_value_inner = {
                            let borrowed = obj_value.borrow();
                            borrowed.inner()
                        };

                        match obj_value_inner {
                            ValueType::Tuple(elements) => {
                                if !obj_value.borrow().is_mutable() {
                                    return Err("Cannot modify element of immutable tuple".to_string());
                                }

                                if index >= elements.len() {
                                    return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                                }

                                let right_val = self.evaluate_expression(value)?;
                                *elements[index].borrow_mut() = right_val.borrow().clone();
                                return Ok(ValueEnum::unit());
                            }

                            ValueType::Reference { original_ptr, .. } => {
                                if !obj_value.borrow().is_mutable() {
                                    return Err("Cannot modify element through immutable reference".to_string());
                                }

                                if original_ptr.is_null() {
                                    return Err("Reference contains null pointer".to_string());
                                }

                                unsafe {
                                    let cell_ref = &*original_ptr;
                                    let inner_value = cell_ref.borrow().inner();
                                    if let ValueType::Tuple(elements) = inner_value {
                                        if index >= elements.len() {
                                            return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                                        }

                                        let right_val = self.evaluate_expression(value)?;
                                        *elements[index].borrow_mut() = right_val.borrow().clone();
                                        return Ok(ValueEnum::unit());
                                    } else {
                                        return Err("Cannot assign to element of non-tuple reference".to_string());
                                    }
                                }
                            }
                            _ => return Err("Cannot index non-tuple value".to_string()),
                        }
                    }

                    let obj_value_inner = {
                        let borrowed = obj_value.borrow();
                        borrowed.inner()
                    };

                    match obj_value_inner {
                        ValueType::Struct { fields, .. } => {
                            if !obj_value.borrow().is_mutable() {
                                return Err("Cannot modify field of immutable struct".to_string());
                            }
                            if let Some(field_ref) = fields.get(member) {
                                let right_val = self.evaluate_expression(value)?;
                                *field_ref.borrow_mut() = right_val.borrow().clone();
                                Ok(ValueEnum::unit())
                            } else {
                                Err(format!("Field '{}' not found", member))
                            }
                        }
                        _ => Err("Cannot assign to field of non-struct value".to_string()),
                    }
                }

                Expr::Index { array, index } => {
                    let array_value = self.evaluate_expression(array)?;

                    let array_value_inner = {
                        let borrowed = array_value.borrow();
                        borrowed.inner()
                    };

                    match array_value_inner {
                        ValueType::Array { el, len, .. } => {
                            if !array_value.borrow().is_mutable() {
                                return Err("Cannot modify element of immutable array".to_string());
                            }

                            let index_value = self.evaluate_expression(index)?;
                            let idx = get_index_as_usize(&index_value)?;

                            if idx >= len {
                                return Err(format!("Index out of bounds: {} (length: {})", idx, len));
                            }

                            let right_val = self.evaluate_expression(value)?;
                            *el[idx].borrow_mut() = right_val.borrow().clone();

                            Ok(ValueEnum::unit())
                        }

                        ValueType::Slice { el, .. } => {
                            if !array_value.borrow().is_mutable() {
                                return Err("Cannot modify element of immutable slice".to_string());
                            }

                            let index_value = self.evaluate_expression(index)?;
                            let idx = get_index_as_usize(&index_value)?;

                            if idx >= el.len() {
                                return Err(format!("Index out of bounds: {} (length: {})", idx, el.len()));
                            }

                            let right_val = self.evaluate_expression(value)?;
                            *el[idx].borrow_mut() = right_val.borrow().clone();

                            Ok(ValueEnum::unit())
                        }

                        _ => Err("Cannot index into non-array/slice value".to_string()),
                    }
                }

                _ => Err("Invalid assignment target".to_string()),
            },

            Expr::CompoundAssignment { target, operator, value } => match target.as_ref() {
                Expr::MethodCall { object, method, arguments } => {
                    let obj_value = self.evaluate_expression(object)?;
                    self.evaluate_method_call(obj_value, method, arguments)
                }

                Expr::Identifier(name) => {
                    if let Some(symbol_info) = self.env.scope_resolver.resolve(name) {
                        if !symbol_info.mutable {
                            return Err(format!("Cannot modify immutable variable '{}'", name));
                        }

                        let left_val = self.env.get_variable(name).ok_or_else(|| format!("Variable '{}' not found", name))?.clone();
                        let right_val = self.evaluate_expression(value)?;
                        let result = self.evaluate_compound_assignment(&left_val, operator, &right_val)?;

                        self.env.set_variable(name, result)?;
                        Ok(ValueEnum::unit())
                    } else {
                        Err(format!("Variable '{}' not found", name))
                    }
                }

                Expr::MemberAccess { object, member } => {
                    let obj_value = self.evaluate_expression(object)?;

                    if let Ok(index) = member.parse::<usize>() {
                        let obj_value_inner = {
                            let borrowed = obj_value.borrow();
                            borrowed.inner()
                        };

                        match obj_value_inner {
                            ValueType::Tuple(elements) => {
                                if !obj_value.borrow().is_mutable() {
                                    return Err("Cannot modify element of immutable tuple".to_string());
                                }

                                if index >= elements.len() {
                                    return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                                }

                                let right_val = self.evaluate_expression(value)?;
                                let current_val = elements[index].clone();
                                let result = self.evaluate_compound_assignment(&current_val, operator, &right_val)?;

                                *elements[index].borrow_mut() = result.borrow().clone();
                                return Ok(ValueEnum::unit());
                            }

                            ValueType::Reference { original_ptr, .. } => {
                                if !obj_value.borrow().is_mutable() {
                                    return Err("Cannot modify element through immutable reference".to_string());
                                }

                                if original_ptr.is_null() {
                                    return Err("Reference contains null pointer".to_string());
                                }

                                unsafe {
                                    let cell_ref = &*original_ptr;
                                    let inner_value = cell_ref.borrow().inner();
                                    if let ValueType::Tuple(elements) = inner_value {
                                        if index >= elements.len() {
                                            return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                                        }

                                        let right_val = self.evaluate_expression(value)?;
                                        let current_val = elements[index].clone();
                                        let result = self.evaluate_compound_assignment(&current_val, operator, &right_val)?;

                                        *elements[index].borrow_mut() = result.borrow().clone();
                                        return Ok(ValueEnum::unit());
                                    } else {
                                        return Err("Cannot assign to element of non-tuple reference".to_string());
                                    }
                                }
                            }
                            _ => return Err("Cannot index non-tuple value".to_string()),
                        }
                    }

                    {
                        let borrowed = obj_value.borrow();
                        match borrowed.inner() {
                            ValueType::Struct { fields, .. } => {
                                if !obj_value.borrow().is_mutable() {
                                    return Err("Cannot modify field of immutable struct".to_string());
                                }
                                if !fields.contains_key(member) {
                                    return Err(format!("Field '{}' not found", member));
                                }
                            }
                            _ => return Err("Cannot modify field of non-struct value".to_string()),
                        }
                    }

                    let right_val = self.evaluate_expression(value)?;

                    let field_ref = {
                        let borrowed = obj_value.borrow();
                        if let ValueType::Struct { fields, .. } = borrowed.inner() {
                            fields.get(member).unwrap().clone()
                        } else {
                            unreachable!()
                        }
                    };

                    let current_val = field_ref.clone();
                    let result = self.evaluate_compound_assignment(&current_val, operator, &right_val)?;

                    *field_ref.borrow_mut() = result.borrow().clone();
                    Ok(ValueEnum::unit())
                }

                Expr::Index { array, index } => {
                    let array_value = self.evaluate_expression(array)?;

                    let array_value_inner = {
                        let borrowed = array_value.borrow();
                        borrowed.inner()
                    };

                    match array_value_inner {
                        ValueType::Array { el, len, .. } => {
                            if !array_value.borrow().is_mutable() {
                                return Err("Cannot modify element of immutable array".to_string());
                            }

                            let index_value = self.evaluate_expression(index)?;
                            let idx = get_index_as_usize(&index_value)?;

                            if idx >= len {
                                return Err(format!("Index out of bounds: {} (length: {})", idx, len));
                            }

                            let right_val = self.evaluate_expression(value)?;
                            let current_val = el[idx].clone();
                            let result = self.evaluate_compound_assignment(&current_val, operator, &right_val)?;

                            *el[idx].borrow_mut() = result.borrow().clone();
                            Ok(ValueEnum::unit())
                        }
                        ValueType::Slice { el, .. } => {
                            if !array_value.borrow().is_mutable() {
                                return Err("Cannot modify element of immutable slice".to_string());
                            }

                            let index_value = self.evaluate_expression(index)?;
                            let idx = get_index_as_usize(&index_value)?;

                            if idx >= el.len() {
                                return Err(format!("Index out of bounds: {} (length: {})", idx, el.len()));
                            }

                            let right_val = self.evaluate_expression(value)?;
                            let current_val = el[idx].clone();
                            let result = self.evaluate_compound_assignment(&current_val, operator, &right_val)?;

                            *el[idx].borrow_mut() = result.borrow().clone();
                            Ok(ValueEnum::unit())
                        }
                        _ => Err("Cannot index into non-array/slice value".to_string()),
                    }
                }

                _ => Err("Invalid assignment target".to_string()),
            },

            Expr::Call { function, arguments } => {
                match &**function {
                    // !MODULE SYSTEM!
                    // TEMPORARY
                    Expr::Path(path) if path.segments.len() <= 2 => {
                        // handle import calls (like use std::io, io::write)
                        // handle path-based calls (like std::io::writeln)
                        // TEMPORARY

                        if path.segments[0].ident == "core" && path.segments[1].ident == "concat" {
                            let mut concatenated = String::new();
                            for arg in arguments {
                                let value = self.evaluate_expression(arg)?;
                                concatenated.push_str(&value.borrow().to_string());
                            }
                            return Ok(ValueEnum::new_str(concatenated));
                        }

                        if let [first, second, ..] = path.segments.as_slice() {
                            if first.ident == "core" && second.ident == "escape" {
                                if arguments.len() != 1 {
                                    return Err("core::escape requires exactly one argument".to_string());
                                }

                                let arg = &arguments[0];
                                let value = self.evaluate_expression(arg)?;

                                let s = value.borrow().to_string();
                                let escaped: String = s.chars().flat_map(char::escape_default).collect();

                                return Ok(ValueEnum::new_str(escaped));
                            }

                            if first.ident == "core" && second.ident == "panic" {
                                if arguments.len() != 1 {
                                    return Err("core::panic requires exactly one argument".to_string());
                                }

                                let arg = &arguments[0];
                                let value = self.evaluate_expression(arg)?;

                                return Err(value.borrow().to_string());
                            }

                            if first.ident == "core" && second.ident == "write" {
                                if arguments.len() != 1 {
                                    return Err("core::write requires exactly one argument".to_string());
                                }

                                let arg = &arguments[0];
                                let value = self.evaluate_expression(arg)?;
                                let inner = value.borrow();

                                io::stdout().write_all(&inner.as_bytes()).map_err(|e| e.to_string())?;

                                return Ok(ValueEnum::unit());
                            }
                        }

                        let type_name = &path.segments[0].ident;
                        let method_name = &path.segments[1].ident;

                        // method calling
                        if let Some((_, value)) = self.env.find_variable(type_name) {
                            let methods = {
                                let borrowed = value.borrow();
                                match borrowed.inner() {
                                    ValueType::StructDef { methods, .. } => methods.clone(),
                                    ValueType::EnumDef { methods, .. } => methods.clone(),
                                    _ => return Err(format!("Type '{}' is not a struct or enum definition", type_name)),
                                }
                            };

                            if let Some(method_fn) = methods.get(method_name) {
                                let function = method_fn.clone();

                                let mut evaluated_args = Vec::new();
                                for arg in arguments {
                                    evaluated_args.push(self.evaluate_expression(arg)?);
                                }

                                self.env.enter_scope();
                                let current_scope_index = self.env.get_current_scope();

                                for (i, arg_val) in evaluated_args.iter().enumerate() {
                                    if let Some((param_pattern, _)) = function.params.get(i) {
                                        if let Pattern::Identifier { name, mutable } = param_pattern {
                                            self.env.set_scoped_variable(name, arg_val.clone(), current_scope_index, *mutable)?;
                                        }
                                    }
                                }

                                let result = self.execute(&function.body)?;
                                self.env.exit_scope();

                                return Ok(result);
                            }
                        }

                        let path = format!("{type_name}::{method_name}");
                        if let Some((_, value)) = self.env.find_variable(path.as_str()) {
                            let callable_type = value.borrow().inner();

                            // improve how this is called?
                            return match callable_type {
                                ValueType::EnumConstructor { enum_name, variant_name, fields } => self.handle_enum_constructor_call(arguments, enum_name, variant_name, fields),

                                ValueType::EnumStructConstructor { enum_name, variant_name, fields } => self.handle_enum_struct_call(arguments, enum_name, variant_name, fields),

                                _ => return Err(format!("Undefined function or method: {}::{}", type_name, method_name)),
                            };
                        }

                        return Err(format!("Undefined function or method: {}::{}", type_name, method_name));
                    }

                    Expr::Identifier(name) => {
                        if name == "main" {
                            return Err("main function cannot be called directly".to_string());
                        }

                        let evaluated_args: Result<Vec<Value>, String> = arguments.iter().map(|arg| self.evaluate_expression(arg)).collect();
                        let arg_values = evaluated_args?;

                        let (params, body) = if let Some(Stmt::Function { params, body, .. }) = self.fnc.get(name) {
                            (params.clone(), body.clone())
                        } else {
                            let callable = self.evaluate_expression(function)?;
                            let callable_type = callable.borrow().inner();

                            return match callable_type {
                                ValueType::StructDef { name, fields, .. } => {
                                    if !fields.keys().all(|k| k.parse::<usize>().is_ok()) {
                                        return Err(format!("Regular struct '{name}' cannot be initialized with function-call syntax, use '{{...}}' syntax instead",));
                                    }

                                    if arguments.len() != fields.len() {
                                        return Err(format!("Expected {} arguments for tuple struct '{name}', got {}", fields.len(), arguments.len()));
                                    }

                                    let mut field_values = HashMap::new();
                                    let mut sorted_fields: Vec<_> = fields.iter().collect();

                                    sorted_fields.sort_by_key(|(k, _)| k.parse::<usize>().unwrap());

                                    for ((index, _), arg) in sorted_fields.iter().zip(arguments.iter()) {
                                        let arg_value = self.evaluate_expression(arg)?;
                                        field_values.insert(index.to_string(), arg_value);
                                    }

                                    Ok(val!(ValueType::Struct { name, fields: field_values }))
                                }

                                ValueType::EnumConstructor { enum_name, variant_name, fields } => self.handle_enum_constructor_call(arguments, enum_name, variant_name, fields),

                                ValueType::EnumStructConstructor { enum_name, variant_name, fields } => self.handle_enum_struct_call(arguments, enum_name, variant_name, fields),

                                _ => return Err(format!("Function '{}' not found", name)),
                            };
                        };

                        if arg_values.len() != params.len() {
                            return Err(format!("Function '{}' expects {} arguments but got {}", name, params.len(), arg_values.len()));
                        }

                        self.env.enter_function_scope();

                        for ((param, param_type), value) in params.iter().zip(arg_values) {
                            if let Pattern::Identifier { name, .. } = param {
                                match param_type {
                                    Type::Reference { mutable: ref_mutable, .. } => {
                                        self.env.scope_resolver.declare_reference(name, *ref_mutable);

                                        if *ref_mutable && !value.borrow().is_mutable() {
                                            return Err("Cannot pass immutable reference as mutable".to_string());
                                        }

                                        if let Some(scope) = self.env.scopes.last_mut() {
                                            if let Some(existing) = scope.get(name) {
                                                *existing.borrow_mut() = value.borrow().clone();
                                            } else {
                                                scope.insert(name.to_string(), value.clone());
                                            }
                                        }
                                    }

                                    _ => {
                                        self.env.scope_resolver.declare_variable(name, false);
                                        if let Some(scope) = self.env.scopes.last_mut() {
                                            if let Some(existing) = scope.get(name) {
                                                *existing.borrow_mut() = value.borrow().clone().into_immutable();
                                            } else {
                                                scope.insert(name.to_string(), value.clone());
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        let result = self.execute(&body);
                        self.env.exit_function_scope();

                        result
                    }

                    _ => Err(format!("Unsupported function call type: {:?}", function)),
                }
            }

            // support compound assignment
            Expr::MemberAssignment { object, member, value } => {
                let right_val = self.evaluate_expression(value)?;

                if let Ok(index) = member.parse::<usize>() {
                    let target = self.evaluate_expression(object)?;

                    if !target.borrow().is_mutable() {
                        return Err("Cannot assign to element of immutable tuple".to_string());
                    }

                    let target_inner = {
                        let borrowed = target.borrow();
                        borrowed.inner()
                    };

                    match target_inner {
                        ValueType::Tuple(elements) => {
                            if index < elements.len() {
                                *elements[index].borrow_mut() = right_val.borrow().clone();
                                return Ok(val!(ValueType::Unit));
                            } else {
                                return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                            }
                        }
                        ValueType::Reference { original_ptr, .. } => {
                            if original_ptr.is_null() {
                                return Err("Reference contains null pointer".to_string());
                            }

                            unsafe {
                                let cell_ref = &*original_ptr;
                                let inner_value = cell_ref.borrow().inner();
                                if let ValueType::Tuple(elements) = inner_value {
                                    if index < elements.len() {
                                        *elements[index].borrow_mut() = right_val.borrow().clone();
                                        return Ok(val!(ValueType::Unit));
                                    } else {
                                        return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                                    }
                                } else {
                                    return Err("Cannot assign to element of non-tuple reference".to_string());
                                }
                            }
                        }
                        _ => return Err("Cannot assign to element of non-tuple value".to_string()),
                    }
                }

                match self.extract_field_chain(object) {
                    Ok((base_name, mut chain)) => {
                        if chain.last().map_or(true, |m| m != member) {
                            chain.push(member.clone());
                        }

                        if let Some((_, base_value)) = self.env.find_variable(&base_name) {
                            if let Some(symbol_info) = self.env.scope_resolver.resolve(&base_name) {
                                if !symbol_info.mutable {
                                    return Err(format!("Cannot assign to immutable variable '{}'", base_name));
                                }
                            } else {
                                return Err(format!("Variable '{}' not found", base_name));
                            }

                            let base_inner = base_value.borrow().inner();

                            match base_inner {
                                ValueType::Struct { ref fields, .. } => {
                                    if let Some(field_ref) = get_nested_field_ref(fields, &chain) {
                                        *field_ref.borrow_mut() = right_val.borrow().clone();
                                        Ok(val!(ValueType::Unit))
                                    } else {
                                        Err(format!("Invalid field chain: {:?}", chain))
                                    }
                                }
                                ValueType::Reference { original_ptr, .. } => {
                                    if original_ptr.is_null() {
                                        return Err("Reference contains null pointer".to_string());
                                    }

                                    let result = unsafe {
                                        let cell_ref = &*original_ptr;
                                        let inner_val = cell_ref.borrow();
                                        match inner_val.inner() {
                                            ValueType::Struct { ref fields, .. } => {
                                                let fields_clone = fields.clone();
                                                get_nested_field_ref(&fields_clone, &chain)
                                            }
                                            _ => None,
                                        }
                                    };

                                    if let Some(field_ref) = result {
                                        *field_ref.borrow_mut() = right_val.borrow().clone();
                                        Ok(val!(ValueType::Unit))
                                    } else {
                                        Err("Cannot access field of non-struct value".to_string())
                                    }
                                }
                                _ => Err("Cannot access field of non-struct value".to_string()),
                            }
                        } else {
                            Err(format!("Variable '{}' not found", base_name))
                        }
                    }

                    Err(_) => {
                        // ... other branch handling assignment via a direct evaluation.
                        let target = self.evaluate_expression(object)?;

                        if !target.borrow().is_mutable() {
                            return Err("Cannot assign through immutable reference".to_string());
                        }

                        let target_inner = {
                            let borrowed = target.borrow();
                            borrowed.inner()
                        };

                        match target_inner {
                            ValueType::Struct { ref fields, .. } => {
                                if let Some(field_ref) = fields.get(member) {
                                    *field_ref.borrow_mut() = right_val.borrow().clone();
                                    Ok(val!(ValueType::Unit))
                                } else {
                                    Err(format!("Field '{}' not found", member))
                                }
                            }

                            ValueType::Reference { original_ptr, .. } => {
                                if original_ptr.is_null() {
                                    return Err("Reference contains null pointer".to_string());
                                }

                                unsafe {
                                    let cell_ref = &*original_ptr;
                                    let inner_value = cell_ref.borrow().inner();
                                    if let ValueType::Struct { ref fields, .. } = inner_value {
                                        if let Some(field_ref) = fields.get(member) {
                                            *field_ref.borrow_mut() = right_val.borrow().clone();
                                            Ok(val!(ValueType::Unit))
                                        } else {
                                            Err(format!("Field '{}' not found", member))
                                        }
                                    } else {
                                        Err("Cannot assign to field of non-struct reference".to_string())
                                    }
                                }
                            }
                            _ => Err("Invalid target for member assignment".to_string()),
                        }
                    }
                }
            }

            Expr::MemberAccess { object, member } => {
                let obj_value = self.evaluate_expression(object)?;

                let obj_inner = {
                    let borrowed = obj_value.borrow();
                    borrowed.inner()
                };

                // make this code DRY
                if let Ok(index) = member.parse::<usize>() {
                    match obj_inner {
                        ValueType::Struct { ref fields, .. } => {
                            if fields.keys().all(|k| k.parse::<usize>().is_ok()) {
                                if let Some(value) = fields.get(&index.to_string()) {
                                    return Ok(value.clone());
                                } else {
                                    return Err(format!("Tuple struct index out of bounds: {} (length: {})", index, fields.len()));
                                }
                            }
                        }

                        ValueType::Tuple(elements) => {
                            if index < elements.len() {
                                return Ok(elements[index].clone());
                            } else {
                                return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                            }
                        }

                        ValueType::Enum { data: Some(values), .. } => {
                            if index < values.len() {
                                return Ok(values[index].clone());
                            } else {
                                return Err(format!("Index {} out of bounds for enum variant with {} fields", index, values.len()));
                            }
                        }

                        ValueType::Reference { original_ptr, .. } => {
                            if original_ptr.is_null() {
                                return Err("Reference contains null pointer".to_string());
                            }

                            unsafe {
                                let cell_ref = &*original_ptr;
                                let inner_inner = cell_ref.borrow().inner();

                                match inner_inner {
                                    ValueType::Struct { ref fields, .. } => {
                                        if fields.keys().all(|k| k.parse::<usize>().is_ok()) {
                                            if let Some(value) = fields.get(&index.to_string()) {
                                                return Ok(value.clone());
                                            } else {
                                                return Err(format!("Tuple struct index out of bounds: {} (length: {})", index, fields.len()));
                                            }
                                        }
                                    }

                                    ValueType::Enum { data: Some(values), .. } => {
                                        if index < values.len() {
                                            return Ok(values[index].clone());
                                        } else {
                                            return Err(format!("Index {} out of bounds for enum variant with {} fields", index, values.len()));
                                        }
                                    }

                                    ValueType::Tuple(elements) => {
                                        if index < elements.len() {
                                            return Ok(elements[index].clone());
                                        } else {
                                            return Err(format!("Tuple index out of bounds: {} (length: {})", index, elements.len()));
                                        }
                                    }

                                    _ => return Err("Cannot access numeric index of non-inner reference".to_string()),
                                }
                            }
                        }

                        _ => return Err("Cannot access numeric index of non-inner value".to_string()),
                    }
                }

                let field_ref = match obj_inner {
                    ValueType::Struct { ref fields, .. } => fields.get(member).ok_or_else(|| format!("Field '{}' not found", member))?.clone(),

                    ValueType::Reference { original_ptr, .. } => {
                        if original_ptr.is_null() {
                            return Err("Reference contains null pointer".to_string());
                        }

                        unsafe {
                            let cell_ref = &*original_ptr;
                            let inner_inner = cell_ref.borrow().inner();
                            match inner_inner {
                                ValueType::Struct { ref fields, .. } => fields.get(member).ok_or_else(|| format!("Field '{}' not found", member))?.clone(),
                                _ => return Err("Cannot access member of non-struct reference".to_string()),
                            }
                        }
                    }
                    _ => return Err("Cannot access member of non-struct value".to_string()),
                };

                Ok(field_ref)
            }

            _ => Ok(ValueEnum::unit()),
        }
    }

    fn pattern_matches(&mut self, pattern: &Pattern, value: &Value) -> Result<bool, String> {
        match (pattern, value) {
            (Pattern::Literal(expr), value) => {
                let pattern_value = self.evaluate_expression(expr)?;

                let pattern_inner = {
                    let borrowed = pattern_value.borrow();
                    borrowed.inner()
                };

                let value_inner = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                Ok(pattern_inner == value_inner)
            }

            (Pattern::Path(path), value) => {
                if path.segments.len() != 2 {
                    return Ok(false);
                }

                let enum_name = &path.segments[0].ident;
                let variant_name = &path.segments[1].ident;

                if self.env.get_variable(enum_name).is_none() {
                    return Err(format!("Enum type '{}' not found in pattern matching", enum_name));
                }

                let enum_def = self.env.get_variable(enum_name).unwrap();
                let valid_variant = match enum_def.borrow().inner() {
                    ValueType::EnumDef { variants, .. } => variants.iter().any(|v| match v {
                        EnumVariant::Simple(name) => name == variant_name,
                        EnumVariant::Tuple(name, _) => name == variant_name,
                        EnumVariant::Struct(name, _) => name == variant_name,
                    }),
                    _ => return Err(format!("'{}' is not an enum type", enum_name)),
                };

                if !valid_variant {
                    return Err(format!("Enum '{}' does not have variant '{}'", enum_name, variant_name));
                }

                let inner_value = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                if let ValueType::Enum { variant, data, enum_type } = inner_value {
                    Ok(*enum_name == enum_type && *variant_name == variant && data.as_ref().map_or(true, |v| v.is_empty()))
                } else {
                    Ok(false)
                }
            }

            // simplify pattern matching in ast
            (Pattern::TupleStruct { path, elements }, value) => {
                let value_inner = value.borrow().inner();
                if let ValueType::Enum { variant, data, enum_type } = value_inner {
                    if path.segments.len() == 2 {
                        if path.segments[0].ident == *enum_type && path.segments[1].ident == *variant {
                            match data {
                                Some(values) if elements.len() == values.len() => {
                                    for (element, val) in elements.iter().zip(values.iter()) {
                                        if !self.pattern_matches(element, val)? {
                                            return Ok(false);
                                        }
                                    }
                                    Ok(true)
                                }
                                None if elements.is_empty() => Ok(true),
                                _ => Ok(false),
                            }
                        } else {
                            Ok(false)
                        }
                    } else if path.segments.len() == 1 {
                        if path.segments[0].ident == variant {
                            match data {
                                Some(values) if elements.len() == values.len() => {
                                    for (element, val) in elements.iter().zip(values.iter()) {
                                        if !self.pattern_matches(element, val)? {
                                            return Ok(false);
                                        }
                                    }
                                    Ok(true)
                                }
                                None if elements.is_empty() => Ok(true),
                                _ => Ok(false),
                            }
                        } else {
                            Ok(false)
                        }
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            }

            (Pattern::Tuple(elements), value) => {
                let inner_value = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                match inner_value {
                    ValueType::Tuple(values) => {
                        if elements.len() == values.len() {
                            for (element, field_val) in elements.iter().zip(values.iter()) {
                                if !self.pattern_matches(element, field_val)? {
                                    return Ok(false);
                                }
                            }
                            Ok(true)
                        } else {
                            Ok(false)
                        }
                    }
                    ValueType::Enum { data: Some(values), .. } => {
                        if elements.len() == values.len() {
                            for (element, field_val) in elements.iter().zip(values.iter()) {
                                if !self.pattern_matches(element, field_val)? {
                                    return Ok(false);
                                }
                            }
                            Ok(true)
                        } else {
                            Ok(false)
                        }
                    }
                    _ => Ok(false),
                }
            }

            (Pattern::Struct { path, fields, rest }, value) => {
                let value_inner = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                match value_inner {
                    ValueType::Struct { name, fields: value_fields, .. } => {
                        if path.segments.last().unwrap().ident != *name {
                            return Ok(false);
                        }

                        for (field_name, field_pattern) in fields {
                            if let Some(field_value) = value_fields.get(field_name) {
                                if !self.pattern_matches(field_pattern, field_value)? {
                                    return Ok(false);
                                }
                            } else if !rest {
                                return Ok(false);
                            }
                        }

                        Ok(true)
                    }
                    ValueType::Reference { original_ptr, .. } => {
                        if original_ptr.is_null() {
                            return Ok(false);
                        }

                        unsafe {
                            let rc = Rc::from_raw(original_ptr);
                            let result = self.pattern_matches(pattern, &rc.clone());
                            std::mem::forget(rc);
                            result
                        }
                    }
                    _ => Ok(false),
                }
            }

            (Pattern::Reference { mutable, pattern }, value) => {
                let inner_value = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                match inner_value {
                    ValueType::Reference { original_ptr, .. } => {
                        if *mutable && !value.borrow().is_mutable() {
                            return Ok(false);
                        }
                        if original_ptr.is_null() {
                            Ok(false)
                        } else {
                            unsafe { self.pattern_matches(pattern, &Rc::from_raw(original_ptr)) }
                        }
                    }
                    _ => self.pattern_matches(pattern, value),
                }
            }

            (Pattern::Or(patterns), value) => {
                for pattern in patterns {
                    if self.pattern_matches(pattern, value)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }

            (Pattern::Identifier { name, mutable }, value) => {
                self.env.scope_resolver.declare_variable(name, *mutable);
                self.env.set_variable(name, value.clone())?;
                Ok(true)
            }

            (Pattern::BindingPattern { name, mutable, subpattern }, value) => {
                self.env.scope_resolver.declare_variable(name, *mutable);
                self.env.set_variable(name, value.clone())?;
                self.pattern_matches(subpattern, value)
            }

            (Pattern::Wildcard, _) => Ok(true),
        }
    }

    fn evaluate_guard(&mut self, guard: &Expr) -> Result<bool, String> {
        let eval_value = self.evaluate_expression(guard)?;

        let inner_value = {
            let borrowed = eval_value.borrow();
            borrowed.inner()
        };

        match inner_value {
            ValueType::Boolean(b) => Ok(b),
            _ => Err("Guard expression must evaluate to a boolean".to_string()),
        }
    }
}

fn get_index_as_usize(value: &Value) -> Result<usize, String> {
    match value.borrow().inner() {
        ValueType::I8(i) => Ok(i as usize),
        ValueType::I16(i) => Ok(i as usize),
        ValueType::I32(i) => Ok(i as usize),
        ValueType::I64(i) => Ok(i as usize),
        ValueType::I128(i) => Ok(i as usize),
        ValueType::ISize(i) => Ok(i as usize),
        ValueType::U8(i) => Ok(i as usize),
        ValueType::U16(i) => Ok(i as usize),
        ValueType::U32(i) => Ok(i as usize),
        ValueType::U64(i) => Ok(i as usize),
        ValueType::U128(i) => Ok(i as usize),
        ValueType::USize(i) => Ok(i),
        _ => Err("Array/slice index must be an integer".to_string()),
    }
}

fn get_nested_field_ref(fields: &HashMap<String, Value>, chain: &[String]) -> Option<Value> {
    if chain.is_empty() {
        return None;
    }

    let mut current_ref = fields.get(&chain[0])?.clone();

    for field_name in &chain[1..] {
        let next_ref = {
            let current_value = current_ref.borrow();

            match current_value.inner() {
                ValueType::Struct { fields: next_fields, .. } => {
                    if let Some(next_value) = next_fields.get(field_name) {
                        next_value.clone()
                    } else {
                        return None;
                    }
                }
                _ => {
                    return None;
                }
            }
        };
        current_ref = next_ref;
    }

    Some(current_ref)
}
