mod assign;
mod cast;
mod environment;
mod structs;

use super::value::{Value, ValueEnum, ValueType};
use crate::parser::{ast::*, lexer::*};
use crate::{impl_binary_ops, impl_promote_to_type, val};

use environment::Environment;
use std::collections::HashMap;

enum Either<L, R> {
    Left(L),
    Right(R),
}

pub struct Interpreter {
    env: Environment,
    fnc: HashMap<String, Stmt>,
}

impl Interpreter {
    pub fn new(ast: &Vec<Stmt>) -> Result<Self, String> {
        let fnc = ast
            .iter()
            .filter_map(|stmt| if let Stmt::Function { name, .. } = stmt { Some((name.clone(), stmt.clone())) } else { None })
            .collect();

        let mut interpreter = Self { env: Environment::new(), fnc };

        interpreter.env.scope_resolver.resolve_program(ast)?;
        interpreter.declare_globals(ast)?;

        Ok(interpreter)
    }

    pub fn start_main(&mut self) -> Result<Value, String> {
        let main_func = self.find_function("main").ok_or("No main function found")?;
        let result = self.execute_statement(&main_func.to_owned())?;

        match result.inner() {
            ValueType::Enum { enum_type, variant, data } if enum_type == "Result" => match variant.as_str() {
                "Ok" => match data {
                    Some(values) if !values.is_empty() => Ok(values[0].clone()),
                    _ => Ok(val!(ValueType::Unit)),
                },

                "Err" => match data {
                    Some(values) if !values.is_empty() => Err(values[0].to_string()),
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

            match result.inner() {
                ValueType::Return(val) => return Ok(val.clone()),
                _ => last_value = result,
            }
        }

        Ok(last_value)
    }

    fn find_function(&self, name: &str) -> Option<&Stmt> { self.fnc.get(name) }

    fn declare_globals(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for stmt in statements {
            match stmt {
                Stmt::Const { name, initializer, .. } => {
                    let value = self.evaluate_expression(initializer)?;
                    self.env.scope_resolver.declare_variable(name, false);
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                Stmt::Static { name, initializer, .. } => {
                    let value = self.evaluate_expression(initializer)?;
                    self.env.scope_resolver.declare_variable(name, false);
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                Stmt::Impl { target, items, .. } => {
                    self.handle_impl_block(target, items)?;
                }

                Stmt::Struct { name, fields, .. } => {
                    self.handle_struct_def(name, fields.to_owned())?;
                }

                Stmt::Module { name, body, .. } => {
                    self.env.scope_resolver.declare_module(name)?;
                    self.env.enter_scope();
                    self.declare_globals(body)?;
                    self.env.exit_scope();
                }

                _ => {}
            }
        }
        Ok(())
    }

    fn execute_statement(&mut self, stmt: &Stmt) -> Result<Value, String> {
        match stmt {
            Stmt::ExpressionValue(expr) => self.evaluate_expression(expr),

            Stmt::ExpressionStmt(expr) => {
                self.evaluate_expression(expr)?;
                Ok(ValueEnum::unit())
            }

            Stmt::Return(expr) => match expr {
                Some(e) => Ok(self.evaluate_expression(e)?),
                None => Ok(ValueEnum::unit()),
            },

            Stmt::Module { body, .. } => {
                self.env.enter_scope();
                let result = self.execute(body)?;
                self.env.exit_scope();
                Ok(result)
            }

            Stmt::Let {
                pattern,
                initializer,
                type_annotation,
                ..
            } => {
                let value = if let Some(init) = initializer {
                    let init_value = self.evaluate_expression(init)?;
                    if let Some(target_type) = type_annotation {
                        self.perform_cast(init_value, target_type)?
                    } else {
                        init_value
                    }
                } else {
                    ValueEnum::unit()
                };

                if let Pattern::Identifier { name, mutable } = pattern {
                    self.env.scope_resolver.declare_variable(name, *mutable);
                    let value = if *mutable { value.into_mutable() } else { value.into_immutable() };
                    self.env.set_variable(name, Box::new(value))?;
                }

                Ok(ValueEnum::unit())
            }

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
                self.env.enter_scope();

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

                let return_val = match result.inner() {
                    ValueType::Return(val) => val,
                    _ => result,
                };

                self.env.exit_scope();
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
                        match value.inner() {
                            ValueType::StructDef { methods, .. } => {
                                if let Some(function) = methods.get(method_name) {
                                    return Ok(Box::new(ValueEnum::Immutable(ValueType::StaticMethod {
                                        struct_name: type_name.to_string(),
                                        method: method_name.to_string(),
                                        function: function.clone(),
                                    })));
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

            Expr::StructInit { struct_name, fields } => {
                return self.evaluate_struct_init(struct_name, fields.to_owned());
            }

            Expr::MethodCall { object, method, arguments } => {
                let obj_value = self.evaluate_expression(object)?;
                self.evaluate_method_call(obj_value, method, arguments)
            }

            Expr::Cast { expr, target_type } => {
                let value = self.evaluate_expression(expr)?;
                self.perform_cast(value, target_type)
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

                _ => unreachable!(), // cannot hit this
            })),

            Expr::Float(value, ty) => Ok(val!(match ty.to_owned().unwrap_or(NumericType::F64) {
                NumericType::F32 => ValueType::F32(*value as f32),
                NumericType::F64 => ValueType::F64(*value),

                _ => unreachable!(), // cannot hit this
            })),

            Expr::String(value) => Ok(val!(ValueType::Reference {
                source_name: None,
                source_scope: None,
                data: Some(val!(ValueType::Str(value.to_owned())))
            })),

            Expr::Tuple(expressions) => {
                let mut values = Vec::new();
                for expr in expressions {
                    values.push(self.evaluate_expression(expr)?);
                }
                Ok(val!(ValueType::Tuple(values)))
            }

            Expr::Identifier(name) => {
                if let Some((_, value)) = self.env.find_variable(name) {
                    if matches!(value.inner(), ValueType::Reference { .. }) {
                        return Ok(value.clone());
                    }

                    let is_mutable = self.env.scope_resolver.resolve(name).map(|info| info.mutable).unwrap_or(false);

                    if is_mutable {
                        Ok(Box::new(value.as_ref().clone().into_mutable()))
                    } else {
                        Ok(value.clone())
                    }
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }

            Expr::Block { statements, value, .. } => {
                self.env.enter_scope();

                for stmt in statements {
                    let result = self.execute_statement(stmt)?;
                    if matches!(result.inner(), ValueType::Return(_)) {
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

            Expr::Dereference { operand } => match &**operand {
                Expr::MethodCall { object, method, arguments } => {
                    let obj_value = self.evaluate_expression(object)?;
                    self.evaluate_method_call(obj_value, method, arguments)
                }

                Expr::Assignment { target, value } => match target.as_ref() {
                    Expr::Identifier(identifier) => {
                        let new_value = self.evaluate_expression(value)?;

                        let (source_name, source_scope) = match self.env.get_variable_ref(identifier) {
                            Some(value) => match value.inner() {
                                ValueType::Reference { source_name, source_scope, .. } => {
                                    if !value.is_mutable() {
                                        return Err(format!("Cannot assign through immutable reference '{}'", identifier));
                                    }
                                    (source_name.clone(), source_scope.clone())
                                }
                                _ => return Err(format!("Variable '{}' is not a reference", identifier)),
                            },
                            None => return Err(format!("Variable '{}' not found", identifier)),
                        };

                        self.env.update_scoped_variable(&source_name.expect("HANDLE THIS"), new_value, source_scope.expect("HANDLE THIS"))?;

                        Ok(ValueEnum::unit())
                    }

                    Expr::MemberAccess { object, member } => {
                        let new_value = self.evaluate_expression(value)?;

                        match self.evaluate_expression(object)? {
                            ref_value if matches!(ref_value.inner(), ValueType::Reference { .. }) => {
                                if !ref_value.is_mutable() {
                                    return Err("Cannot assign through immutable reference".to_string());
                                }

                                if let ValueType::Reference { data: Some(mut target), .. } = ref_value.inner() {
                                    match target.inner_mut() {
                                        ValueType::Struct { fields, .. } => {
                                            if let Some(field_value) = fields.get_mut(member) {
                                                *field_value = new_value;
                                                Ok(ValueEnum::unit())
                                            } else {
                                                Err(format!("Field '{}' not found", member))
                                            }
                                        }
                                        _ => Err("Cannot assign to field of non-struct reference".to_string()),
                                    }
                                } else {
                                    Err("Reference contains no value".to_string())
                                }
                            }
                            _ => Err("Cannot dereference non-reference value".to_string()),
                        }
                    }
                    _ => Err("Invalid assignment target".to_string()),
                },

                Expr::CompoundAssignment { target, operator, value } => {
                    let identifier = if let Expr::Identifier(ref name) = target.as_ref() {
                        name.clone()
                    } else {
                        return Err("Invalid assignment target".to_string());
                    };

                    let right_val = self.evaluate_expression(value)?;

                    let ref_info = if let Some(symbol_info) = self.env.scope_resolver.resolve(&identifier) {
                        if let Some((scope_index, current_value)) = self.env.find_variable(&identifier) {
                            match current_value.inner() {
                                ValueType::Reference { source_name, source_scope, .. } => {
                                    if !current_value.is_mutable() {
                                        return Err(format!("Cannot assign to immutable reference '{}'", identifier));
                                    }
                                    if let Some(scope) = self.env.scopes.get(source_scope.expect("HANDLE THIS")) {
                                        if let Some(inner_value) = scope.get(&source_name.clone().expect("HANDLE THIS")) {
                                            Some(Either::Left((source_name.clone(), source_scope, inner_value.clone())))
                                        } else {
                                            return Err(format!("Reference source '{}' not found", source_name.clone().expect("HANDLE THIS")));
                                        }
                                    } else {
                                        return Err(format!("Reference scope {} not found", source_scope.expect("HANDLE THIS")));
                                    }
                                }
                                _ => {
                                    if !symbol_info.mutable && !matches!(current_value.inner(), ValueType::Unit) {
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
                            self.env
                                .update_scoped_variable(&ref_source_name.expect("HANDLE THIS"), result_value, ref_source_scope.expect("HANDLE THIS"))?;
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

                    match reference.inner() {
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

                        ValueType::Reference { data: Some(inner_value), .. } => Ok(inner_value.clone()),

                        _ => Err("Cannot dereference a non-reference value".to_string()),
                    }
                }
            },

            Expr::Reference { mutable, operand } => match &**operand {
                Expr::Identifier(name) => {
                    if let Some((scope_index, existing)) = self.env.find_variable(name) {
                        let reference = match existing.inner() {
                            ValueType::Reference { source_name, source_scope, data, .. } => ValueType::Reference {
                                source_scope,
                                source_name: source_name.clone(),
                                data: data.clone(),
                            },
                            _ => ValueType::Reference {
                                source_name: Some(name.clone()),
                                source_scope: Some(scope_index),
                                data: None,
                            },
                        };

                        Ok(if *mutable { val!(mut reference) } else { val!(reference) })
                    } else {
                        Err(format!("Variable '{}' not found", name))
                    }
                }

                other => {
                    let value = self.evaluate_expression(&*other)?;

                    match value.inner() {
                        ValueType::Reference { ref source_name, source_scope, .. } => {
                            if *mutable && !value.is_mutable() {
                                return Err("Cannot create a mutable reference to an immutable value".to_string());
                            }

                            let reference = ValueType::Reference {
                                source_name: source_name.clone(),
                                source_scope,
                                data: Some(value),
                            };

                            Ok(if *mutable { val!(mut reference) } else { val!(reference) })
                        }

                        _ => {
                            let reference = ValueType::Reference {
                                source_name: None,
                                source_scope: None,
                                data: Some(value),
                            };

                            Ok(if *mutable { val!(mut reference) } else { val!(reference) })
                        }
                    }
                }
            },

            Expr::Binary { left, operator, right } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;

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
                        I8, U8, I16, U16, I32, U32, F32,
                        I64, U64, F64, I128, U128,
                        ISize, USize
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
                        I8, U8, I16, U16, I32, U32, F32,
                        I64, U64, F64, I128, U128,
                        ISize, USize
                    };
                }
                Err(format!("Cannot perform operation between {:?} and {:?}", left_val, right_val))
            }

            Expr::If { condition, then_branch, else_branch } => {
                let cond_value = self.evaluate_expression(condition)?;

                match cond_value.inner() {
                    ValueType::Boolean(true) => self.evaluate_expression(then_branch),
                    ValueType::Boolean(false) => {
                        if let Some(else_expr) = else_branch {
                            self.evaluate_expression(else_expr)
                        } else {
                            Ok(ValueEnum::unit())
                        }
                    }
                    _ => Err("Condition must be boolean".to_string()),
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

            Expr::Assignment { target, value } => match target.as_ref() {
                Expr::MethodCall { object, method, arguments } => {
                    let obj_value = self.evaluate_expression(object)?;
                    self.evaluate_method_call(obj_value, method, arguments)
                }

                Expr::Identifier(name) => {
                    if let Some(symbol_info) = self.env.scope_resolver.resolve(name) {
                        if !symbol_info.mutable {
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
                    let mut obj_value = self.evaluate_expression(object)?;

                    if !obj_value.is_mutable() {
                        return Err("Cannot modify field of immutable struct".to_string());
                    }

                    match obj_value.inner_mut() {
                        ValueType::Struct { fields, .. } => {
                            if let Some(field_value) = fields.get_mut(member) {
                                let right_val = self.evaluate_expression(value)?;
                                *field_value = right_val;
                                Ok(ValueEnum::unit())
                            } else {
                                Err(format!("Field '{}' not found", member))
                            }
                        }
                        _ => Err("Cannot assign to field of non-struct value".to_string()),
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
                    let mut obj_value = self.evaluate_expression(object)?;

                    if !obj_value.is_mutable() {
                        return Err("Cannot modify field of immutable struct".to_string());
                    }

                    match obj_value.inner_mut() {
                        ValueType::Struct { fields, .. } => {
                            if let Some(field_value) = fields.get_mut(member) {
                                let right_val = self.evaluate_expression(value)?;
                                let result = self.evaluate_compound_assignment(&field_value, operator, &right_val)?;
                                *field_value = result;
                                Ok(ValueEnum::unit())
                            } else {
                                Err(format!("Field '{}' not found", member))
                            }
                        }
                        _ => Err("Cannot modify field of non-struct value".to_string()),
                    }
                }

                _ => Err("Invalid assignment target".to_string()),
            },

            // !MODULE SYSTEM!
            // TEMPORARY
            Expr::Call { function, arguments } => {
                match &**function {
                    Expr::Path(path) if path.segments.len() == 2 => {
                        // handle import calls (like use std::io, io::println)
                        // handle path-based calls (like std::io::println)
                        // TEMPORARY
                        if path.segments[0].ident == "io" && path.segments[1].ident == "println" {
                            if let Some(arg) = arguments.first() {
                                let value = self.evaluate_expression(arg)?;
                                println!("{value}");
                                return Ok(ValueEnum::unit());
                            } else {
                                return Err("io::println requires an argument".to_string());
                            }
                        }

                        let type_name = &path.segments[0].ident;
                        let method_name = &path.segments[1].ident;

                        if let Some((scope_idx, value)) = self.env.find_variable(type_name) {
                            if let ValueType::StructDef { methods, .. } = value.inner() {
                                if let Some(method_fn) = methods.get(method_name) {
                                    let function = method_fn.clone();

                                    let mut evaluated_args = Vec::new();
                                    for arg in arguments {
                                        evaluated_args.push(self.evaluate_expression(arg)?);
                                    }

                                    self.env.enter_scope();

                                    for (i, value) in evaluated_args.iter().enumerate() {
                                        if let Some((param_pattern, _)) = function.params.get(i) {
                                            if let Pattern::Identifier { name, mutable } = param_pattern {
                                                self.env.set_scoped_variable(name, value.clone(), scope_idx, *mutable)?;
                                            }
                                        }
                                    }

                                    let result = self.execute(&function.body)?;
                                    self.env.exit_scope();

                                    return Ok(result);
                                }
                            }
                        }

                        let values: Vec<Value> = arguments.iter().map(|arg| self.evaluate_expression(arg)).collect::<Result<_, _>>()?;

                        Ok(val!(ValueType::Enum {
                            enum_type: type_name.clone(),
                            variant: method_name.clone(),
                            data: if values.is_empty() { None } else { Some(values) },
                        }))
                    }

                    Expr::Identifier(name) => {
                        if name == "main" {
                            return Err("main function cannot be called directly".to_string());
                        }

                        let evaluated_args: Result<Vec<Value>, String> = arguments.iter().map(|arg| self.evaluate_expression(arg)).collect();
                        let arg_values = evaluated_args?;

                        let (params, body) = if let Some(Stmt::Function { params, body, .. }) = self.find_function(name) {
                            (params.clone(), body.clone())
                        } else {
                            return Err(format!("Function '{}' not found", name));
                        };

                        if arg_values.len() != params.len() {
                            return Err(format!("Function '{}' expects {} arguments but got {}", name, params.len(), arg_values.len()));
                        }

                        let scope_depth = self.env.scopes.len();
                        self.env.enter_scope();

                        for ((param, param_type), value) in params.iter().zip(arg_values) {
                            if let Pattern::Identifier { name, .. } = param {
                                match param_type {
                                    Type::Reference { mutable: ref_mutable, .. } => {
                                        self.env.scope_resolver.declare_reference(name, *ref_mutable);
                                        match value.inner() {
                                            ValueType::Reference { source_name, source_scope, data, .. } => {
                                                let reference = ValueType::Reference {
                                                    source_scope,
                                                    source_name: source_name.clone(),
                                                    data: data.clone(),
                                                };

                                                if *ref_mutable && !value.is_mutable() {
                                                    return Err("Cannot pass immutable reference as mutable".to_string());
                                                }

                                                let ref_value = if *ref_mutable && value.is_mutable() {
                                                    val! { mut reference }
                                                } else {
                                                    val! { reference }
                                                };

                                                if let Some(scope) = self.env.scopes.last_mut() {
                                                    scope.insert(name.to_string(), ref_value);
                                                }
                                            }
                                            _ => return Err("Expected a reference value".to_string()),
                                        }
                                    }
                                    _ => {
                                        self.env.scope_resolver.declare_variable(name, false);
                                        let value_copy = value.clone().into_immutable();
                                        self.env.set_variable(name, Box::new(value_copy))?;
                                    }
                                }
                            }
                        }

                        let result = self.execute(&body);

                        while self.env.scopes.len() > scope_depth {
                            self.env.exit_scope();
                        }

                        result
                    }

                    _ => Err(format!("Unsupported function call type: {:?}", function)),
                }
            }

            // support compound assignment
            Expr::MemberAssignment { object, member, value } => {
                let right_val = self.evaluate_expression(value)?;

                match self.extract_field_chain(object) {
                    Ok((base_name, mut chain)) => {
                        if chain.last().map_or(true, |m| m != member) {
                            chain.push(member.clone());
                        }
                        if let Some((scope_index, base_value)) = self.env.find_variable(&base_name) {
                            // check that the variable is mutable (move to struct?)
                            if let Some(symbol_info) = self.env.scope_resolver.resolve(&base_name) {
                                if !symbol_info.mutable {
                                    return Err(format!("Cannot assign to immutable variable '{}'", base_name));
                                }
                            } else {
                                return Err(format!("Variable '{}' not found", base_name));
                            }

                            let mut updated_base = base_value.clone();

                            updated_base.set_struct_field(&chain, right_val)?;
                            self.env.update_scoped_variable(&base_name, updated_base, scope_index)?;
                            Ok(ValueEnum::unit())
                        } else {
                            Err(format!("Variable '{}' not found", base_name))
                        }
                    }

                    Err(_) => {
                        let mut evaluated = self.evaluate_expression(object)?;

                        if !evaluated.is_mutable() {
                            return Err("Cannot assign through immutable reference".to_string());
                        }

                        match evaluated.inner_mut() {
                            ValueType::Reference {
                                data: Some(ref mut boxed_value),
                                source_name: Some(ref source_name),
                                source_scope: Some(scope),
                                ..
                            } => match boxed_value.inner_mut() {
                                ValueType::Struct { fields, .. } => {
                                    if let Some(field_value) = fields.get_mut(member) {
                                        *field_value = right_val;
                                        self.env.update_scoped_variable(source_name, boxed_value.clone(), *scope)?;
                                        Ok(ValueEnum::unit())
                                    } else {
                                        Err(format!("Field '{}' not found", member))
                                    }
                                }
                                _ => Err("Cannot assign to field of non-struct reference".to_string()),
                            },
                            _ => Err("Invalid target for member assignment".to_string()),
                        }
                    }
                }
            }

            Expr::MemberAccess { object, member } => {
                let obj_value = self.evaluate_expression(object)?;
                match obj_value.inner() {
                    ValueType::Struct { fields, .. } => fields.get(member).cloned().ok_or_else(|| format!("Field '{}' not found", member)),

                    ValueType::Reference { data: Some(ref boxed_value), .. } => match boxed_value.inner() {
                        ValueType::Struct { fields, .. } => {
                            if let Some(field_value) = fields.get(member) {
                                if obj_value.is_mutable() {
                                    Ok(val!(mut ValueType::Reference {
                                        source_name: None,
                                        source_scope: None,
                                        data: Some(field_value.clone()),
                                    }))
                                } else {
                                    Ok(field_value.clone())
                                }
                            } else {
                                Err(format!("Field '{}' not found", member))
                            }
                        }
                        _ => Err("Cannot access member of non-struct reference".to_string()),
                    },
                    _ => Err("Cannot access member of non-struct value".to_string()),
                }
            }

            _ => Ok(ValueEnum::unit()),
        }
    }

    fn pattern_matches(&mut self, pattern: &Pattern, value: &Value) -> Result<bool, String> {
        match (pattern, value) {
            (Pattern::Literal(expr), value) => {
                let pattern_value = self.evaluate_expression(expr)?;
                Ok(pattern_value.inner() == value.inner())
            }

            (Pattern::Path(path), value) => {
                if let ValueType::Enum { variant, data, enum_type } = value.inner() {
                    if path.segments.len() == 2 {
                        Ok(path.segments[0].ident == *enum_type && path.segments[1].ident == *variant && data.as_ref().map_or(true, |v| v.is_empty()))
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            }

            (Pattern::TupleStruct { path, elements }, value) => {
                if let ValueType::Enum { variant, data, enum_type } = value.inner() {
                    if path.segments.len() == 2 && path.segments[0].ident == *enum_type && path.segments[1].ident == *variant {
                        match data {
                            Some(values) if elements.len() == values.len() => {
                                for (element, value) in elements.iter().zip(values.iter()) {
                                    if !self.pattern_matches(element, value)? {
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
            }

            (Pattern::Tuple(elements), value) => {
                if let ValueType::Enum { data: Some(values), .. } = value.inner() {
                    if elements.len() == values.len() {
                        for (element, value) in elements.iter().zip(values.iter()) {
                            if !self.pattern_matches(element, value)? {
                                return Ok(false);
                            }
                        }
                        Ok(true)
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            }

            (Pattern::Reference { mutable, pattern }, value) => match value.inner() {
                ValueType::Reference { data, .. } => {
                    if *mutable && !value.is_mutable() {
                        return Ok(false);
                    }

                    if let Some(inner_value) = data {
                        self.pattern_matches(pattern, &inner_value)
                    } else {
                        Ok(false)
                    }
                }

                _ => self.pattern_matches(pattern, value),
            },

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

            (Pattern::Wildcard, _) => Ok(true),

            _ => Ok(false),
        }
    }

    fn evaluate_guard(&mut self, guard: &Expr) -> Result<bool, String> {
        match self.evaluate_expression(guard)?.inner() {
            ValueType::Boolean(b) => Ok(b),
            _ => Err("Guard expression must evaluate to a boolean".to_string()),
        }
    }
}
