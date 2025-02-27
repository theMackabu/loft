use crate::parser::ast::{self, Expr, Stmt};
use crate::util::extract_identifier_info;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum DeclKind {
    Enum,
    Variable,
    Function,
    Module,
    Reference { mutable: bool },
}

#[derive(Clone, Debug)]
pub struct SymbolInfo {
    pub name: String,
    pub kind: DeclKind,
    pub mutable: bool,
    pub is_function: bool,
    pub initialized: bool,
}

#[derive(Debug, PartialEq)]
pub enum BindingKind {
    Variable(bool),
    Reference(bool),
}

#[derive(Debug)]
pub struct Scope {
    scopes: Vec<HashMap<String, SymbolInfo>>,
    function_boundaries: Vec<usize>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            scopes: vec![HashMap::new()],
            function_boundaries: vec![0],
        }
    }

    pub fn enter_scope(&mut self) { self.scopes.push(HashMap::new()); }

    pub fn exit_scope(&mut self) { self.scopes.pop(); }

    pub fn enter_function_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.function_boundaries.push(self.scopes.len() - 1);
    }

    pub fn exit_function_scope(&mut self) {
        while self.scopes.len() > self.function_boundaries.last().cloned().unwrap_or(0) {
            self.scopes.pop();
        }
        self.function_boundaries.pop();
    }

    pub fn resolve_program(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for stmt in statements {
            resolve_stmt(stmt, self)?;
        }

        Ok(())
    }

    pub fn declare_variable(&mut self, name: &str, mutable: bool) {
        if let Some(current) = self.scopes.last_mut() {
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Variable,
                    mutable,
                    is_function: false,
                    initialized: false,
                },
            );
        }
    }

    pub fn declare_reference(&mut self, name: &str, mutable: bool) {
        if let Some(current) = self.scopes.last_mut() {
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Reference { mutable },
                    mutable,
                    is_function: false,
                    initialized: false,
                },
            );
        }
    }

    pub fn declare_enum(&mut self, name: &str) -> Result<(), String> {
        if let Some(current) = self.scopes.last_mut() {
            if current.contains_key(name) {
                return Err(format!("Enum `{name}` is already declared in this scope"));
            }

            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Enum,
                    mutable: false,
                    is_function: false,
                    initialized: true,
                },
            );
            Ok(())
        } else {
            Err("No active scope found".to_owned())
        }
    }

    pub fn declare_variable_in_scope(&mut self, name: &str, mutable: bool, scope_index: usize) -> Result<(), String> {
        if let Some(scope) = self.scopes.get_mut(scope_index) {
            if scope.contains_key(name) {
                return Err(format!("Variable '{name}' already declared in this scope"));
            }

            scope.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Variable,
                    mutable,
                    is_function: false,
                    initialized: false,
                },
            );
            Ok(())
        } else {
            Err(format!("Scope {} not found", scope_index))
        }
    }

    pub fn declare_function(&mut self, name: &str) -> Result<(), String> {
        if let Some(current) = self.scopes.last_mut() {
            if let Some(existing) = current.get(name) {
                if existing.kind == DeclKind::Function {
                    return Err(format!("Function `{}` is already declared in this scope", name));
                }
            }
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Function,
                    mutable: false,
                    is_function: true,
                    initialized: true,
                },
            );
            Ok(())
        } else {
            Err("No active scope found".to_owned())
        }
    }

    pub fn declare_module(&mut self, name: &str) -> Result<(), String> {
        if let Some(current) = self.scopes.last_mut() {
            if current.contains_key(name) {
                return Err(format!("Module `{}` is already declared in this scope", name));
            }
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Module,
                    mutable: false,
                    is_function: false,
                    initialized: true,
                },
            );
            Ok(())
        } else {
            Err("No active scope found".to_owned())
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&SymbolInfo> {
        let current_boundary = *self.function_boundaries.last().unwrap_or(&0);

        for scope in self.scopes[current_boundary..].iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }

        if current_boundary > 0 { self.scopes[0].get(name) } else { None }
    }

    pub fn mark_as_initialized(&mut self, name: &str) -> Result<(), String> {
        let current_boundary = *self.function_boundaries.last().unwrap_or(&0);

        for scope in self.scopes[current_boundary..].iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.initialized = true;
                return Ok(());
            }
        }

        if current_boundary > 0 {
            if let Some(info) = self.scopes[0].get_mut(name) {
                info.initialized = true;
                return Ok(());
            }
        }

        Err(format!("Variable '{}' not found", name))
    }
}

fn resolve_stmt(stmt: &Stmt, env: &mut Scope) -> Result<(), String> {
    use self::ast::{EnumVariant, Stmt::*, Type, UsePath};

    match stmt {
        Let { pattern, initializer, .. } => {
            if let Some(init_expr) = initializer {
                resolve_expr(init_expr, env)?;
            }

            let (init_is_ref, init_ref_mut) = if let Some(init_expr) = initializer {
                match &**init_expr {
                    Expr::Reference { mutable, .. } => (true, *mutable),
                    _ => (true, true),
                }
            } else {
                (false, false)
            };

            for (name, binding) in extract_identifier_info(pattern) {
                match binding {
                    BindingKind::Variable(var_mut) => {
                        if init_is_ref {
                            env.declare_reference(&name, init_ref_mut);
                        } else {
                            env.declare_variable(&name, var_mut);
                        }
                    }

                    BindingKind::Reference(ref_mut) => {
                        env.declare_reference(&name, ref_mut);
                    }
                }

                if initializer.is_some() {
                    env.mark_as_initialized(&name)?;
                }
            }

            Ok(())
        }

        Function { name, params, body, .. } => {
            env.declare_function(name)?;
            env.enter_scope();

            // add type checking for ty
            for (pat, ty) in params {
                for (param_name, binding) in extract_identifier_info(pat) {
                    match binding {
                        BindingKind::Variable(ident_mutable) => match ty {
                            Type::Reference { mutable: ref_mutable, .. } => {
                                env.declare_reference(&param_name, *ref_mutable);
                            }
                            _ => env.declare_variable(&param_name, ident_mutable),
                        },

                        BindingKind::Reference(ident_mutable) => {
                            env.declare_reference(&param_name, ident_mutable);
                        }
                    }

                    env.mark_as_initialized(&param_name)?;
                }
            }

            for s in body {
                resolve_stmt(s, env)?;
            }

            Ok(env.exit_scope())
        }

        Module { body, .. } => {
            env.enter_scope();
            for s in body {
                resolve_stmt(s, env)?;
            }
            env.exit_scope();
            Ok(())
        }

        Enum { variants, .. } => {
            for variant in variants {
                match variant {
                    EnumVariant::Simple(var_name) => {
                        env.declare_variable(var_name, false);
                        env.mark_as_initialized(var_name)?;
                    }
                    EnumVariant::Tuple(var_name, _) => {
                        env.declare_variable(var_name, false);
                        env.mark_as_initialized(var_name)?;
                    }
                    EnumVariant::Struct(var_name, _) => {
                        env.declare_variable(var_name, false);
                        env.mark_as_initialized(var_name)?;
                    }
                }
            }

            Ok(())
        }

        Struct { path, .. } => {
            if let Some(name) = path.segments.last().map(|seg| &seg.ident) {
                env.declare_variable(name, false);
                env.mark_as_initialized(name)?;
            }
            Ok(())
        }

        Impl { items, .. } | TraitImpl { items, .. } => {
            env.enter_scope();
            for item in items {
                resolve_stmt(item, env)?;
            }
            env.exit_scope();
            Ok(())
        }

        Trait { items, .. } => {
            env.enter_scope();
            for item in items {
                resolve_stmt(item, env)?;
            }
            env.exit_scope();
            Ok(())
        }

        Const { name, initializer, .. } | Static { name, initializer, .. } => {
            resolve_expr(initializer, env)?;
            env.declare_variable(name, false);
            env.mark_as_initialized(name)?;
            Ok(())
        }

        Use { path, alias, .. } => {
            let name = match (path, alias) {
                (UsePath::Simple(name), None) => name,
                (_, Some(alias_name)) => alias_name,
                (UsePath::Nested(segments), None) => {
                    if let Some(last) = segments.last() {
                        last
                    } else {
                        return Err("Empty use path".to_string());
                    }
                }
            };
            env.declare_reference(name, false);
            env.mark_as_initialized(name)?;
            Ok(())
        }

        Return(expr) => {
            if let Some(e) = expr {
                resolve_expr(e, env)?;
            }
            Ok(())
        }

        Continue(_) | Break(_, None) => Ok(()),

        Break(_, Some(expr)) => resolve_expr(expr, env),

        TypeAlias { .. } | MacroDefinition { .. } => Ok(()),

        ExpressionStmt(expr) | ExpressionValue(expr) => resolve_expr(expr, env),
    }
}

fn resolve_expr(expr: &Expr, env: &mut Scope) -> Result<(), String> {
    use self::ast::{Expr, Expr::*, WhileCondition};

    match expr {
        Block { statements, value, .. } => {
            env.enter_scope();
            for s in statements {
                resolve_stmt(s, env)?;
            }
            if let Some(inner) = value {
                resolve_expr(inner, env)?;
            }
            env.exit_scope();
            Ok(())
        }

        Closure { params, body, .. } => {
            env.enter_function_scope();

            for (name, _) in params {
                env.declare_variable(name, false);
                env.mark_as_initialized(name)?;
            }

            resolve_expr(body, env)?;

            env.exit_function_scope();
            Ok(())
        }

        Call { function, arguments } => {
            resolve_expr(function, env)?;
            for arg in arguments {
                resolve_expr(arg, env)?;
            }
            Ok(())
        }

        MethodCall { object, arguments, .. } => {
            resolve_expr(object, env)?;
            for arg in arguments {
                resolve_expr(arg, env)?;
            }
            Ok(())
        }

        Binary { left, right, .. } => {
            resolve_expr(left, env)?;
            resolve_expr(right, env)
        }

        Index { array, index } => {
            resolve_expr(array, env)?;
            resolve_expr(index, env)
        }

        MemberAssignment { object, value, .. } => {
            resolve_expr(object, env)?;
            resolve_expr(value, env)
        }

        ArrayRepeat { value, count } => {
            resolve_expr(value, env)?;
            resolve_expr(count, env)
        }

        Range { start, end, .. } => {
            if let Some(start_expr) = start {
                resolve_expr(start_expr, env)?;
            }
            if let Some(end_expr) = end {
                resolve_expr(end_expr, env)?;
            }
            Ok(())
        }

        StructInit { fields, .. } => {
            for (_, (expr, _)) in fields {
                resolve_expr(expr, env)?;
            }
            Ok(())
        }

        Array(elements) => {
            for elem in elements {
                resolve_expr(elem, env)?;
            }
            Ok(())
        }

        Tuple(elements) => {
            for elem in elements {
                resolve_expr(elem, env)?;
            }
            Ok(())
        }

        If {
            condition, then_branch, else_branch, ..
        } => {
            resolve_expr(condition, env)?;
            resolve_expr(then_branch, env)?;
            if let Some(else_expr) = else_branch {
                resolve_expr(else_expr, env)?;
            }
            Ok(())
        }

        IfLet {
            pattern,
            value,
            then_branch,
            else_branch,
        } => {
            resolve_expr(value, env)?;
            env.enter_scope();

            for (name, binding) in extract_identifier_info(pattern) {
                match binding {
                    BindingKind::Variable(mutable) => {
                        env.declare_variable(&name, mutable);
                    }
                    BindingKind::Reference(mutable) => {
                        env.declare_reference(&name, mutable);
                    }
                }

                env.mark_as_initialized(&name)?;
            }

            resolve_expr(then_branch, env)?;
            env.exit_scope();

            if let Some(else_expr) = else_branch {
                resolve_expr(else_expr, env)?;
            }

            Ok(())
        }

        While { condition, body, .. } => {
            match condition {
                WhileCondition::Expression(expr) => resolve_expr(expr, env)?,

                WhileCondition::Let(pattern, expr) => {
                    resolve_expr(expr, env)?;
                    env.enter_scope();

                    for (name, binding) in extract_identifier_info(pattern) {
                        match binding {
                            BindingKind::Variable(mutable) => {
                                env.declare_variable(&name, mutable);
                            }
                            BindingKind::Reference(mutable) => {
                                env.declare_reference(&name, mutable);
                            }
                        }

                        env.mark_as_initialized(&name)?;
                    }

                    resolve_expr(body, env)?;
                    env.exit_scope();

                    return Ok(());
                }
            }

            resolve_expr(body, env)
        }

        For { pattern, iterable, body, .. } => {
            resolve_expr(iterable, env)?;
            env.enter_scope();

            for (name, binding) in extract_identifier_info(pattern) {
                match binding {
                    BindingKind::Variable(mutable) => {
                        env.declare_variable(&name, mutable);
                    }
                    BindingKind::Reference(mutable) => {
                        env.declare_reference(&name, mutable);
                    }
                }

                env.mark_as_initialized(&name)?;
            }

            resolve_expr(body, env)?;
            env.exit_scope();

            Ok(())
        }

        Identifier(name) => {
            if let Some(info) = env.resolve(name) {
                if !info.initialized {
                    return Err(format!("Variable '{}' used before initialization", name));
                }
            } else {
                return Err(format!("Variable '{}' not found", name));
            }
            Ok(())
        }

        Assignment { target, value } => {
            resolve_expr(value, env)?;

            if let Expr::Identifier(name) = target.as_ref() {
                if let Some(info) = env.resolve(name) {
                    if info.initialized && !info.mutable {
                        return Err(format!("Cannot assign to immutable variable '{}'", name));
                    }
                } else {
                    return Err(format!("Variable '{}' not found", name));
                }
            } else {
                resolve_expr(target, env)?;
            }

            Ok(())
        }

        CompoundAssignment { target, value, .. } => {
            resolve_expr(target, env)?;
            resolve_expr(value, env)?;

            if let Expr::Identifier(name) = target.as_ref() {
                if let Some(info) = env.resolve(name) {
                    if !info.initialized {
                        return Err(format!("Variable '{}' used before initialization", name));
                    }
                    if !info.mutable {
                        return Err(format!("Cannot modify immutable variable '{}'", name));
                    }
                } else {
                    return Err(format!("Variable '{}' not found", name));
                }
            }

            Ok(())
        }

        Match { value, arms } => {
            resolve_expr(value, env)?;

            for arm in arms {
                env.enter_scope();

                for (name, binding) in extract_identifier_info(&arm.pattern) {
                    match binding {
                        BindingKind::Variable(mutable) => {
                            env.declare_variable(&name, mutable);
                        }
                        BindingKind::Reference(mutable) => {
                            env.declare_reference(&name, mutable);
                        }
                    }

                    env.mark_as_initialized(&name)?;
                }

                if let Some(guard) = &arm.guard {
                    resolve_expr(guard, env)?;
                }

                resolve_expr(&arm.body, env)?;
                env.exit_scope();
            }

            Ok(())
        }

        Cast { expr, .. } => resolve_expr(expr, env),

        Try(expr) | Await(expr) => resolve_expr(expr, env),

        Loop { body, .. } => resolve_expr(body, env),

        MemberAccess { object, .. } => resolve_expr(object, env),

        Unary { operand, .. } => resolve_expr(operand, env),

        Reference { operand, .. } | Dereference { operand } => resolve_expr(operand, env),

        Path(_) | Boolean(_) | String(_) | Integer(_, _) | Float(_, _) | Unit => Ok(()),

        // implement later...
        MacroInvocation { .. } => Ok(()),
    }
}
