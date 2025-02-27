use super::stack::WorkQueue;
use crate::parser::ast::{self, *};
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

enum ResolutionTask<'a> {
    Stmt(&'a Stmt),
    Expr(&'a Expr),
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

    pub fn in_closure(&self) -> bool { self.function_boundaries.len() > 1 }

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

    pub fn resolve_program(&mut self, stmt: &[Stmt]) -> Result<(), String> {
        resolve_ast_iteratively(stmt, self)?;

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

        if self.function_boundaries.len() > 1 {
            for i in (0..current_boundary).rev() {
                if let Some(info) = self.scopes[i].get(name) {
                    return Some(info);
                }
            }
        } else if current_boundary > 0 {
            return self.scopes[0].get(name);
        }

        None
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

fn resolve_ast_iteratively(statements: &[Stmt], env: &mut Scope) -> Result<(), String> {
    let mut work_queue = WorkQueue::new();

    for stmt in statements {
        work_queue.enqueue(ResolutionTask::Stmt(stmt));
    }

    Ok(process_local_queue(&mut work_queue, env)?)
}

fn process_local_queue<'a>(mut work_queue: &mut WorkQueue<ResolutionTask<'a>>, env: &mut Scope) -> Result<(), String> {
    while !work_queue.is_empty() {
        let task = work_queue.dequeue().unwrap();
        match task {
            ResolutionTask::Stmt(stmt) => process_stmt_local(stmt, env, &mut work_queue)?,
            ResolutionTask::Expr(expr) => process_expr_local(expr, env, &mut work_queue)?,
        }
    }
    Ok(())
}

fn process_stmt_local<'a>(stmt: &'a Stmt, env: &mut Scope, work_queue: &mut WorkQueue<ResolutionTask<'a>>) -> Result<(), String> {
    use ast::Stmt::*;
    match stmt {
        Let { pattern, initializer, .. } => {
            if let Some(init_expr) = initializer {
                work_queue.enqueue(ResolutionTask::Expr(init_expr));
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

            let mut local_queue = WorkQueue::new();
            for s in body {
                local_queue.enqueue(ResolutionTask::Stmt(s));
            }

            process_local_queue(&mut local_queue, env)?;
            env.exit_scope();
        }

        Module { body, .. } => {
            env.enter_scope();

            let mut local_queue = WorkQueue::new();
            for s in body {
                local_queue.enqueue(ResolutionTask::Stmt(s));
            }

            process_local_queue(&mut local_queue, env)?;
            env.exit_scope();
        }

        Impl { items, .. } | TraitImpl { items, .. } => {
            env.enter_scope();

            let mut local_queue = WorkQueue::new();
            for item in items {
                local_queue.enqueue(ResolutionTask::Stmt(item));
            }

            process_local_queue(&mut local_queue, env)?;
            env.exit_scope();
        }

        Trait { items, .. } => {
            env.enter_scope();

            let mut local_queue = WorkQueue::new();
            for item in items {
                local_queue.enqueue(ResolutionTask::Stmt(item));
            }

            process_local_queue(&mut local_queue, env)?;
            env.exit_scope();
        }

        Enum { variants, .. } => {
            for variant in variants {
                match variant {
                    EnumVariant::Simple(var_name) | EnumVariant::Tuple(var_name, _) | EnumVariant::Struct(var_name, _) => {
                        env.declare_variable(var_name, false);
                        env.mark_as_initialized(var_name)?;
                    }
                }
            }
        }

        Struct { path, .. } => {
            if let Some(name) = path.segments.last().map(|seg| &seg.ident) {
                env.declare_variable(name, false);
                env.mark_as_initialized(name)?;
            }
        }

        Const { name, initializer, .. } | Static { name, initializer, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(initializer));
            env.declare_variable(name, false);
            env.mark_as_initialized(name)?;
        }

        Use { path, alias, .. } => {
            let name = match (path, alias) {
                (UsePath::Simple(name), None) => name,
                (_, Some(alias_name)) => alias_name,
                (UsePath::Nested(segments), None) => segments.last().ok_or_else(|| "Empty use path".to_string())?,
            };

            env.declare_reference(name, false);
            env.mark_as_initialized(name)?;
        }

        Return(expr_opt) => {
            if let Some(expr) = expr_opt {
                work_queue.enqueue(ResolutionTask::Expr(expr));
            }
        }

        Break(_, Some(expr)) | ExpressionStmt(expr) | ExpressionValue(expr) => {
            work_queue.enqueue(ResolutionTask::Expr(expr));
        }

        Continue(_) | Break(_, None) => {}

        TypeAlias { .. } | MacroDefinition { .. } => {}
    }

    Ok(())
}

fn process_expr_local<'a>(expr: &'a Expr, env: &mut Scope, work_queue: &mut WorkQueue<ResolutionTask<'a>>) -> Result<(), String> {
    use ast::Expr::*;
    match expr {
        Block { statements, value, .. } => {
            env.enter_scope();

            let mut local_queue = WorkQueue::new();
            for s in statements {
                local_queue.enqueue(ResolutionTask::Stmt(s));
            }

            if let Some(inner) = value {
                local_queue.enqueue(ResolutionTask::Expr(inner));
            }

            process_local_queue(&mut local_queue, env)?;
            env.exit_scope();
        }

        Closure { params, body, .. } => {
            env.enter_function_scope();

            for (name, _) in params {
                env.declare_variable(name, false);
                env.mark_as_initialized(name)?;
            }

            let mut local_queue = WorkQueue::new();
            local_queue.enqueue(ResolutionTask::Expr(body));

            process_local_queue(&mut local_queue, env)?;
            env.exit_function_scope();
        }

        Call { function, arguments } => {
            work_queue.enqueue(ResolutionTask::Expr(function));
            for arg in arguments {
                work_queue.enqueue(ResolutionTask::Expr(arg));
            }
        }

        MethodCall { object, arguments, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(object));
            for arg in arguments {
                work_queue.enqueue(ResolutionTask::Expr(arg));
            }
        }

        Binary { left, right, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(left));
            work_queue.enqueue(ResolutionTask::Expr(right));
        }

        Index { array, index } => {
            work_queue.enqueue(ResolutionTask::Expr(array));
            work_queue.enqueue(ResolutionTask::Expr(index));
        }

        MemberAssignment { object, value, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(object));
            work_queue.enqueue(ResolutionTask::Expr(value));
        }

        ArrayRepeat { value, count } => {
            work_queue.enqueue(ResolutionTask::Expr(value));
            work_queue.enqueue(ResolutionTask::Expr(count));
        }

        Range { start, end, .. } => {
            if let Some(start_expr) = start {
                work_queue.enqueue(ResolutionTask::Expr(start_expr));
            }
            if let Some(end_expr) = end {
                work_queue.enqueue(ResolutionTask::Expr(end_expr));
            }
        }

        StructInit { fields, .. } => {
            for (_, (expr, _)) in fields {
                work_queue.enqueue(ResolutionTask::Expr(expr));
            }
        }

        Array(elements) => {
            for elem in elements {
                work_queue.enqueue(ResolutionTask::Expr(elem));
            }
        }

        Tuple(elements) => {
            for elem in elements {
                work_queue.enqueue(ResolutionTask::Expr(elem));
            }
        }

        If {
            condition, then_branch, else_branch, ..
        } => {
            work_queue.enqueue(ResolutionTask::Expr(condition));
            work_queue.enqueue(ResolutionTask::Expr(then_branch));
            if let Some(else_expr) = else_branch {
                work_queue.enqueue(ResolutionTask::Expr(else_expr));
            }
        }

        IfLet {
            pattern,
            value,
            then_branch,
            else_branch,
        } => {
            work_queue.enqueue(ResolutionTask::Expr(value));
            env.enter_scope();

            for (name, binding) in extract_identifier_info(pattern) {
                match binding {
                    BindingKind::Variable(mutable) => env.declare_variable(&name, mutable),
                    BindingKind::Reference(mutable) => env.declare_reference(&name, mutable),
                }
                env.mark_as_initialized(&name)?;
            }

            let mut local_queue = WorkQueue::new();
            local_queue.enqueue(ResolutionTask::Expr(then_branch));

            process_local_queue(&mut local_queue, env)?;
            env.exit_scope();

            if let Some(else_expr) = else_branch {
                work_queue.enqueue(ResolutionTask::Expr(else_expr));
            }
        }

        While { condition, body, .. } => {
            match condition {
                WhileCondition::Expression(expr) => {
                    work_queue.enqueue(ResolutionTask::Expr(expr));
                }

                WhileCondition::Let(pattern, expr) => {
                    work_queue.enqueue(ResolutionTask::Expr(expr));
                    env.enter_scope();

                    for (name, binding) in extract_identifier_info(pattern) {
                        match binding {
                            BindingKind::Variable(mutable) => env.declare_variable(&name, mutable),
                            BindingKind::Reference(mutable) => env.declare_reference(&name, mutable),
                        }
                        env.mark_as_initialized(&name)?;
                    }

                    let mut local_queue = WorkQueue::new();
                    local_queue.enqueue(ResolutionTask::Expr(body));

                    process_local_queue(&mut local_queue, env)?;
                    env.exit_scope();

                    return Ok(());
                }
            }

            work_queue.enqueue(ResolutionTask::Expr(body));
        }

        For { pattern, iterable, body, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(iterable));
            env.enter_scope();

            for (name, binding) in extract_identifier_info(pattern) {
                match binding {
                    BindingKind::Variable(mutable) => env.declare_variable(&name, mutable),
                    BindingKind::Reference(mutable) => env.declare_reference(&name, mutable),
                }
                env.mark_as_initialized(&name)?;
            }

            let mut local_queue = WorkQueue::new();
            work_queue.enqueue(ResolutionTask::Expr(body));

            process_local_queue(&mut local_queue, env)?;
            env.exit_scope();
        }

        Identifier(name) => {
            if let Some(info) = env.resolve(name) {
                if !info.initialized {
                    return Err(format!("Variable '{}' used before initialization", name));
                }
            } else {
                return Err(format!("Variable '{}' not found", name));
            }
        }

        Assignment { target, value } => {
            work_queue.enqueue(ResolutionTask::Expr(value));

            if let Expr::Identifier(name) = target.as_ref() {
                if let Some(info) = env.resolve(name) {
                    if info.initialized && !info.mutable {
                        return Err(format!("Cannot assign to immutable variable '{}'", name));
                    }
                } else {
                    return Err(format!("Variable '{}' not found", name));
                }
            } else {
                work_queue.enqueue(ResolutionTask::Expr(target));
            }
        }

        CompoundAssignment { target, value, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(target));
            work_queue.enqueue(ResolutionTask::Expr(value));

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
        }

        Match { value, arms } => {
            work_queue.enqueue(ResolutionTask::Expr(value));

            for arm in arms {
                env.enter_scope();

                for (name, binding) in extract_identifier_info(&arm.pattern) {
                    match binding {
                        BindingKind::Variable(mutable) => env.declare_variable(&name, mutable),
                        BindingKind::Reference(mutable) => env.declare_reference(&name, mutable),
                    }
                    env.mark_as_initialized(&name)?;
                }

                let mut local_queue = WorkQueue::new();
                if let Some(guard) = &arm.guard {
                    local_queue.enqueue(ResolutionTask::Expr(guard));
                }
                local_queue.enqueue(ResolutionTask::Expr(&arm.body));

                process_local_queue(&mut local_queue, env)?;
                env.exit_scope();
            }
        }

        Cast { expr, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(expr));
        }

        Try(expr) | Await(expr) => {
            work_queue.enqueue(ResolutionTask::Expr(expr));
        }

        Loop { body, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(body));
        }

        MemberAccess { object, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(object));
        }

        Unary { operand, .. } => {
            work_queue.enqueue(ResolutionTask::Expr(operand));
        }

        Reference { operand, .. } | Dereference { operand } => {
            work_queue.enqueue(ResolutionTask::Expr(operand));
        }

        Path(_) | Boolean(_) | String(_) | Integer(_, _) | Float(_, _) | Unit => {}

        MacroInvocation { .. } => {}
    }

    Ok(())
}
