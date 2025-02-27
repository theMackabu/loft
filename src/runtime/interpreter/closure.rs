use super::*;
use std::collections::HashSet;

impl<'st> Interpreter {
    pub fn evaluate_closure(&mut self, params: &[(String, Option<Type>)], body: &Expr) -> Result<Value, String> {
        let captured_names = self.analyze_captures(body)?;
        let mut captures = HashMap::new();

        for name in &captured_names {
            if let Some(value) = self.env.get_variable(name) {
                captures.insert(name.clone(), value.clone());
            } else {
                return Err(format!("Cannot capture variable '{}' - not found", name));
            }
        }

        let formal_params = params
            .iter()
            .map(|(name, ty)| {
                (
                    Pattern::Identifier { name: name.clone(), mutable: false },
                    ty.clone().unwrap_or_else(|| Type::Implied(name.to_string())),
                )
            })
            .collect();

        let body_stmts = match body {
            Expr::Block { statements, value, .. } => {
                let mut stmts = statements.clone();
                if let Some(val) = value {
                    stmts.push(Stmt::Return(Some(*val.clone())));
                }
                stmts
            }
            _ => vec![Stmt::Return(Some(body.clone()))],
        };

        let function_data = Rc::new(FunctionData {
            name: None,
            params: formal_params,
            body: body_stmts,
            return_type: None,
            captures: Some(captures),
            is_method: false,
            visibility: false,
        });

        Ok(val!(ValueType::Function(function_data)))
    }

    fn analyze_captures(&self, expr: &Expr) -> Result<HashSet<String>, String> {
        let mut captures = HashSet::new();
        let mut defined_vars = HashSet::new();

        if let Expr::Identifier(name) = expr {
            if !defined_vars.contains(name) && self.env.get_variable(name).is_some() {
                captures.insert(name.clone());
            }
            return Ok(captures);
        }

        self.find_captures_in_expr(expr, &mut captures, &mut defined_vars)?;

        Ok(captures)
    }

    fn find_captures_in_expr(&self, expr: &Expr, captures: &mut HashSet<String>, defined_vars: &mut HashSet<String>) -> Result<(), String> {
        match expr {
            Expr::Identifier(name) => {
                if !defined_vars.contains(name) && self.env.get_variable(name).is_some() {
                    captures.insert(name.clone());
                }
            }

            Expr::Block { statements, value, .. } => {
                let mut block_defined = HashSet::new();

                for stmt in statements {
                    self.find_captures_in_stmt(stmt, captures, defined_vars, &mut block_defined)?;
                }

                if let Some(val_expr) = value {
                    self.find_captures_in_expr(val_expr, captures, defined_vars)?;
                }

                defined_vars.extend(block_defined);
            }

            Expr::IfLet {
                pattern,
                value,
                then_branch,
                else_branch,
            } => {
                self.find_captures_in_expr(value, captures, defined_vars)?;

                let mut if_let_defined = HashSet::new();
                self.extract_defined_vars_from_pattern(pattern, &mut if_let_defined);

                let mut then_defined = defined_vars.clone();
                then_defined.extend(if_let_defined);

                self.find_captures_in_expr(then_branch, captures, &mut then_defined)?;

                if let Some(else_expr) = else_branch {
                    self.find_captures_in_expr(else_expr, captures, defined_vars)?;
                }
            }

            Expr::Match { value, arms } => {
                self.find_captures_in_expr(value, captures, defined_vars)?;

                for arm in arms {
                    let mut arm_defined = defined_vars.clone();
                    self.extract_defined_vars_from_pattern(&arm.pattern, &mut arm_defined);

                    if let Some(guard) = &arm.guard {
                        self.find_captures_in_expr(guard, captures, &mut arm_defined)?;
                    }

                    self.find_captures_in_expr(&arm.body, captures, &mut arm_defined)?;
                }
            }

            Expr::Closure { params, body, .. } => {
                let mut closure_defined = defined_vars.clone();

                for (name, _) in params {
                    closure_defined.insert(name.clone());
                }

                self.find_captures_in_expr(body, captures, &mut closure_defined)?;
            }

            Expr::For { pattern, iterable, body, .. } => {
                self.find_captures_in_expr(iterable, captures, defined_vars)?;

                let mut for_defined = defined_vars.clone();
                self.extract_defined_vars_from_pattern(pattern, &mut for_defined);

                self.find_captures_in_expr(body, captures, &mut for_defined)?;
            }

            Expr::While { condition, body, .. } => match condition {
                WhileCondition::Expression(expr) => {
                    self.find_captures_in_expr(expr, captures, defined_vars)?;
                    self.find_captures_in_expr(body, captures, defined_vars)?;
                }
                WhileCondition::Let(pattern, expr) => {
                    self.find_captures_in_expr(expr, captures, defined_vars)?;

                    let mut while_defined = defined_vars.clone();
                    self.extract_defined_vars_from_pattern(pattern, &mut while_defined);

                    self.find_captures_in_expr(body, captures, &mut while_defined)?;
                }
            },

            Expr::Assignment { target, value } => {
                if let Expr::Identifier(name) = target.as_ref() {
                    defined_vars.insert(name.clone());
                } else {
                    self.find_captures_in_expr(target, captures, defined_vars)?;
                }

                self.find_captures_in_expr(value, captures, defined_vars)?;
            }

            Expr::Binary { left, operator: _, right } => {
                self.find_captures_in_expr(left, captures, defined_vars)?;
                self.find_captures_in_expr(right, captures, defined_vars)?;
            }

            Expr::Call { function, arguments } => {
                self.find_captures_in_expr(function, captures, defined_vars)?;
                for arg in arguments {
                    self.find_captures_in_expr(arg, captures, defined_vars)?;
                }
            }

            Expr::MethodCall { object, method: _, arguments } => {
                self.find_captures_in_expr(object, captures, defined_vars)?;
                for arg in arguments {
                    self.find_captures_in_expr(arg, captures, defined_vars)?;
                }
            }

            Expr::If { condition, then_branch, else_branch } => {
                self.find_captures_in_expr(condition, captures, defined_vars)?;
                self.find_captures_in_expr(then_branch, captures, defined_vars)?;
                if let Some(else_expr) = else_branch {
                    self.find_captures_in_expr(else_expr, captures, defined_vars)?;
                }
            }

            Expr::Unary { operator: _, operand } => {
                self.find_captures_in_expr(operand, captures, defined_vars)?;
            }

            Expr::MemberAccess { object, member: _ } => {
                self.find_captures_in_expr(object, captures, defined_vars)?;
            }

            Expr::Index { array, index } => {
                self.find_captures_in_expr(array, captures, defined_vars)?;
                self.find_captures_in_expr(index, captures, defined_vars)?;
            }

            Expr::Reference { mutable: _, operand } => {
                self.find_captures_in_expr(operand, captures, defined_vars)?;
            }

            Expr::Dereference { operand } => {
                self.find_captures_in_expr(operand, captures, defined_vars)?;
            }

            Expr::Array(elements) => {
                for elem in elements {
                    self.find_captures_in_expr(elem, captures, defined_vars)?;
                }
            }

            Expr::Tuple(elements) => {
                for elem in elements {
                    self.find_captures_in_expr(elem, captures, defined_vars)?;
                }
            }

            Expr::StructInit { path: _, fields } => {
                for (_, (expr, _)) in fields {
                    self.find_captures_in_expr(expr, captures, defined_vars)?;
                }
            }

            Expr::Range { start, end, inclusive: _ } => {
                if let Some(start_expr) = start {
                    self.find_captures_in_expr(start_expr, captures, defined_vars)?;
                }
                if let Some(end_expr) = end {
                    self.find_captures_in_expr(end_expr, captures, defined_vars)?;
                }
            }

            Expr::Boolean(_)
            | Expr::Integer(_, _)
            | Expr::Float(_, _)
            | Expr::String(_)
            | Expr::Path(_)
            | Expr::Unit
            | Expr::Try(_)
            | Expr::Await(_)
            | Expr::ArrayRepeat { .. }
            | Expr::Cast { .. }
            | Expr::MemberAssignment { .. }
            | Expr::CompoundAssignment { .. }
            | Expr::Loop { .. }
            | Expr::MacroInvocation { .. } => {}
        }

        Ok(())
    }

    fn find_captures_in_stmt(&self, stmt: &Stmt, captures: &mut HashSet<String>, defined_vars: &mut HashSet<String>, block_defined: &mut HashSet<String>) -> Result<(), String> {
        match stmt {
            Stmt::Let {
                pattern,
                type_annotation: _,
                initializer,
                attributes: _,
            } => {
                if let Some(init) = initializer {
                    self.find_captures_in_expr(init, captures, defined_vars)?;
                }

                self.extract_defined_vars_from_pattern(pattern, block_defined);
            }

            Stmt::Function { name, .. } => {
                block_defined.insert(name.clone());
            }

            Stmt::ExpressionStmt(expr) | Stmt::ExpressionValue(expr) => {
                self.find_captures_in_expr(expr, captures, defined_vars)?;
            }

            Stmt::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.find_captures_in_expr(expr, captures, defined_vars)?;
                }
            }

            _ => {}
        }

        Ok(())
    }

    fn extract_defined_vars_from_pattern(&self, pattern: &Pattern, defined_vars: &mut HashSet<String>) {
        match pattern {
            Pattern::Identifier { name, mutable: _ } => {
                defined_vars.insert(name.clone());
            }

            Pattern::Reference { mutable: _, pattern } => {
                self.extract_defined_vars_from_pattern(pattern, defined_vars);
            }

            Pattern::Tuple(patterns) => {
                for pat in patterns {
                    self.extract_defined_vars_from_pattern(pat, defined_vars);
                }
            }

            Pattern::TupleStruct { elements, .. } => {
                for pat in elements {
                    self.extract_defined_vars_from_pattern(pat, defined_vars);
                }
            }

            Pattern::Struct { fields, .. } => {
                for (_, field_pattern) in fields {
                    self.extract_defined_vars_from_pattern(field_pattern, defined_vars);
                }
            }

            Pattern::BindingPattern { name, mutable: _, subpattern } => {
                defined_vars.insert(name.clone());
                self.extract_defined_vars_from_pattern(subpattern, defined_vars);
            }

            Pattern::Or(patterns) => {
                if patterns.is_empty() {
                    return;
                }

                let mut first_vars = HashSet::new();
                if let Some(first) = patterns.first() {
                    self.extract_defined_vars_from_pattern(first, &mut first_vars);
                }

                for pattern in patterns.iter().skip(1) {
                    let mut branch_vars = HashSet::new();
                    self.extract_defined_vars_from_pattern(pattern, &mut branch_vars);
                    first_vars.retain(|var| branch_vars.contains(var));

                    if first_vars.is_empty() {
                        break;
                    }
                }

                defined_vars.extend(first_vars);
            }

            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path(_) => {}
        }
    }
}
