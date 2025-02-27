use super::*;

impl<'st> Interpreter {
    pub fn handle_loop(&mut self, label: &Option<String>, body: &Expr) -> Result<Value, String> {
        loop {
            match self.evaluate_expression(body)? {
                break_value if matches!(break_value.borrow().inner(), ValueType::Return(_)) => {
                    return Ok(break_value);
                }
                break_value if matches!(break_value.borrow().inner(), ValueType::Break(_, _)) => {
                    let (break_label, value) = if let ValueType::Break(label, value) = break_value.borrow().inner() {
                        (label, value)
                    } else {
                        unreachable!()
                    };

                    match (label, &break_label) {
                        (Some(loop_label), Some(break_label)) if loop_label == break_label => {
                            return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                        }
                        (None, None) => {
                            return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                        }
                        (_, Some(_)) => return Ok(break_value),
                        (Some(_), None) => return Ok(break_value),
                    }
                }
                continue_value if matches!(continue_value.borrow().inner(), ValueType::Continue(_)) => {
                    let continue_label = if let ValueType::Continue(label) = continue_value.borrow().inner() {
                        label
                    } else {
                        unreachable!()
                    };

                    match (label, &continue_label) {
                        (Some(loop_label), Some(continue_label)) if loop_label == continue_label => continue,
                        (None, None) => continue,
                        (_, Some(_)) => return Ok(continue_value),
                        (Some(_), None) => continue,
                    }
                }
                _ => continue,
            }
        }
    }

    pub fn handle_while(&mut self, label: &Option<String>, condition: &WhileCondition, body: &Expr) -> Result<Value, String> {
        loop {
            let should_execute = match condition {
                WhileCondition::Expression(expr) => {
                    let cond_value = self.evaluate_expression(expr)?;
                    if let ValueType::Boolean(b) = cond_value.borrow().inner() {
                        b
                    } else {
                        return Err("While condition must evaluate to a boolean".to_string());
                    }
                }
                WhileCondition::Let(pattern, expr) => {
                    let value = self.evaluate_expression(expr)?;
                    self.env.enter_scope();
                    match self.declare_pattern(pattern, None, &value, true) {
                        Ok(_) => true,
                        Err(_) => {
                            self.env.exit_scope();
                            false
                        }
                    }
                }
            };

            if !should_execute {
                break;
            }

            match self.evaluate_expression(body)? {
                break_value if matches!(break_value.borrow().inner(), ValueType::Return(_)) => {
                    if let WhileCondition::Let(..) = condition {
                        self.env.exit_scope();
                    }
                    return Ok(break_value);
                }
                break_value if matches!(break_value.borrow().inner(), ValueType::Break(_, _)) => {
                    if let WhileCondition::Let(..) = condition {
                        self.env.exit_scope();
                    }

                    let (break_label, value) = if let ValueType::Break(label, value) = break_value.borrow().inner() {
                        (label, value)
                    } else {
                        unreachable!()
                    };

                    match (label, &break_label) {
                        (Some(loop_label), Some(break_label)) if loop_label == break_label => {
                            return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                        }
                        (None, None) => {
                            return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                        }
                        (_, Some(_)) => return Ok(break_value),
                        (Some(_), None) => return Ok(break_value),
                    }
                }
                continue_value if matches!(continue_value.borrow().inner(), ValueType::Continue(_)) => {
                    if let WhileCondition::Let(..) = condition {
                        self.env.exit_scope();
                    }

                    let continue_label = if let ValueType::Continue(label) = continue_value.borrow().inner() {
                        label
                    } else {
                        unreachable!()
                    };

                    match (label, &continue_label) {
                        (Some(loop_label), Some(continue_label)) if loop_label == continue_label => continue,
                        (None, None) => continue,
                        (_, Some(_)) => return Ok(continue_value),
                        (Some(_), None) => continue,
                    }
                }
                _ => {
                    if let WhileCondition::Let(..) = condition {
                        self.env.exit_scope();
                    }
                    continue;
                }
            }
        }

        Ok(val!(ValueType::Unit))
    }

    pub fn handle_for(&mut self, label: &Option<String>, pattern: &Pattern, iterable: &Expr, body: &Expr) -> Result<Value, String> {
        let iter_value = self.evaluate_expression(iterable)?;

        match iter_value.borrow().inner() {
            ValueType::Array { ref el, .. } => {
                for item in el {
                    self.env.enter_scope();
                    self.declare_pattern(pattern, None, item, true)?;

                    match self.evaluate_expression(body)? {
                        break_value if matches!(break_value.borrow().inner(), ValueType::Return(_)) => {
                            self.env.exit_scope();
                            return Ok(break_value);
                        }
                        break_value if matches!(break_value.borrow().inner(), ValueType::Break(_, _)) => {
                            self.env.exit_scope();
                            let (break_label, value) = if let ValueType::Break(label, value) = break_value.borrow().inner() {
                                (label, value)
                            } else {
                                unreachable!()
                            };

                            match (label, &break_label) {
                                (Some(loop_label), Some(break_label)) if loop_label == break_label => {
                                    return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                                }
                                (None, None) => {
                                    return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                                }
                                (_, Some(_)) => return Ok(break_value),
                                (Some(_), None) => return Ok(break_value),
                            }
                        }
                        continue_value if matches!(continue_value.borrow().inner(), ValueType::Continue(_)) => {
                            self.env.exit_scope();
                            let continue_label = if let ValueType::Continue(label) = continue_value.borrow().inner() {
                                label
                            } else {
                                unreachable!()
                            };

                            match (label, &continue_label) {
                                (Some(loop_label), Some(continue_label)) if loop_label == continue_label => continue,
                                (None, None) => continue,
                                (_, Some(_)) => return Ok(continue_value),
                                (Some(_), None) => continue,
                            }
                        }
                        _ => {
                            self.env.exit_scope();
                            continue;
                        }
                    }
                }
            }

            ValueType::Range { start, end, inclusive } => {
                let start_val = start.borrow().inner();
                let end_val = end.borrow().inner();

                macro_rules! handle_range {
                    ($s:expr, $e:expr, $variant:ident) => {{
                        let s = match $s {
                            Some(val) => val,
                            None => 0,
                        };

                        match $e {
                            None => {
                                let mut i = 0;
                                loop {
                                    self.env.enter_scope();
                                    self.declare_pattern(pattern, None, &val!(ValueType::$variant(s + i)), true)?;
                                    match self.evaluate_expression(body)? {
                                        break_value if matches!(break_value.borrow().inner(), ValueType::Return(_)) => {
                                            self.env.exit_scope();
                                            return Ok(break_value);
                                        }
                                        break_value if matches!(break_value.borrow().inner(), ValueType::Break(_, _)) => {
                                            self.env.exit_scope();
                                            let (break_label, value) = if let ValueType::Break(label, value) = break_value.borrow().inner() {
                                                (label, value)
                                            } else {
                                                unreachable!()
                                            };
                                            match (label, &break_label) {
                                                (Some(loop_label), Some(break_label)) if loop_label == break_label => {
                                                    return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                                                }
                                                (None, None) => {
                                                    return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                                                }
                                                (_, Some(_)) => return Ok(break_value),
                                                (Some(_), None) => return Ok(break_value),
                                            }
                                        }
                                        continue_value if matches!(continue_value.borrow().inner(), ValueType::Continue(_)) => {
                                            self.env.exit_scope();
                                            let continue_label = if let ValueType::Continue(label) = continue_value.borrow().inner() {
                                                label
                                            } else {
                                                unreachable!()
                                            };
                                            match (label, &continue_label) {
                                                (Some(loop_label), Some(continue_label)) if loop_label == continue_label => {
                                                    i += 1;
                                                    continue;
                                                }
                                                (None, None) => {
                                                    i += 1;
                                                    continue;
                                                }
                                                (_, Some(_)) => return Ok(continue_value),
                                                (Some(_), None) => {
                                                    i += 1;
                                                    continue;
                                                }
                                            }
                                        }
                                        _ => {
                                            self.env.exit_scope();
                                            i += 1;
                                            continue;
                                        }
                                    }
                                }
                            }

                            Some(e) => {
                                for i in s..if inclusive { e + 1 } else { e } {
                                    self.env.enter_scope();
                                    self.declare_pattern(pattern, None, &val!(ValueType::$variant(i)), true)?;
                                    match self.evaluate_expression(body)? {
                                        break_value if matches!(break_value.borrow().inner(), ValueType::Return(_)) => {
                                            self.env.exit_scope();
                                            return Ok(break_value);
                                        }
                                        break_value if matches!(break_value.borrow().inner(), ValueType::Break(_, _)) => {
                                            self.env.exit_scope();
                                            let (break_label, value) = if let ValueType::Break(label, value) = break_value.borrow().inner() {
                                                (label, value)
                                            } else {
                                                unreachable!()
                                            };
                                            match (label, &break_label) {
                                                (Some(loop_label), Some(break_label)) if loop_label == break_label => {
                                                    return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                                                }
                                                (None, None) => {
                                                    return Ok(value.unwrap_or_else(|| val!(ValueType::Unit)));
                                                }
                                                (_, Some(_)) => return Ok(break_value),
                                                (Some(_), None) => return Ok(break_value),
                                            }
                                        }
                                        continue_value if matches!(continue_value.borrow().inner(), ValueType::Continue(_)) => {
                                            self.env.exit_scope();
                                            let continue_label = if let ValueType::Continue(label) = continue_value.borrow().inner() {
                                                label
                                            } else {
                                                unreachable!()
                                            };
                                            match (label, &continue_label) {
                                                (Some(loop_label), Some(continue_label)) if loop_label == continue_label => continue,
                                                (None, None) => continue,
                                                (_, Some(_)) => return Ok(continue_value),
                                                (Some(_), None) => continue,
                                            }
                                        }
                                        _ => {
                                            self.env.exit_scope();
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                    }};
                }

                match (start_val, end_val) {
                    (ValueType::Unbounded, ValueType::I8(e)) => handle_range!(None, Some(e), I8),
                    (ValueType::I8(s), ValueType::I8(e)) => handle_range!(Some(s), Some(e), I8),

                    (ValueType::Unbounded, ValueType::I16(e)) => handle_range!(None, Some(e), I16),
                    (ValueType::I16(s), ValueType::I16(e)) => handle_range!(Some(s), Some(e), I16),

                    (ValueType::Unbounded, ValueType::I32(e)) => handle_range!(None, Some(e), I32),
                    (ValueType::I32(s), ValueType::I32(e)) => handle_range!(Some(s), Some(e), I32),

                    (ValueType::Unbounded, ValueType::I64(e)) => handle_range!(None, Some(e), I64),
                    (ValueType::I64(s), ValueType::I64(e)) => handle_range!(Some(s), Some(e), I64),

                    (ValueType::Unbounded, ValueType::I128(e)) => handle_range!(None, Some(e), I128),
                    (ValueType::I128(s), ValueType::I128(e)) => handle_range!(Some(s), Some(e), I128),

                    (ValueType::Unbounded, ValueType::ISize(e)) => handle_range!(None, Some(e), ISize),
                    (ValueType::ISize(s), ValueType::ISize(e)) => handle_range!(Some(s), Some(e), ISize),

                    (ValueType::Unbounded, ValueType::U8(e)) => handle_range!(None, Some(e), U8),
                    (ValueType::U8(s), ValueType::U8(e)) => handle_range!(Some(s), Some(e), U8),

                    (ValueType::Unbounded, ValueType::U16(e)) => handle_range!(None, Some(e), U16),
                    (ValueType::U16(s), ValueType::U16(e)) => handle_range!(Some(s), Some(e), U16),

                    (ValueType::Unbounded, ValueType::U32(e)) => handle_range!(None, Some(e), U32),
                    (ValueType::U32(s), ValueType::U32(e)) => handle_range!(Some(s), Some(e), U32),

                    (ValueType::Unbounded, ValueType::U64(e)) => handle_range!(None, Some(e), U64),
                    (ValueType::U64(s), ValueType::U64(e)) => handle_range!(Some(s), Some(e), U64),

                    (ValueType::Unbounded, ValueType::U128(e)) => handle_range!(None, Some(e), U128),
                    (ValueType::U128(s), ValueType::U128(e)) => handle_range!(Some(s), Some(e), U128),

                    (ValueType::Unbounded, ValueType::USize(e)) => handle_range!(None, Some(e), USize),
                    (ValueType::USize(s), ValueType::USize(e)) => handle_range!(Some(s), Some(e), USize),

                    _ => return Err("Invalid range type".to_string()),
                }
            }

            _ => return Err("Expression is not iterable".to_string()),
        }

        Ok(val!(ValueType::Unit))
    }
}
