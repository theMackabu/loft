use super::*;

impl<'st> Interpreter {
    pub fn handle_enum_def(&mut self, name: &str, variants: Vec<EnumVariant>) -> Result<(), String> {
        let enum_def = val!(mut ValueType::EnumDef {
            name: name.to_string(),
            variants: variants.clone(),
            methods: HashMap::new(),
        });

        self.env.declare_enum(name, enum_def)?;

        for variant in variants {
            match variant {
                EnumVariant::Simple(variant_name) => {
                    let constructor = val!(ValueType::EnumConstructor {
                        fields: Vec::new(),
                        enum_name: name.to_string(),
                        variant_name: variant_name.clone(),
                    });

                    let full_path = format!("{}::{}", name, variant_name);
                    self.env.scope_resolver.declare_variable(&full_path, false);
                    self.env.set_variable(&full_path, constructor)?;
                }

                EnumVariant::Tuple(variant_name, fields) => {
                    let constructor = val!(ValueType::EnumConstructor {
                        fields,
                        enum_name: name.to_string(),
                        variant_name: variant_name.clone(),
                    });

                    let full_path = format!("{}::{}", name, variant_name);
                    self.env.scope_resolver.declare_variable(&full_path, false);
                    self.env.set_variable(&full_path, constructor)?;
                }

                EnumVariant::Struct(variant_name, fields) => {
                    let constructor = val!(ValueType::EnumStructConstructor {
                        fields,
                        enum_name: name.to_string(),
                        variant_name: variant_name.clone(),
                    });

                    let full_path = format!("{}::{}", name, variant_name);
                    self.env.scope_resolver.declare_variable(&full_path, false);
                    self.env.set_variable(&full_path, constructor)?;
                }
            }
        }

        Ok(())
    }

    pub fn handle_enum_constructor_call(&mut self, arguments: &Vec<Expr>, enum_name: String, variant_name: String, fields: Vec<Type>) -> Result<Value, String> {
        if arguments.len() != fields.len() {
            return Err(format!("Expected {} arguments for enum variant {}, got {}", fields.len(), variant_name, arguments.len()));
        }

        let mut arg_values = Vec::new();
        for arg in arguments {
            arg_values.push(self.evaluate_expression(arg)?);
        }

        Ok(val!(ValueType::Enum {
            enum_type: enum_name,
            variant: variant_name,
            data: Some(arg_values),
        }))
    }

    pub fn handle_enum_struct_call(&mut self, arguments: &Vec<Expr>, enum_name: String, variant_name: String, fields: HashMap<String, (Type, bool)>) -> Result<Value, String> {
        if arguments.len() != 1 {
            return Err(format!(
                "Expected 1 argument (field initializer) for struct-like enum variant {}, got {}",
                variant_name,
                arguments.len()
            ));
        }

        if let Expr::StructInit { fields: init_fields, .. } = &arguments[0] {
            for field_name in fields.keys() {
                if !init_fields.contains_key(field_name) {
                    return Err(format!("Missing field '{}' in struct-like enum initialization", field_name));
                }
            }

            let mut field_values = Vec::with_capacity(fields.len());
            for field_name in fields.keys() {
                let expr = &init_fields.get(field_name).expect("Expected field to exist").0;
                field_values.push(self.evaluate_expression(expr)?);
            }

            return Ok(val!(ValueType::Enum {
                enum_type: enum_name.clone(),
                variant: variant_name.clone(),
                data: Some(field_values),
            }));
        } else {
            return Err(format!("Expected struct initializer for struct-like enum variant {}", variant_name));
        }
    }
}
