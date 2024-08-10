use std::{collections::HashMap, fmt::Display};

use oxc_ast::{
    ast::{Expression, NumericLiteral, Statement},
    Visit,
};
use oxc_syntax::operator::BinaryOperator;

use super::runtime::ByteCode;

#[derive(Default, Debug)]
pub struct WasmGenerator {
    pub bytecodes: Vec<ByteCode>,
    index: usize,
    label_index: usize,
    globals: HashMap<String, f64>,
}

impl WasmGenerator {
    fn emit_bytecode(&mut self, bytecode: ByteCode) {
        self.bytecodes.push(bytecode);
    }

    fn format_globals(&self) -> String {
        let mut globals = String::new();
        for (name, value) in &self.globals {
            globals.push_str(&format!(
                "(global ${} (mut f64) (f64.const {}))\n",
                name, value
            ));
        }
        globals
    }

    pub fn print_bytecode(&self) {
        println!(
            "(module {} (func (export \"main\") (result f64)",
            self.format_globals()
        );
        for bytecode in &self.bytecodes {
            println!(r#"    {}"#, bytecode.to_string());
        }
        print!("  )\n)");
    }

    fn advance(&mut self) -> usize {
        let index = self.index;
        self.index += 1;
        index
    }

    fn new_label(&mut self) -> String {
        let label = format!("label_{}", self.label_index);
        self.label_index += 1;
        label
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> f64 {
        match expr {
            Expression::NumericLiteral(literal) => {
                self.visit_numeric_literal(literal);
                literal.raw.parse::<f64>().unwrap_or(0.)
            }
            Expression::ParenthesizedExpression(expr) => self.evaluate_expression(&expr.expression),
            Expression::BinaryExpression(binary) => {
                let left = self.evaluate_expression(&binary.left);
                let right = self.evaluate_expression(&binary.right);
                match binary.operator {
                    BinaryOperator::Addition => {
                        self.emit_bytecode(ByteCode::Add);
                        left + right
                    }
                    BinaryOperator::Subtraction => {
                        self.emit_bytecode(ByteCode::Sub);
                        left - right
                    }
                    BinaryOperator::Multiplication => {
                        self.emit_bytecode(ByteCode::Mul);
                        left * right
                    }
                    BinaryOperator::Division => {
                        self.emit_bytecode(ByteCode::Div);
                        left / right
                    }
                    _ => 0.0,
                }
            }
            _ => 0.0,
        }
    }
}

impl Display for WasmGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for bytecode in &self.bytecodes {
            write!(f, "{:?}\n", bytecode)?;
        }
        Ok(())
    }
}

impl<'a> Visit<'a> for WasmGenerator {
    fn visit_numeric_literal(&mut self, it: &NumericLiteral<'a>) {
        let value = it.raw.parse::<i32>().unwrap();
        self.emit_bytecode(ByteCode::Const(value));
    }

    fn visit_variable_declarator(&mut self, it: &oxc_ast::ast::VariableDeclarator<'a>) {
        let identifier = it.id.get_identifier().unwrap().into_string();
        match &it.init {
            Some(init) => {
                let value = self.evaluate_expression(&init);
                self.globals.insert(identifier.clone(), value);
            }
            None => {
                self.globals.insert(identifier.clone(), 0.);
            }
        }

        let index = self.advance();
        self.emit_bytecode(ByteCode::SetGlobal(index, identifier));
    }

    fn visit_binary_expression(&mut self, it: &oxc_ast::ast::BinaryExpression<'a>) {
        if it.left.is_identifier_reference() {
            let identifier = it.left.get_identifier_reference().unwrap();
            let identifer = identifier.name.to_string();
            self.emit_bytecode(ByteCode::GetGlobal(identifer));
        }
        self.evaluate_expression(&it.right);
        match it.operator {
            BinaryOperator::Addition => {
                self.emit_bytecode(ByteCode::Add);
            }
            BinaryOperator::Subtraction => {
                self.emit_bytecode(ByteCode::Sub);
            }
            BinaryOperator::Multiplication => {
                self.emit_bytecode(ByteCode::Mul);
            }
            BinaryOperator::Division => {
                self.emit_bytecode(ByteCode::Div);
            }
            BinaryOperator::LessThan => {
                self.emit_bytecode(ByteCode::Lt);
            }
            _ => {}
        }
    }

    fn visit_identifier_reference(&mut self, it: &oxc_ast::ast::IdentifierReference<'a>) {
        let identifier = it.name.to_string();
        self.emit_bytecode(ByteCode::GetGlobal(identifier));
    }

    fn visit_update_expression(&mut self, it: &oxc_ast::ast::UpdateExpression<'a>) {
        let identifier = it.argument.get_identifier().unwrap();
        let value = &self.globals.get(&identifier.to_string()).cloned();
        self.emit_bytecode(ByteCode::GetGlobal(identifier.into()));
        self.emit_bytecode(ByteCode::Const(1));
        match it.operator {
            oxc_syntax::operator::UpdateOperator::Increment => {
                self.emit_bytecode(ByteCode::Add);
                self.globals.insert(identifier.into(), value.unwrap() + 1.0);
            }
            oxc_syntax::operator::UpdateOperator::Decrement => {
                self.emit_bytecode(ByteCode::Sub);
                self.globals.insert(identifier.into(), value.unwrap() - 1.0);
            }
        }
        let index = self.advance();
        self.emit_bytecode(ByteCode::SetGlobal(index, identifier.into()));
    }

    fn visit_while_statement(&mut self, it: &oxc_ast::ast::WhileStatement<'a>) {
        let label = self.new_label();
        let body = &it.body;
        let bytecodes_len = &self.bytecodes.len();

        match body {
            Statement::BlockStatement(block) => {
                for statement in &block.body {
                    self.visit_statement(statement);
                }
            }
            _ => {}
        }

        if let Expression::BinaryExpression(test) = &it.test {
            self.visit_binary_expression(test);
            self.emit_bytecode(ByteCode::BrIf(label.clone()));
        }

        let bytecodes = self.bytecodes.split_off(*bytecodes_len);
        self.emit_bytecode(ByteCode::Loop(label, bytecodes));
    }
}
