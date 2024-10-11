use oxc_ast::{
    ast::{Expression, ForStatementInit, NumericLiteral, Statement},
    Visit,
};
use oxc_syntax::operator::BinaryOperator;
use std::{collections::HashMap, fmt::Display};

use super::runtime::ByteCode;

#[derive(Default, Debug)]
pub struct WasmGenerator {
    pub bytecodes: Vec<ByteCode>,
    index: usize,
    label_index: usize,
    block_index: usize,
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

    pub fn get_wat(&self) -> String {
        let header = format!(
            "(module  {} (func (export \"main\") (result f64)",
            self.format_globals()
        );

        let bytecodes: Vec<String> = self
            .bytecodes
            .iter()
            .map(|bytecode| format!(r#"    {:#?}"#, bytecode))
            .collect();

        let bytecodes = bytecodes.join("\n");

        format!("{} {} ))", header, bytecodes)
    }

    pub fn print_bytecode(&self) {
        println!(
            "(module \n {} (func (export \"main\") (result f64)",
            self.format_globals()
        );
        for bytecode in &self.bytecodes {
            println!(r#"    {:#?}"#, bytecode);
        }

        print!("    )\n)");
    }

    fn advance(&mut self) -> usize {
        let index = self.index;
        self.index += 1;
        index
    }

    fn current_label(&self) -> String {
        format!("label_{}", self.label_index - 1)
    }

    fn new_block(&mut self) -> String {
        let block = format!("block_{}", self.block_index);
        self.block_index += 1;
        block
    }

    fn current_block(&self) -> String {
        format!("block_{}", self.block_index - 1)
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
            writeln!(f, "{:?}", bytecode)?;
        }
        Ok(())
    }
}

impl<'a> Visit<'a> for WasmGenerator {
    fn visit_break_statement(&mut self, _: &oxc_ast::ast::BreakStatement<'a>) {
        let new_block = self.new_block();
        self.emit_bytecode(ByteCode::Br(new_block));
    }

    fn visit_while_statement(&mut self, it: &oxc_ast::ast::WhileStatement<'a>) {
        let label = self.new_label();
        let body = &it.body;
        let bytecodes_len = &self.bytecodes.len();

        if let Statement::BlockStatement(block) = body {
            for statement in &block.body {
                self.visit_statement(statement);
            }
        }

        if let Expression::BinaryExpression(test) = &it.test {
            self.visit_binary_expression(test);
            self.emit_bytecode(ByteCode::BrIf(label.clone()));
        }

        if self.block_index > 0 {
            let bytecodes = self.bytecodes.split_off(*bytecodes_len);
            self.emit_bytecode(ByteCode::Loop(label, bytecodes));
            let bytecodes = self.bytecodes.split_off(*bytecodes_len);
            self.emit_bytecode(ByteCode::Block(self.current_block(), bytecodes));
        } else {
            let bytecodes = self.bytecodes.split_off(*bytecodes_len);
            self.emit_bytecode(ByteCode::Loop(label, bytecodes));
        }
    }

    fn visit_for_statement(&mut self, it: &oxc_ast::ast::ForStatement<'a>) {
        let label = self.new_label();
        let body = &it.body;
        if let Some(ForStatementInit::VariableDeclaration(init)) = &it.init {
            for declarator in &init.declarations {
                self.visit_variable_declarator(declarator);
            }
        }
        let bytecodes_len = &self.bytecodes.len();
        if let Statement::BlockStatement(block) = body {
            for statement in &block.body {
                self.visit_statement(statement);
            }
        }

        if let Some(update) = &it.update {
            self.visit_expression(update);
        }

        if let Some(test) = &it.test {
            self.visit_expression(test);
            self.emit_bytecode(ByteCode::BrIf(label.clone()));
        }
        if self.block_index > 0 {
            let bytecodes = self.bytecodes.split_off(*bytecodes_len);
            self.emit_bytecode(ByteCode::Loop(label, bytecodes));
            let bytecodes = self.bytecodes.split_off(*bytecodes_len);
            self.emit_bytecode(ByteCode::Block(self.current_block(), bytecodes));
        } else {
            let bytecodes = self.bytecodes.split_off(*bytecodes_len);
            self.emit_bytecode(ByteCode::Loop(label, bytecodes));
        }
    }
    fn visit_numeric_literal(&mut self, it: &NumericLiteral<'a>) {
        let value = it.raw.parse::<f64>().unwrap();
        self.emit_bytecode(ByteCode::Const(value));
    }

    fn visit_if_statement(&mut self, it: &oxc_ast::ast::IfStatement<'a>) {
        self.visit_expression(&it.test);

        let bytecodes_len = self.bytecodes.len();
        self.visit_statement(&it.consequent);
        let true_condition = self.bytecodes.split_off(bytecodes_len);
        let mut else_condition = vec![];
        if let Some(alternate) = &it.alternate {
            let bytecodes_len = self.bytecodes.len();
            self.visit_statement(alternate);
            else_condition = self.bytecodes.split_off(bytecodes_len);
        }
        self.emit_bytecode(ByteCode::If(true_condition, else_condition));
    }

    fn visit_identifier_reference(&mut self, it: &oxc_ast::ast::IdentifierReference<'a>) {
        let identifier = it.name.to_string();
        self.emit_bytecode(ByteCode::GetGlobal(identifier));
    }

    fn visit_expression_statement(&mut self, it: &oxc_ast::ast::ExpressionStatement<'a>) {
        match &it.expression {
            Expression::Identifier(identifier) => {
                self.visit_identifier_reference(identifier);
            }
            Expression::BinaryExpression(binary) => {
                if let Expression::Identifier(_) = binary.left {
                    // TODO: This may be hacky,idk, but it works for now.
                    // It is used when the variable is unused, and only used as expression
                    // ```
                    // 1. let a = 5;
                    // a - 5;
                    // 2 + 5;
                    // ```
                    // Here a is unused, so we can drop it.
                    self.visit_binary_expression(binary);
                    self.emit_bytecode(ByteCode::Drop);
                } else {
                    self.visit_binary_expression(binary);
                }
            }
            _ => {
                self.visit_expression(&it.expression);
            }
        }
    }

    fn visit_variable_declarator(&mut self, it: &oxc_ast::ast::VariableDeclarator<'a>) {
        let identifier = it.id.get_identifier().unwrap().into_string();
        match &it.init {
            Some(init) => {
                if let Expression::StringLiteral(string_literal) = init {
                    self.emit_bytecode(ByteCode::ConstString(string_literal.value.to_string()));
                }
                let value = self.evaluate_expression(init);
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
        if let Expression::Identifier(ident) = &it.left {
            self.visit_identifier_reference(ident);
        }
        self.evaluate_expression(&it.left);
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
            BinaryOperator::GreaterThan => {
                self.emit_bytecode(ByteCode::Gt);
            }
            BinaryOperator::StrictEquality | BinaryOperator::Equality => {
                self.emit_bytecode(ByteCode::Eq);
            }

            _ => {}
        }
    }

    fn visit_assignment_expression(&mut self, it: &oxc_ast::ast::AssignmentExpression<'a>) {
        let identifier = it.left.get_identifier().unwrap();
        self.visit_expression(&it.right);
        let index = self.advance();
        self.emit_bytecode(ByteCode::SetGlobal(index, identifier.into()));
    }

    fn visit_update_expression(&mut self, it: &oxc_ast::ast::UpdateExpression<'a>) {
        let identifier = it.argument.get_identifier().unwrap();
        self.emit_bytecode(ByteCode::GetGlobal(identifier.into()));
        self.emit_bytecode(ByteCode::Const(1.));
        match it.operator {
            oxc_syntax::operator::UpdateOperator::Increment => {
                self.emit_bytecode(ByteCode::Add);
            }
            oxc_syntax::operator::UpdateOperator::Decrement => {
                self.emit_bytecode(ByteCode::Sub);
            }
        }
        let index = self.advance();
        self.emit_bytecode(ByteCode::SetGlobal(index, identifier.into()));
    }
}
