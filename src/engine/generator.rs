use std::fmt::Display;

use oxc_ast::{
    ast::{Expression, NumericLiteral},
    Visit,
};
use oxc_syntax::operator::BinaryOperator;

use super::runtime::ByteCode;

#[derive(Default, Debug)]
pub struct ByteCodeGenerator {
    pub bytecodes: Vec<ByteCode>,
    register_count: usize,
}

impl ByteCodeGenerator {
    fn emit_bytecode(&mut self, bytecode: ByteCode) {
        self.bytecodes.push(bytecode);
    }

    fn current_register(&self) -> i32 {
        self.register_count as i32
    }

    fn allocate_register(&mut self) -> i32 {
        self.register_count += 1;
        self.register_count as i32
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> f64 {
        match expr {
            Expression::NumericLiteral(literal) => {
                self.visit_numeric_literal(literal);
                literal.raw.parse::<f64>().unwrap()
            }
            Expression::ParenthesizedExpression(expr) => self.evaluate_expression(&expr.expression),
            Expression::BinaryExpression(binary) => {
                let left = self.evaluate_expression(&binary.left);
                let src1 = self.current_register();
                let right = self.evaluate_expression(&binary.right);
                let src2 = self.current_register();
                let dest = self.allocate_register();
                match binary.operator {
                    BinaryOperator::Addition => {
                        self.emit_bytecode(ByteCode::Add(dest, src1, src2));
                        left + right
                    }
                    BinaryOperator::Subtraction => {
                        self.emit_bytecode(ByteCode::Sub(dest, src1, src2));
                        left - right
                    }
                    BinaryOperator::Multiplication => {
                        self.emit_bytecode(ByteCode::Mul(dest, src1, src2));
                        left * right
                    }
                    BinaryOperator::Division => {
                        self.emit_bytecode(ByteCode::Div(dest, src1, src2));
                        left / right
                    }
                    _ => 0.0,
                }
            }
            _ => 0.0,
        }
    }
}

impl Display for ByteCodeGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for bytecode in &self.bytecodes {
            write!(f, "{:?}\n", bytecode)?;
        }
        Ok(())
    }
}

impl<'a> Visit<'a> for ByteCodeGenerator {
    fn visit_numeric_literal(&mut self, it: &NumericLiteral<'a>) {
        let register = self.allocate_register();
        let value = it.raw.parse::<f64>().unwrap();
        self.emit_bytecode(ByteCode::Load(register, value));
    }

    fn visit_binary_expression(&mut self, it: &oxc_ast::ast::BinaryExpression<'a>) {
        self.evaluate_expression(&it.left);
        let src1 = self.current_register();
        self.evaluate_expression(&it.right);
        let src2 = self.current_register();
        let dest = self.allocate_register();
        match it.operator {
            BinaryOperator::Addition => {
                self.emit_bytecode(ByteCode::Add(dest, src1, src2));
            }
            BinaryOperator::Subtraction => {
                self.emit_bytecode(ByteCode::Sub(dest, src1, src2));
            }
            BinaryOperator::Multiplication => {
                self.emit_bytecode(ByteCode::Mul(dest, src1, src2));
            }
            BinaryOperator::Division => {
                self.emit_bytecode(ByteCode::Div(dest, src1, src2));
            }
            _ => {}
        }
    }
}
