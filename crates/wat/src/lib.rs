#![allow(clippy::print_stdout)]
use oxc_allocator::Allocator;
pub mod console;
use oxc_ast::ast::{
    self, AssignmentExpression, BinaryExpression, BooleanLiteral, CallExpression, Expression,
    ExpressionStatement, ForStatement, ForStatementInit, IdentifierReference, NumericLiteral,
    ParenthesizedExpression, Statement, StringLiteral, UnaryExpression, UpdateExpression,
    VariableDeclaration, VariableDeclarationKind,
};
use oxc_parser::{Parser, ParserReturn};
use oxc_span::SourceType;
use oxc_syntax::
    operator::{self, AssignmentOperator, BinaryOperator};
use wasmtime::{Engine, Instance, Module, Store};
use std::{
    collections::HashMap, fs, ops::{Add, Div, Mul, Sub}
};

pub enum Instruction {
    Const(f64),
    IntConst(i32),
    Call(String),
    I32LessThan,
    I32Equal,
    LocalGet(String),
    I32store8,
    LocalSet(String),
    GetGlobal(String),
    TeeGlobal(String),
    SetGlobal(String),
    I32Add,
    I32Sub,
    F64Add,
    F64Sub,
    F32Add,
    I32TruncF64,
    Loop(String, Vec<Instruction>),
    F64ConvertI32,
    Block(String, Vec<Instruction>),
    Mul,
    Sub,
    If(Vec<Instruction>, Option<Vec<Instruction>>),
    Drop,
    Br(String),
    Div,
    Lt,
    Eq,
    Gt,
    BrIf(String),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_with_indent(
            statements: &[Instruction],
            f: &mut std::fmt::Formatter<'_>,
            indent_level: usize,
        ) -> std::fmt::Result {
            let indent = " ".repeat(indent_level);
            for statement in statements {
                writeln!(f, "{}{}", indent, statement)?;
            }
            Ok(())
        }

        match self {
            Self::Const(num) => write!(f, "f64.const {}", num),
            Self::IntConst(num) => write!(f, "i32.const {}", num),
            Self::F32Add => write!(f, "f32.add"),
            Self::F64Add => write!(f, "f64.add"),
            Self::I32Add => write!(f, "i32.add"),
            Self::Mul => write!(f, "f64.mul"),
            Self::Sub => write!(f, "f64.sub"),
            Self::Div => write!(f, "f64.div"),
            Self::Gt => write!(f, "f64.gt"),
            Self::SetGlobal(variable) => write!(f, "global.set ${}", variable),
            Self::GetGlobal(variable) => write!(f, "global.get ${}", variable),
            Self::TeeGlobal(variable) => write!(f, "global.tee ${}", variable),
            Self::Block(label, statements) => {
                writeln!(f, "(block ${label}        ")?;
                fmt_with_indent(statements, f, 8)?;
                write!(f, "     )")
            }

            Self::Loop(label, statements) => {
                writeln!(f, "(loop ${label}        ")?;
                fmt_with_indent(statements, f, 8)?;
                write!(f, "     )")
            }
            Self::Lt => write!(f, "f64.lt"),
            Self::If(then, rest) => {
                write!(f, "(if\n  (then\n")?;
                fmt_with_indent(then, f, 4)?;
                write!(f, "  )\n")?;
                if let Some(rest) = rest {
                    write!(f, "  (else \n")?;
                    fmt_with_indent(&rest, f, 4)?;
                    write!(f, "    )\n)")
                } else {
                    write!(f, "      )\n")
                }
            }
            Self::BrIf(label) => write!(f, "br_if ${}", label),
            Self::Call(function) => write!(f, "call ${function}"),
            Self::Br(label) => write!(f, "br ${}", label),
            Self::Drop => write!(f, "drop"),
            Self::Eq => write!(f, "f64.eq"),
            Self::I32TruncF64 => write!(f, "i32.trunc_sat_f64_s"),
            Self::F64ConvertI32 => write!(f, "f64.convert_i32_s"),
            Self::LocalGet(var) => write!(f, "local.get ${}", var),
            Self::LocalSet(var) => write!(f, "local.set ${}", var),
            Self::I32store8 => write!(f, "i32.store8"),
            Self::I32LessThan => write!(f, "i32.lt_s"),
            Self::I32Sub => write!(f, "i32.sub"),
            Self::F64Sub => write!(f, "f64.sub"),
            Self::I32Equal => write!(f, "i32.eq"),
        }
    }
}

const TAG_INT: u64 = 0b000;
const TAG_FLOAT: u64 = 0b001;
const TAG_STRING: u64 = 0b010;
const TAG_OBJECT: u64 = 0b011;
const TAG_BOOLEAN: u64 = 0b100;
const TAG_UNDEFINED: u64 = 0b101;
const TAG_NULL: u64 = 0b110;
const TAG_SYMBOL: u64 = 0b111;

struct JsValue(u64);

impl JsValue {
    fn new(tag: u64, value: u64) -> Self {
        Self((tag << 61) | value)
    }

    fn tag(&self) -> u64 {
        self.0 >> 61
    }

    fn value(&self) -> u64 {
        self.0 & 0x1FFFFFFFFFFFFFFF
    }

    fn is_string(&self) -> bool {
        self.tag() == TAG_STRING
    }

    fn is_integer(&self) -> bool {
        self.tag() == TAG_INT
    }

    fn is_float(&self) -> bool {
        self.tag() == TAG_FLOAT
    }

    fn is_object(&self) -> bool {
        self.tag() == TAG_OBJECT
    }

    fn is_boolean(&self) -> bool {
        self.tag() == TAG_BOOLEAN
    }
}

impl Add for JsValue {
    type Output = JsValue;
    fn add(self, rhs: Self) -> Self::Output {
        if self.is_integer() && rhs.is_integer() {
            Self::new(TAG_INT, self.value() + rhs.value())
        } else if self.is_float() && rhs.is_float() {
            Self::new(TAG_FLOAT, self.value() + rhs.value())
        } else if self.is_string() && rhs.is_string() {
            Self::new(TAG_STRING, self.value() + rhs.value())
        } else {
            unimplemented!()
        }
    }
}

impl Sub for JsValue {
    type Output = JsValue;
    fn sub(self, rhs: Self) -> Self::Output {
        if self.is_integer() && rhs.is_integer() {
            Self::new(TAG_INT, self.value() - rhs.value())
        } else if self.is_float() && rhs.is_float() {
            Self::new(TAG_FLOAT, self.value() - rhs.value())
        } else {
            unimplemented!()
        }
    }
}

impl Mul for JsValue {
    type Output = JsValue;
    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_integer() && rhs.is_integer() {
            Self::new(TAG_INT, self.value() * rhs.value())
        } else if self.is_float() && rhs.is_float() {
            Self::new(TAG_FLOAT, self.value() * rhs.value())
        } else {
            unimplemented!()
        }
    }
}

impl Div for JsValue {
    type Output = JsValue;
    fn div(self, rhs: Self) -> Self::Output {
        if self.is_integer() && rhs.is_integer() {
            Self::new(TAG_INT, self.value() / rhs.value())
        } else if self.is_float() && rhs.is_float() {
            Self::new(TAG_FLOAT, self.value() / rhs.value())
        } else {
            unimplemented!()
        }
    }
}

#[derive(PartialEq, Debug)]
enum ValueType {
    String,
    Integer,
    Float,
    Identifier,
}

pub struct CompileContext {
    instructions: Vec<Instruction>,
    data: Vec<String>,
    stack: Vec<ValueType>,
    block_index: usize,
    label_index: usize,
    current_offset: usize,
    globals: HashMap<String, JsValue>,
}

impl Default for CompileContext {
    fn default() -> Self {
        Self {
            instructions: vec![],
            data: vec![],
            label_index: 0,
            block_index: 0,
            current_offset: 1,
            globals: HashMap::new(),
            stack: vec![],
        }
    }
}

impl CompileContext {
    fn emit_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn new_block(&mut self) -> String {
        let block = format!("block_{}", self.block_index);
        self.block_index += 1;
        block
    }

    fn current_block(&self) -> String {
        format!("block_{}", self.block_index - 1)
    }

    fn emit_data(&mut self, str_value: String) {
        let utf16_bytes: Vec<u8> = str_value
            .encode_utf16()
            .flat_map(|unit| vec![(unit >> 8) as u8, (unit & 0xFF) as u8])
            .collect();

        let length_prefix = utf16_bytes.len() as u8; // Adjust as needed for larger strings
        let prefixed_str_value = format!(
            "\\{:02x}{}",
            length_prefix,
            utf16_bytes
                .iter()
                .map(|byte| format!("\\{:02x}", byte))
                .collect::<String>()
        );

        self.data.push(format!(
            "(data (i32.const {}) \"{}\")",
            self.current_offset, prefixed_str_value
        ));

        self.current_offset += 20 + prefixed_str_value.len();
    }

    pub fn compile_statements(&mut self, stmts: &[Statement]) {
        let iter = stmts.iter();

        for stmt in iter {
            stmt.compile(self);
        }
    }

    fn format_globals(&self) -> String {
        let mut globals = String::new();
        globals.push_str(&format!(
            "(global $alloc_offset (mut i32) (i32.const 1024))"
        ));
        for (name, value) in &self.globals {
            if value.is_float() {
                globals.push_str(&format!(
                    "(global ${} (mut f64) (f64.const {}))\n",
                    name, value.value()
                ));
            } else if value.is_integer() || value.is_string() {
                globals.push_str(&format!(
                    "(global ${} (mut i32) (i32.const {}))\n",
                    name, value.value()
                ));
            } 
        }
        globals
    }

    fn new_label(&mut self) -> String {
        let label = format!("label_{}", self.label_index);
        self.label_index += 1;
        label
    }

    pub fn finish(&self) -> String {
        let data_segments = self.data.join("\n");
        let template = include_str!("../template.wat");

        let instructions: Vec<String> = self
            .instructions
            .iter()
            .map(|instruction| format!(r#"    {}"#, instruction))
            .collect();

        let instructions = instructions.join("\n");
        let template = template
            .replace("{globals}", self.format_globals().as_str())
            .replace("{data_sections}", data_segments.as_str())
            .replace("{main_func}", instructions.as_str());

        template
    }
}

trait CompileEvaluation {
    fn compile(&self, ctx: &mut CompileContext);
}
impl CompileEvaluation for NumericLiteral<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        if self.raw.contains(".") {
            ctx.stack.push(ValueType::Float);
            ctx.emit_instruction(Instruction::Const(self.value));
        } else {
            ctx.stack.push(ValueType::Integer);
            ctx.emit_instruction(Instruction::IntConst(self.value as i32));
        }
    }
}

impl CompileEvaluation for StringLiteral<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        let offset = ctx.current_offset as i32;
        ctx.emit_data(self.value.to_string());
        ctx.emit_instruction(Instruction::IntConst(offset as i32));

        ctx.stack.push(ValueType::String);
    }
}

impl CompileEvaluation for ParenthesizedExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        self.expression.compile(ctx);
    }
}

impl CompileEvaluation for BooleanLiteral {
    fn compile(&self, ctx: &mut CompileContext) {}
}

impl CompileEvaluation for CallExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        if let Expression::Identifier(ident) = &self.callee {
            for arg in &self.arguments {
                arg.to_expression().compile(ctx);
            }
            if ident.name == "print_int" {
                ctx.emit_instruction(Instruction::Call("print_number".into()));
            }

            if ident.name == "print_float" {
                ctx.emit_instruction(Instruction::Call("print_float".into()));
            }
        }
        if let Expression::StaticMemberExpression(static_member) = &self.callee {
            static_member.object.compile(ctx);
            for arg in &self.arguments {
                arg.to_expression().compile(ctx);
            }

            if static_member.object.is_string_literal() {
                if static_member.property.name == "toLowerCase" {
                    ctx.emit_instruction(Instruction::Call("lowercase".into()));
                }
                if static_member.property.name == "toUpperCase" {
                    ctx.emit_instruction(Instruction::Call("uppercase".into()));
                }

                if static_member.property.name == "substring" {
                    ctx.emit_instruction(Instruction::Call("substring".into()));
                }

                if static_member.property.name == "indexOf" {
                    ctx.emit_instruction(Instruction::IntConst(0));
                    ctx.emit_instruction(Instruction::Call("indexOf".into()));
                }

                if static_member.property.name == "slice" {
                    ctx.emit_instruction(Instruction::Call("substring".into()));
                }
            }

            if let Expression::Identifier(obj) = &static_member.object {
                if obj.name == "console" && static_member.property.name == "log" {
                    ctx.emit_instruction(Instruction::Call("print".into()));
                }
            }
        }
    }
}

impl CompileEvaluation for BinaryExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        self.left.compile(ctx);
        self.right.compile(ctx);
        let left = ctx.stack.pop();
        let right = ctx.stack.pop();

        match self.operator {
            BinaryOperator::Addition => {
                if let (Some(left), Some(right)) = (left, right) {
                    match (left, right) {
                        (ValueType::String, ValueType::String) => {
                            ctx.emit_instruction(Instruction::Call("concatenate_strings".into()));
                            ctx.stack.push(ValueType::String);
                        }
                        (ValueType::Integer, ValueType::Integer) => {
                            ctx.emit_instruction(Instruction::I32Add);
                            ctx.stack.push(ValueType::Integer);
                        }
                        (ValueType::Float, ValueType::Float) => {
                            ctx.emit_instruction(Instruction::F64Add);
                            ctx.stack.push(ValueType::Float)
                        }
                        (ValueType::Integer, ValueType::Float) => {
                            ctx.emit_instruction(Instruction::F64ConvertI32);
                            ctx.emit_instruction(Instruction::F64Add);
                            ctx.stack.push(ValueType::Float)
                        }
                        (ValueType::Identifier, ValueType::Identifier) => {
                            ctx.emit_instruction(Instruction::I32Add);
                        }
                        _ => todo!(),
                    }
                }
            }
            BinaryOperator::Subtraction => {
                if let (Some(left), Some(right)) = (left, right) {
                    match (left, right) {
                        (ValueType::Integer, ValueType::Integer) => {
                            ctx.emit_instruction(Instruction::I32Sub);
                            ctx.stack.push(ValueType::Integer);
                        }
                        (ValueType::Float, ValueType::Float) => {
                            ctx.emit_instruction(Instruction::F64Sub);
                            ctx.stack.push(ValueType::Float)
                        }
                        (ValueType::Integer, ValueType::Float) => {
                            ctx.emit_instruction(Instruction::F64ConvertI32);
                            ctx.emit_instruction(Instruction::F64Sub);
                            ctx.stack.push(ValueType::Float)
                        }
                        _ => todo!(),
                    }
                }
            }
            BinaryOperator::LessThan => {
                if let (Some(left), Some(right)) = (left, right) {
                    match (left, right) {
                        (ValueType::Integer, ValueType::Integer) => {
                            ctx.emit_instruction(Instruction::I32LessThan);
                            ctx.stack.push(ValueType::Integer);
                        }
                        (ValueType::Float, ValueType::Float) => {
                            ctx.emit_instruction(Instruction::Lt);
                            ctx.stack.push(ValueType::Float)
                        }
                        (ValueType::Integer, ValueType::Float) => {
                            ctx.emit_instruction(Instruction::F64ConvertI32);
                            ctx.emit_instruction(Instruction::I32LessThan);
                            ctx.stack.push(ValueType::Float)
                        }
                        (ValueType::Integer, ValueType::Identifier) => {
                            ctx.emit_instruction(Instruction::I32LessThan);
                        }

                        x => todo!("{:#?}", x),
                    }
                }
            }
            BinaryOperator::Multiplication => {
                ctx.emit_instruction(Instruction::Mul);
            }
            BinaryOperator::Equality => {
                if let (Some(left), Some(right)) = (left, right) {
                    match (left, right) {
                        (ValueType::Integer, ValueType::Integer) => {
                            ctx.emit_instruction(Instruction::I32Equal);
                            ctx.stack.push(ValueType::Integer);
                        }
                        (ValueType::Float, ValueType::Float) => {
                            ctx.emit_instruction(Instruction::Eq);
                            ctx.stack.push(ValueType::Float)
                        }
                        (ValueType::Integer, ValueType::Float) => {
                            ctx.emit_instruction(Instruction::F64ConvertI32);
                            ctx.emit_instruction(Instruction::Eq);
                            ctx.stack.push(ValueType::Float)
                        }
                        _ => todo!(),
                    }
                }
            }
            something => unimplemented!("{:#?}", something),
        }
    }
}

impl CompileEvaluation for UnaryExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        if let Expression::StringLiteral(_) = &self.argument {
            let offset = ctx.current_offset as i32;
            ctx.emit_data("NaN".into());
            ctx.emit_instruction(Instruction::IntConst(offset as i32));
            ctx.stack.push(ValueType::String);
        }
    }
}

impl CompileEvaluation for IdentifierReference<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        let identifier = self.name.to_string();
        if ctx.globals.contains_key(&identifier) {
            ctx.emit_instruction(Instruction::GetGlobal(identifier));
            ctx.stack.push(ValueType::Identifier);
        }
    }
}

impl CompileEvaluation for UpdateExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        let identifier = self.argument.get_identifier().unwrap();
        ctx.emit_instruction(Instruction::GetGlobal(identifier.into()));
        ctx.emit_instruction(Instruction::IntConst(1));
        match self.operator {
            operator::UpdateOperator::Increment => ctx.emit_instruction(Instruction::I32Add),
            operator::UpdateOperator::Decrement => ctx.emit_instruction(Instruction::Sub),
        }
        ctx.emit_instruction(Instruction::SetGlobal(identifier.into()));
    }
}

impl CompileEvaluation for ast::ObjectExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        todo!()
    }
}

impl CompileEvaluation for ast::TemplateLiteral<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        todo!()
    }
}

impl CompileEvaluation for ast::ArrayExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {}
}

impl CompileEvaluation for ast::Expression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        match self {
            Expression::BinaryExpression(x) => x.compile(ctx),
            Expression::StringLiteral(x) => x.compile(ctx),
            Expression::NumericLiteral(x) => x.compile(ctx),
            Expression::Identifier(x) => x.compile(ctx),
            Expression::ParenthesizedExpression(x) => x.compile(ctx),
            Expression::CallExpression(x) => x.compile(ctx),
            Expression::UnaryExpression(x) => x.compile(ctx),
            Expression::AssignmentExpression(x) => x.compile(ctx),
            Expression::UpdateExpression(x) => x.compile(ctx),
            Expression::ObjectExpression(x) => x.compile(ctx),
            Expression::TemplateLiteral(x) => x.compile(ctx),
            something => todo!(),
        }
    }
}

impl CompileEvaluation for ExpressionStatement<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        self.expression.compile(ctx);
    }
}

impl CompileEvaluation for VariableDeclaration<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        match self.kind {
            VariableDeclarationKind::Var => {
                for decl in &self.declarations {
                    let ast::BindingPatternKind::BindingIdentifier(identifier) = &decl.id.kind
                    else {
                        return;
                    };

                    let Some(init) = &decl.init else {
                        return;
                    };

                    match init {
                        Expression::NumericLiteral(num) => {
                            if num.raw.contains(".") {
                                ctx.globals.insert(
                                    identifier.name.to_string(),
                                    JsValue::new(TAG_FLOAT, num.value as u64),
                                );
                            } else {
                                ctx.globals.insert(
                                    identifier.name.to_string(),
                                    JsValue::new(TAG_INT, num.value as u64),
                                );
                            }
                        }
                        Expression::StringLiteral(literal) => {
                            let offset = ctx.current_offset as i32;
                            ctx.globals.insert(
                                identifier.name.to_string(),
                                JsValue::new(TAG_STRING, offset as u64),
                            );
                            literal.compile(ctx);
                            ctx.emit_instruction(Instruction::SetGlobal(
                                identifier.name.to_string(),
                            ));
                        }
                        Expression::CallExpression(x) => x.compile(ctx),
                        something => unimplemented!("{:#?}", something),
                    }
                }
            }
            VariableDeclarationKind::Let | VariableDeclarationKind::Const => {
                for decl in &self.declarations {
                    let ast::BindingPatternKind::BindingIdentifier(identifier) = &decl.id.kind
                    else {
                        return;
                    };

                    let Some(init) = &decl.init else {
                        return;
                    };

                    match init {
                        Expression::NumericLiteral(num) => {
                            if num.raw.contains(".") {
                                ctx.globals.insert(
                                    identifier.name.to_string(),
                                    JsValue::new(TAG_FLOAT, num.value as u64),
                                );
                            } else {
                                ctx.globals.insert(
                                    identifier.name.to_string(),
                                    JsValue::new(TAG_INT, num.value as u64),
                                );
                            }
                        }
                        Expression::StringLiteral(literal) => {
                            let offset = ctx.current_offset as i32;
                            ctx.globals.insert(
                                identifier.name.to_string(),
                                JsValue::new(TAG_STRING, offset as u64),
                            );
                            literal.compile(ctx);
                        }
                        Expression::CallExpression(expr) => {
                            ctx.globals.insert(
                                identifier.name.to_string(),
                                JsValue::new(TAG_INT, ctx.current_offset as u64),
                            );
                            expr.compile(ctx);
                            ctx.emit_instruction(Instruction::SetGlobal(
                                identifier.name.to_string(),
                            ));
                        }
                        _ => init.compile(ctx),
                    }
                }
            }
        }
    }
}

impl CompileEvaluation for AssignmentExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        let ast::AssignmentTarget::AssignmentTargetIdentifier(identifier) = &self.left else {
            todo!();
        };

        if self.operator == AssignmentOperator::Addition {
            // Assignment addition is basically increment
            identifier.compile(ctx);
            self.right.compile(ctx);
            ctx.emit_instruction(Instruction::I32Add);
            ctx.emit_instruction(Instruction::SetGlobal(identifier.name.to_string()));
        }
    }
}

impl CompileEvaluation for ForStatement<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        let label = ctx.new_label();

        let body = &self.body;

        let Some(ForStatementInit::VariableDeclaration(init)) = &self.init else {
            return;
        };
        init.compile(ctx);
        let instructions_len = &ctx.instructions.len();
        body.compile(ctx);

        if let Some(update) = &self.update {
            update.compile(ctx);
        }

        if let Some(test) = &self.test {
            test.compile(ctx);
            ctx.emit_instruction(Instruction::BrIf(label.clone()));
        }
        if ctx.block_index > 0 {
            let instructions = ctx.instructions.split_off(*instructions_len);
            ctx.emit_instruction(Instruction::Loop(label, instructions));
            let instructions = ctx.instructions.split_off(*instructions_len);
            ctx.emit_instruction(Instruction::Block(ctx.current_block(), instructions));
        } else {
            let instructions = ctx.instructions.split_off(*instructions_len);
            ctx.emit_instruction(Instruction::Loop(label, instructions));
        }
    }
}

impl CompileEvaluation for ast::IfStatement<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        self.test.compile(ctx);
        let instructions_len = ctx.instructions.len();
        self.consequent.compile(ctx);
        let true_condition = ctx.instructions.split_off(instructions_len);
        if let Some(alternate) = &self.alternate {
            let instructions_len = ctx.instructions.len();
            alternate.compile(ctx);
            let else_condition = ctx.instructions.split_off(instructions_len);
            ctx.emit_instruction(Instruction::If(true_condition, Some(else_condition)));
        } else {
            ctx.emit_instruction(Instruction::If(true_condition, None));
        }
    }
}

impl CompileEvaluation for ast::BlockStatement<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        self.body.iter().for_each(|b| {
            b.compile(ctx);
        });
    }
}

impl CompileEvaluation for ast::BreakStatement<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        let new_block = ctx.new_block();
        ctx.emit_instruction(Instruction::Br(new_block));
    }
}

impl CompileEvaluation for ast::Statement<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        match self {
            Statement::ExpressionStatement(x) => x.compile(ctx),
            Statement::VariableDeclaration(x) => x.compile(ctx),
            Statement::ForStatement(x) => x.compile(ctx),
            Statement::IfStatement(x) => x.compile(ctx),
            Statement::BlockStatement(x) => x.compile(ctx),
            Statement::BreakStatement(x) => x.compile(ctx),
            _ => todo!(),
        }
    }
}

pub fn compile(path: &str) ->  Result<String, Box<dyn std::error::Error>> 
 {
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();
    let source = std::fs::read_to_string(path).unwrap();
    let ParserReturn {
        program,
        errors,
        ..
    } = Parser::new(&allocator, &source, source_type).parse();
    assert_eq!(errors.len(), 0);
    let mut compiler = CompileContext::default();
    compiler.compile_statements(&program.body);
    let output = compiler.finish();
    fs::write("random.wat", &output)?;
    let engine = Engine::default();
    let module = Module::new(&engine, &output)?;
    let mut store = Store::new(&engine, ());
    let print_num = console::print_num(&mut store);
    let print_float = console::print_float(&mut store);
    let print_str = console::print_str(&mut store);
    let instance = Instance::new(
        &mut store,
        &module,
        &[print_str.into(), print_num.into(), print_float.into()],
    )?;
    let main_func = instance.get_typed_func::<(), ()>(&mut store, "main")?;

    main_func.call(&mut store, ())?;
    Ok(output)
}
