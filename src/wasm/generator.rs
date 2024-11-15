use oxc_ast::ast::{
    self, AssignmentExpression, BinaryExpression, BooleanLiteral, CallExpression, Expression,
    ExpressionStatement, ForStatement, ForStatementInit, IdentifierReference, NumericLiteral,
    ParenthesizedExpression, Statement, StringLiteral, UnaryExpression, UpdateExpression,
    VariableDeclaration, VariableDeclarationKind,
};
use oxc_syntax::operator::{self, AssignmentOperator, BinaryOperator};
use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Sub},
};

pub enum Instruction {
    Const(f64),
    IntConst(i32),
    Call(String),
    I32LessThan,
    LocalGet(String),
    I32store8,
    LocalSet(String),
    GetGlobal(String),
    TeeGlobal(String),
    SetGlobal(String),
    I32Add,
    F64Add,
    F32Add,
    I32TruncF64,
    Loop(String, Vec<Instruction>),
    F64ConvertI32,
    Block(String, Vec<Instruction>),
    Mul,
    Sub,
    If(Vec<Instruction>, Vec<Instruction>),
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
                write!(f, "  )\n  (else\n")?;
                fmt_with_indent(rest, f, 4)?;
                write!(f, "    )\n)")
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
        }
    }
}

#[derive(Debug)]
enum JsValue {
    Integer(i32),
    Float(f64),
    StringLiteral(String),
    StringOffset(i32),
}

impl Add for JsValue {
    type Output = JsValue;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(left), Self::Integer(right)) => Self::Integer(left + right),
            (Self::Float(left), Self::Float(right)) => Self::Float(left + right),
            (Self::StringLiteral(left), Self::StringLiteral(right)) => {
                Self::StringLiteral(format!("{}{}", left, right))
            }
            _ => unimplemented!(),
        }
    }
}

impl Sub for JsValue {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(left), Self::Integer(right)) => Self::Integer(left - right),
            (Self::Float(left), Self::Float(right)) => Self::Float(left - right),
            _ => unimplemented!(),
        }
    }
}

impl Mul for JsValue {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(left), Self::Integer(right)) => Self::Integer(left * right),
            (Self::Float(left), Self::Float(right)) => Self::Float(left * right),
            _ => unimplemented!(),
        }
    }
}

impl Div for JsValue {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(left), Self::Integer(right)) => Self::Integer(left / right),
            (Self::Float(left), Self::Float(right)) => Self::Float(left / right),
            _ => unimplemented!(),
        }
    }
}

impl JsValue {
    fn is_string(&self) -> bool {
        matches!(self, Self::StringLiteral(_) | Self::StringOffset(_))
    }

    fn get_offset(&self) -> Option<&i32> {
        if let JsValue::StringOffset(offset) = self {
            Some(offset)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Debug)]
enum ValueType {
    String,
    Integer,
    Float,
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
        let length_prefix = str_value.len() as u8;
        let prefixed_str_value = format!("\\{:02x}{}", length_prefix, str_value);

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
            match value {
                JsValue::Float(fvalue) => globals.push_str(&format!(
                    "(global ${} (mut f64) (f64.const {}))\n",
                    name, fvalue
                )),
                JsValue::Integer(val) | JsValue::StringOffset(val) => globals.push_str(&format!(
                    "(global ${} (mut i32) (i32.const {}))\n",
                    name, val
                )),
                _ => {}
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
        let template = include_str!("../wat/template.wat");

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
                    ctx.emit_instruction(Instruction::Call("print_str".into()));
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
                        _ => todo!(),
                    }
                }
            }
            BinaryOperator::Subtraction => {
                ctx.emit_instruction(Instruction::Sub);
            }
            BinaryOperator::LessThan => {
                ctx.emit_instruction(Instruction::Lt);
            }
            BinaryOperator::Multiplication => {
                ctx.emit_instruction(Instruction::Mul);
            }
            BinaryOperator::Equality => {
                ctx.emit_instruction(Instruction::Eq);
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
        }
    }
}

impl CompileEvaluation for UpdateExpression<'_> {
    fn compile(&self, ctx: &mut CompileContext) {
        let identifier = self.argument.get_identifier().unwrap();
        ctx.emit_instruction(Instruction::GetGlobal(identifier.into()));
        ctx.emit_instruction(Instruction::Const(1.));
        match self.operator {
            operator::UpdateOperator::Increment => ctx.emit_instruction(Instruction::F64Add),
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
                            let value = num.raw.parse::<f64>().unwrap();
                            ctx.globals
                                .insert(identifier.name.to_string(), JsValue::Float(value));
                        }
                        Expression::StringLiteral(literal) => {
                            let offset = ctx.current_offset as i32;
                            ctx.globals
                                .insert(identifier.name.to_string(), JsValue::StringOffset(offset));
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
                            let value = num.raw.parse::<f64>().unwrap();
                            ctx.globals
                                .insert(identifier.name.to_string(), JsValue::Float(value));
                        }
                        Expression::StringLiteral(literal) => {
                            let offset = ctx.current_offset as i32;
                            ctx.globals
                                .insert(identifier.name.to_string(), JsValue::StringOffset(offset));
                            literal.compile(ctx);
                        }
                        Expression::CallExpression(expr) => {
                            ctx.globals.insert(
                                identifier.name.to_string(),
                                JsValue::StringOffset(ctx.current_offset as i32),
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
        let is_identifier_ref = match &self.left {
            ast::AssignmentTarget::ArrayAssignmentTarget(_) => todo!(),
            ast::AssignmentTarget::AssignmentTargetIdentifier(identifier) => {
                identifier.compile(ctx);
                true
            }
            ast::AssignmentTarget::ComputedMemberExpression(expression) => false,
            ast::AssignmentTarget::ObjectAssignmentTarget(_) => todo!(),
            ast::AssignmentTarget::PrivateFieldExpression(_) => todo!(),
            ast::AssignmentTarget::StaticMemberExpression(expression) => false,
            ast::AssignmentTarget::TSAsExpression(_)
            | ast::AssignmentTarget::TSSatisfiesExpression(_)
            | ast::AssignmentTarget::TSNonNullExpression(_)
            | ast::AssignmentTarget::TSTypeAssertion(_)
            | ast::AssignmentTarget::TSInstantiationExpression(_) => unreachable!(),
        };

        if self.operator == AssignmentOperator::Addition {
            ctx.emit_instruction(Instruction::F64Add);
        }
        self.right.compile(ctx);
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
        if let Statement::BlockStatement(block) = body {
            for statement in &block.body {
                statement.compile(ctx);
            }
        }

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
        let mut else_condition = vec![];
        if let Some(alternate) = &self.alternate {
            let instructions_len = ctx.instructions.len();
            alternate.compile(ctx);
            else_condition = ctx.instructions.split_off(instructions_len);
        }
        ctx.emit_instruction(Instruction::If(true_condition, else_condition));
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
