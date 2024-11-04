use oxc_ast::{
    ast::{Expression, ForStatementInit, NumericLiteral, Statement},
    Visit,
};
use oxc_syntax::operator::BinaryOperator;
use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

pub enum Instruction {
    Const(f64),
    IntConst(i32),
    Call(String),
    GetGlobal(String),
    SetGlobal(String),
    I32Add,
    F64Add,
    F32Add,
    Loop(String, Vec<Instruction>),
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

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_with_indent(
            statements: &[Instruction],
            f: &mut std::fmt::Formatter<'_>,
            indent_level: usize,
        ) -> std::fmt::Result {
            let indent = " ".repeat(indent_level);
            for statement in statements {
                writeln!(f, "{}{:#?}", indent, statement)?;
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

#[derive(Debug)]
pub struct WasmGenerator {
    pub instructions: Vec<Instruction>,
    data: Vec<String>,
    label_index: usize,
    block_index: usize,
    current_offset: usize,
    globals: HashMap<String, JsValue>,
}

impl Default for WasmGenerator {
    fn default() -> Self {
        Self {
            instructions: vec![],
            data: vec![],
            label_index: 0,
            block_index: 0,
            current_offset: 1,
            globals: HashMap::new(),
        }
    }
}

impl WasmGenerator {
    fn emit_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn handle_string_concat(&mut self, left: JsValue, right: JsValue) -> JsValue {
        match (left, right) {
            (JsValue::StringLiteral(left_str), JsValue::StringLiteral(right_str)) => {
                let left_offset = self.current_offset as i32;
                self.emit_data(left_str.clone());
                let right_offset = self.current_offset as i32;
                self.emit_data(right_str.clone());
                self.emit_instruction(Instruction::IntConst(left_offset));
                self.emit_instruction(Instruction::IntConst(right_offset));
                self.emit_instruction(Instruction::Call("concatenate_strings".into()));
                JsValue::StringOffset(self.current_offset as i32)
            }
            (JsValue::StringOffset(offset), JsValue::StringLiteral(str)) => {
                self.emit_instruction(Instruction::IntConst(offset));
                self.emit_data(str);
                self.emit_instruction(Instruction::Call("concatenate_strings".into()));
                JsValue::StringOffset(self.current_offset as i32)
            }
            (JsValue::StringOffset(left), JsValue::StringOffset(right)) => {
                self.emit_instruction(Instruction::IntConst(left));
                self.emit_instruction(Instruction::IntConst(right));
                self.emit_instruction(Instruction::Call("concatenate_strings".into()));
                JsValue::StringOffset(self.current_offset as i32)
            }
            something_else => panic!("Invalid string concatenation operands {something_else:#?}",),
        }
    }

    fn emit_data(&mut self, str_value: String) {
        let length_prefix = str_value.len() as u8;
        let prefixed_str_value = format!("\\{:02x}{}", length_prefix, str_value);

        self.data.push(format!(
            "(data (i32.const {}) \"{}\")",
            self.current_offset, prefixed_str_value
        ));

        self.current_offset += prefixed_str_value.len();
    }

    fn format_globals(&self) -> String {
        let mut globals = String::new();
        globals.push_str(&format!(
            "(global $alloc_offset (mut i32) (i32.const 1024))"
        ));
        for (name, value) in &self.globals {
            match value {
                JsValue::Float(fvalue) => globals.push_str(&format!(
                    "(global ${} (mut f64) (f64.const {})\n",
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

    pub fn get_wat(&self) -> String {
        let data_segments = self.data.join("\n");

        let default_functions = r#"
            (func $len (param $addr i32) (result i32)
                local.get $addr
                call $nullthrow
                i32.load8_u  ;; Changed to load8_u since we're storing single byte lengths
            )


            (func $nullthrow (param $addr i32) (result i32)
                local.get $addr
                i32.eqz
                (if
                    (then
                        unreachable
                    )
                )
                local.get $addr
            )
            (func $mem_cpy (param $src i32) (param $dest i32) (param $size i32)
                (local $idx i32)
                (local $temp i32)
                i32.const 0
                local.set $idx
                (block $block
                    (loop $loop
                        local.get $idx
                        local.get $size
                        i32.lt_s
                        i32.eqz
                        br_if $block
                        local.get $idx
                        local.get $src
                        i32.add
                        i32.load8_u
                        local.set $temp
                        local.get $idx
                        local.get $dest
                        i32.add
                        local.get $temp
                        i32.store8
                        local.get $idx
                        i32.const 1
                        i32.add
                        local.set $idx
                        br $loop
                    )
                )
            )

            (func $alloc (param $size i32) (result i32)
                (local $curr_alloc_addr i32)
                (local $next_alloc_addr i32)

                ;; Get current allocation offset and store it in $curr_alloc_addr
                global.get $alloc_offset
                local.set $curr_alloc_addr

                ;; Calculate next allocation address and store it in $next_alloc_addr
                local.get $curr_alloc_addr
                local.get $size
                i32.add
                local.set $next_alloc_addr

                ;; Update global allocation offset
                local.get $next_alloc_addr
                global.set $alloc_offset

                ;; Return the current allocation address
                local.get $curr_alloc_addr
            )
            ;; concatenate two strings, returning the address of the new string
            (func $concatenate_strings (param $s1 i32) (param $s2 i32) (result i32)
                (local $len1 i32)
                (local $len2 i32)
                (local $addr i32)
                (local $total_len i32)

                ;; Get lengths from the length prefixes
                local.get $s1
                call $len
                local.set $len1

                local.get $s2
                call $len
                local.set $len2

                ;; Calculate total length needed (lengths + combined content + new length byte)
                local.get $len1
                local.get $len2
                i32.add
                local.tee $total_len
                i32.const 1    ;; Add 1 for the length prefix
                i32.add
                call $alloc
                local.set $addr

                ;; Store new combined length
                local.get $addr
                local.get $total_len
                i32.store8

                ;; Copy first string content (skipping its length prefix)
                local.get $s1
                i32.const 1
                i32.add        ;; Skip length byte
                local.get $addr
                i32.const 1
                i32.add        ;; Skip length byte
                local.get $len1
                call $mem_cpy

                ;; Copy second string content (skipping its length prefix)
                local.get $s2
                i32.const 1
                i32.add        ;; Skip length byte
                local.get $addr
                i32.const 1
                i32.add
                local.get $len1
                i32.add        ;; Position after first string
                local.get $len2
                call $mem_cpy

                local.get $addr
            )
        "#;

        let default_exports = r#"
            (export "allocate" (func $alloc))
            (export "concatenate_strings" (func $concatenate_strings))
        "#;
        let header = format!(
            "(module\n(import \"\" \"\" (func $print_str(param i32)))\n(memory (export \"memory\") 1) \n {} {} \n {} \n (func (export \"main\") ",
            self.format_globals(),
            data_segments,
            default_functions
        );

        let instructions: Vec<String> = self
            .instructions
            .iter()
            .map(|instruction| format!(r#"    {:#?}"#, instruction))
            .collect();

        let instructions = instructions.join("\n");

        format!("{}\n{}\nreturn ){})", header, instructions, default_exports)
    }

    pub fn print_with_line(&mut self) {
        println!(
            "{}",
            self.get_wat()
                .lines()
                .enumerate()
                .map(|(i, line)| format!("{:>3} {}", i + 1, line))
                .collect::<Vec<String>>()
                .join("\n")
        );
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

    fn evaluate_expression(&mut self, expr: &Expression) -> JsValue {
        match expr {
            Expression::NumericLiteral(literal) => {
                self.visit_numeric_literal(literal);
                JsValue::Float(literal.raw.parse::<f64>().unwrap_or(0.))
            }
            Expression::StringLiteral(literal) => JsValue::StringLiteral(literal.value.to_string()),
            Expression::ParenthesizedExpression(expr) => self.evaluate_expression(&expr.expression),
            Expression::UnaryExpression(expr) => {
                if let Expression::StringLiteral(_) = &expr.argument {
                    self.emit_data("NaN".into());
                    JsValue::StringLiteral("NaN".into())
                } else if let Expression::NumericLiteral(literal) = &expr.argument {
                    self.visit_numeric_literal(literal);
                    JsValue::Float(literal.value)
                } else {
                    todo!()
                }
            }
            Expression::BinaryExpression(binary) => {
                let left = self.evaluate_expression(&binary.left);
                let right = self.evaluate_expression(&binary.right);
                match binary.operator {
                    BinaryOperator::Addition => {
                        if left.is_string() || right.is_string() {
                            self.handle_string_concat(left, right)
                        } else {
                            self.emit_instruction(Instruction::F64Add);
                            left + right
                        }
                    }
                    BinaryOperator::Subtraction => {
                        self.emit_instruction(Instruction::Sub);
                        left - right
                    }
                    BinaryOperator::Multiplication => {
                        self.emit_instruction(Instruction::Mul);
                        left * right
                    }
                    BinaryOperator::Division => {
                        self.emit_instruction(Instruction::Div);
                        left / right
                    }
                    var => todo!("{:#?}", var),
                }
            }
            var => unimplemented!("{:#?}", var),
        }
    }
}

impl Display for WasmGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in &self.instructions {
            writeln!(f, "{:?}", instruction)?;
        }
        Ok(())
    }
}

impl<'a> Visit<'a> for WasmGenerator {
    fn visit_break_statement(&mut self, _: &oxc_ast::ast::BreakStatement<'a>) {
        let new_block = self.new_block();
        self.emit_instruction(Instruction::Br(new_block));
    }

    fn visit_while_statement(&mut self, it: &oxc_ast::ast::WhileStatement<'a>) {
        let label = self.new_label();
        let body = &it.body;
        let instructions_len = &self.instructions.len();

        if let Statement::BlockStatement(block) = body {
            for statement in &block.body {
                self.visit_statement(statement);
            }
        }

        if let Expression::BinaryExpression(test) = &it.test {
            self.visit_binary_expression(test);
            self.emit_instruction(Instruction::BrIf(label.clone()));
        }

        if self.block_index > 0 {
            let instructions = self.instructions.split_off(*instructions_len);
            self.emit_instruction(Instruction::Loop(label, instructions));
            let instructions = self.instructions.split_off(*instructions_len);
            self.emit_instruction(Instruction::Block(self.current_block(), instructions));
        } else {
            let instructions = self.instructions.split_off(*instructions_len);
            self.emit_instruction(Instruction::Loop(label, instructions));
        }
    }
    fn visit_string_literal(&mut self, it: &oxc_ast::ast::StringLiteral<'a>) {
        self.emit_instruction(Instruction::IntConst(self.current_offset as i32));
        self.emit_data(it.value.to_string());
    }

    fn visit_for_statement(&mut self, it: &oxc_ast::ast::ForStatement<'a>) {
        let label = self.new_label();
        let body = &it.body;
        if let Some(ForStatementInit::VariableDeclaration(init)) = &it.init {
            for declarator in &init.declarations {
                self.visit_variable_declarator(declarator);
            }
        }
        let instructions_len = &self.instructions.len();
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
            self.emit_instruction(Instruction::BrIf(label.clone()));
        }
        if self.block_index > 0 {
            let instructions = self.instructions.split_off(*instructions_len);
            self.emit_instruction(Instruction::Loop(label, instructions));
            let instructions = self.instructions.split_off(*instructions_len);
            self.emit_instruction(Instruction::Block(self.current_block(), instructions));
        } else {
            let instructions = self.instructions.split_off(*instructions_len);
            self.emit_instruction(Instruction::Loop(label, instructions));
        }
    }
    fn visit_numeric_literal(&mut self, it: &NumericLiteral<'a>) {
        let value = it.raw.parse::<f64>().unwrap();
        self.emit_instruction(Instruction::Const(value));
    }

    fn visit_if_statement(&mut self, it: &oxc_ast::ast::IfStatement<'a>) {
        self.visit_expression(&it.test);

        let instructions_len = self.instructions.len();
        self.visit_statement(&it.consequent);
        let true_condition = self.instructions.split_off(instructions_len);
        let mut else_condition = vec![];
        if let Some(alternate) = &it.alternate {
            let instructions_len = self.instructions.len();
            self.visit_statement(alternate);
            else_condition = self.instructions.split_off(instructions_len);
        }
        self.emit_instruction(Instruction::If(true_condition, else_condition));
    }

    fn visit_identifier_reference(&mut self, it: &oxc_ast::ast::IdentifierReference<'a>) {
        let identifier = it.name.to_string();
        if let Some(ident) = self.globals.get(&identifier) {
            if ident.is_string() {
                let offset = ident.get_offset();
                self.emit_instruction(Instruction::IntConst(*offset.unwrap()));
                self.emit_instruction(Instruction::GetGlobal(format!(
                    "_______{}_____len",
                    identifier
                )));
            } else {
                self.emit_instruction(Instruction::GetGlobal(identifier));
            }
        }
    }

    fn visit_call_expression(&mut self, it: &oxc_ast::ast::CallExpression<'a>) {
        if let Expression::StaticMemberExpression(expr) = &it.callee {
            it.arguments.iter().for_each(|arg| {
                self.visit_expression(arg.to_expression());
            });
            if let Some(ident) = expr.object.get_identifier_reference() {
                let object = ident.name.to_string();
                let property = expr.property.name.to_string();
                if object == "console" && property == "log" {
                    self.emit_instruction(Instruction::Call("print_str".into()));
                }
            }
            if let Expression::StringLiteral(literal) = &expr.object {
                self.visit_string_literal(literal);
                let property = expr.property.name.to_string();
                if property == "indexOf" {}
            }
        }
    }

    fn visit_expression_statement(&mut self, it: &oxc_ast::ast::ExpressionStatement<'a>) {
        match &it.expression {
            Expression::Identifier(identifier) => {
                self.visit_identifier_reference(identifier);
            }
            Expression::BinaryExpression(binary) => {
                if let Expression::Identifier(ident) = &binary.left {
                    // TODO: This may be hacky,idk, but it works for now.
                    // It is used when the variable is unused, and only used as expression
                    // ```
                    // 1. let a = 5;
                    // a - 5;
                    // 2 + 5;
                    // ```
                    // Here a is unused, so we can drop it.

                    self.visit_binary_expression(binary);
                    self.emit_instruction(Instruction::Drop);
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
                    self.globals.insert(
                        identifier.clone(),
                        JsValue::StringOffset(self.current_offset as i32),
                    );
                    self.visit_string_literal(string_literal);
                } else {
                    let value = self.evaluate_expression(init);
                    self.globals.insert(identifier.clone(), value);
                }
            }
            None => {
                self.globals.insert(identifier.clone(), JsValue::Float(0.));
            }
        }
    }

    fn visit_binary_expression(&mut self, it: &oxc_ast::ast::BinaryExpression<'a>) {
        if let Expression::Identifier(ident) = &it.left {
            self.visit_identifier_reference(ident);
        }

        let left = self.evaluate_expression(&it.left);
        let right = self.evaluate_expression(&it.right);
        match it.operator {
            BinaryOperator::Addition => {
                if left.is_string() && right.is_string() {
                    self.handle_string_concat(left, right);
                } else {
                    self.emit_instruction(Instruction::F64Add);
                }
            }
            BinaryOperator::Subtraction => {
                self.emit_instruction(Instruction::Sub);
            }
            BinaryOperator::Multiplication => {
                self.emit_instruction(Instruction::Mul);
            }
            BinaryOperator::Division => {
                self.emit_instruction(Instruction::Div);
            }
            BinaryOperator::LessThan => {
                self.emit_instruction(Instruction::Lt);
            }
            BinaryOperator::GreaterThan => {
                self.emit_instruction(Instruction::Gt);
            }
            BinaryOperator::StrictEquality | BinaryOperator::Equality => {
                self.emit_instruction(Instruction::Eq);
            }

            _ => {}
        }
    }

    fn visit_assignment_expression(&mut self, it: &oxc_ast::ast::AssignmentExpression<'a>) {
        let identifier = it.left.get_identifier().unwrap();
        self.visit_expression(&it.right);
        self.emit_instruction(Instruction::SetGlobal(identifier.into()));
    }

    fn visit_update_expression(&mut self, it: &oxc_ast::ast::UpdateExpression<'a>) {
        let identifier = it.argument.get_identifier().unwrap();
        self.emit_instruction(Instruction::GetGlobal(identifier.into()));
        self.emit_instruction(Instruction::Const(1.));
        match it.operator {
            oxc_syntax::operator::UpdateOperator::Increment => {
                self.emit_instruction(Instruction::F64Add);
            }
            oxc_syntax::operator::UpdateOperator::Decrement => {
                self.emit_instruction(Instruction::Sub);
            }
        }
        self.emit_instruction(Instruction::SetGlobal(identifier.into()));
    }
}
