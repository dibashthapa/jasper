pub enum ByteCode {
    Const(f64),
    IntConst(i32),
    Call(String),
    GetGlobal(String),
    SetGlobal(String),
    Add,
    Loop(String, Vec<ByteCode>),
    Block(String, Vec<ByteCode>),
    Mul,
    Sub,
    If(Vec<ByteCode>, Vec<ByteCode>),
    Drop,
    Br(String),
    Div,
    Lt,
    Eq,
    Gt,
    BrIf(String),
}

impl std::fmt::Debug for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_with_indent(
            statements: &[ByteCode],
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
            Self::Add => write!(f, "f64.add"),
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
