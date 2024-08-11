pub enum ByteCode {
    Const(f64),
    GetGlobal(String),
    SetGlobal(usize, String),
    Add,
    Loop(String, Vec<ByteCode>),
    Mul,
    Sub,
    If(Vec<ByteCode>, Vec<ByteCode>),
    Drop,
    Div,
    Lt,
    Eq,
    Gt,
    BrIf(String),
}

impl std::fmt::Debug for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(num) => write!(f, "f64.const {}", num),
            Self::Add => write!(f, "f64.add"),
            Self::Mul => write!(f, "f64.mul"),
            Self::Sub => write!(f, "f64.sub"),
            Self::Div => write!(f, "f64.div"),
            Self::Gt => write!(f, "f64.gt"),
            Self::SetGlobal(_, variable) => write!(f, "global.set ${}", variable),
            Self::GetGlobal(variable) => write!(f, "global.get ${}", variable),
            Self::Loop(label, statements) => write!(f, "loop ${} {:#?}", label, statements),
            Self::Lt => write!(f, "f64.lt"),
            Self::If(then, rest) => {
                // make space of then relative to if
                write!(f, "(if \n      (then \n")?;
                for statement in then {
                    write!(f, "         {:#?}\n", statement)?;
                }
                write!(f, "      )\n      (else \n")?;
                for statement in rest {
                    write!(f, "         {:#?}\n", statement)?;
                }
                write!(f, "      )\n    )")
            }
            Self::BrIf(label) => write!(f, "br_if ${}", label),
            Self::Drop => write!(f, "drop"),
            Self::Eq => write!(f, "f64.eq"),
        }
    }
}

struct Vm {
    registers: [f64; 8],
    pc: usize,
    stack: Vec<f64>,
    program: Vec<ByteCode>,
}
