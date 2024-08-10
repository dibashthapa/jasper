pub enum ByteCode {
    Const(i32),
    GetGlobal(String),
    SetGlobal(usize, String),
    Add,
    Loop(String, Vec<ByteCode>),
    Mul,
    Sub,
    If,
    Drop,
    Div,
    Lt,
    BrIf(String),
}
impl ToString for ByteCode {
    fn to_string(&self) -> String {
        match self {
            Self::Const(num) => format!("f64.const {}", num),
            Self::Add => "f64.add".to_string(),
            Self::Mul => "f64.mul".to_string(),
            Self::Sub => "f64.sub".to_string(),
            Self::Div => "f64.div".to_string(),
            Self::SetGlobal(_to, variable) => format!("global.set ${} ", variable),
            Self::GetGlobal(variable) => format!("global.get ${}", variable),
            Self::Loop(label, statements) => {
                let statements: Vec<String> = statements.iter().map(|s| s.to_string()).collect();
                format!(
                    "(loop ${:}\n     {}    \n   )",
                    label,
                    &statements.join("\n     ")
                )
            }
            Self::Lt => "f64.lt".to_string(),
            Self::If => "if".to_string(),
            Self::BrIf(label) => format!("br_if ${}", label),
            Self::Drop => "drop".to_string(),
        }
    }
}

impl std::fmt::Debug for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(num) => write!(f, "i32.const {}", num),
            Self::Add => write!(f, "i32.add"),
            Self::Mul => write!(f, "i32.mul"),
            Self::Sub => write!(f, "i32.sub"),
            Self::Div => write!(f, "i32.div"),
            Self::SetGlobal(tos, variable) => write!(f, "global.set {} ;; ${}", tos, variable),
            Self::GetGlobal(variable) => write!(f, "global.get ${}", variable),
            Self::Loop(label, statements) => write!(f, "loop ${} {:#?}", label, statements),
            Self::Lt => write!(f, "i32.lt"),
            Self::If => write!(f, "if"),
            Self::BrIf(label) => write!(f, "br_if ${}", label),
            Self::Drop => write!(f, "drop"),
        }
    }
}

struct Vm {
    registers: [f64; 8],
    pc: usize,
    stack: Vec<f64>,
    program: Vec<ByteCode>,
}
