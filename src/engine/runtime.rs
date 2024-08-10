pub enum ByteCode {
    Add(i32, i32, i32),
    Mul(i32, i32, i32),
    Sub(i32, i32, i32),
    Div(i32, i32, i32),
    Load(i32, f64),
}

impl std::fmt::Debug for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Load(reg, value) => write!(f, "Load R{:?} {}", reg, value),
            Self::Add(dst, src1, src2) => write!(f, "Add  R{:?}, R{:?}, R{:?}", dst, src1, src2),
            Self::Mul(dst, src1, src2) => write!(f, "Mul  R{} R{:?}, R{:?}", dst, src1, src2),
            Self::Sub(dst, src1, src2) => write!(f, "Sub  R{} R{:?}, R{:?}", dst, src1, src2),
            Self::Div(dst, src1, src2) => write!(f, "Div  R{} R{:?}, R{:?}", dst, src1, src2),
        }
    }
}

#[derive(Debug)]
pub enum Registers {
    R1,
    R2,
    R3,
    R4,
    R5,
}

impl From<Registers> for usize {
    fn from(reg: Registers) -> usize {
        match reg {
            Registers::R1 => 1,
            Registers::R2 => 2,
            Registers::R3 => 3,
            Registers::R4 => 4,
            Registers::R5 => 5,
        }
    }
}

impl Into<Registers> for usize {
    fn into(self) -> Registers {
        match self {
            1 => Registers::R1,
            2 => Registers::R2,
            3 => Registers::R3,
            4 => Registers::R4,
            5 => Registers::R5,
            _ => Registers::R1,
        }
    }
}

// struct Vm {
//     registers: [f64; 8],
//     pc: usize,
//     stack: Vec<f64>,
//     program: Vec<ByteCode>,
// }
