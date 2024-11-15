use super::generator::Instruction;

enum ValType {
    I32,
    I64,
    F64,
    F32,
}

trait WatCode {
    fn locals<'a>(&self) -> Vec<(&'a str, ValType)>;
    fn wasm(&mut self) -> Vec<Instruction>;
    fn params(&mut self) -> Vec<ValType>;
}

struct StringToLowerCase;

impl WatCode for StringToLowerCase {
    fn locals<'a>(&self) -> Vec<(&'a str, ValType)> {
        vec![
            ("len", ValType::I32),
            ("idx", ValType::I32),
            ("char", ValType::I32),
            ("new_addr", ValType::I32),
        ]
    }
    fn wasm(&mut self) -> Vec<Instruction> {
        vec![
            Instruction::LocalGet("addr".to_string()),
            Instruction::Call("len".to_string()),
            Instruction::LocalSet("len".to_string()),
            Instruction::IntConst(1),
            Instruction::I32Add,
            Instruction::Call("alloc".to_string()),
            Instruction::LocalSet("new_addr".to_string()),
            Instruction::LocalGet("new_addr".to_string()),
            Instruction::LocalGet("len".to_string()),
            Instruction::I32Store16,
            Instruction::IntConst(0),
            Instruction::LocalSet("idx".to_string()),
            Instruction::Loop(
                "label_0".to_string(),
                vec![
                    Instruction::LocalGet("idx".to_string()),
                    Instruction::LocalGet("len".to_string()),
                    Instruction::I32LessThan,
                ],
            ),
        ]
    }

    fn params(&mut self) -> Vec<ValType> {
        vec![ValType::I32, ValType::I32, ValType::I32]
    }
}
