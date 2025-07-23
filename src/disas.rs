use m68kdecode::{
    AddressRegister, DataRegister, DecodedInstruction, Displacement, Indexer, Instruction,
    InstructionExtra, MemoryIndirection, Operand, Operation,
};

/// Printable instruction.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PrintableInstruction {
    /// Instruction as a string.
    pub instruction: String,
    /// Operands as a string.
    pub operands: String,
    /// Data words making the instruction.
    pub words: Vec<u16>,
    /// Additional data added to the line as a comment.
    pub comment: String,
    /// Address of the next instruction after this one.
    pub next_address: u32,
}

pub fn disas(code: &[u8], address: u32) -> std::io::Result<PrintableInstruction> {
    let mut instr = match m68kdecode::decode_instruction(&code[address as usize..]) {
        Ok(i) => i,
        Err(m68kdecode::DecodingError::NotImplemented) => error_instr(-1),
        Err(m68kdecode::DecodingError::Reserved) => error_instr(-2),
        Err(m68kdecode::DecodingError::BadSize) => error_instr(-3),
        Err(m68kdecode::DecodingError::BadRegister) => error_instr(-4),
        Err(m68kdecode::DecodingError::OutOfSpace) => {
            return Err(std::io::Error::from(std::io::ErrorKind::UnexpectedEof));
        }
    };

    let mut words = vec![];
    for addr in (address as usize..(address as usize + instr.bytes_used as usize)).step_by(2) {
        let word = ((code[addr] as u16) << 8) | code[addr + 1] as u16;
        words.push(word);
    }
    let inst_op;
    let mut inst_opers;
    let mut comment = String::new();
    if instr.instruction.size >= 0 {
        let op = format_op(&instr.instruction.operation, &instr.instruction.extra);
        let mut opers = vec![];
        if is_shift_instruction(&instr.instruction.operation) {
            // in shift instructions: #0 means #8
            if let m68kdecode::Operand::IMM8(imm8) = &instr.instruction.operands[0] {
                if *imm8 == 0 {
                    instr.instruction.operands[0] = m68kdecode::Operand::IMM8(8);
                }
            }
        }
        match &instr.instruction.operands[0] {
            Operand::NoOperand => {}
            Operand::Implied => {
                // use the implied value returned from format_op()
                if !op.1.is_empty() {
                    opers.push(op.1.to_string());
                }
            }
            _ => opers.push(format_operand(
                &instr.instruction.operands[0],
                address,
                true,
                &mut comment,
            )),
        };
        match &instr.instruction.operands[1] {
            Operand::NoOperand => {}
            Operand::Implied => {
                // use the implied value returned from format_op()
                if !op.1.is_empty() {
                    opers.push(op.1.to_string());
                }
            }
            _ => opers.push(format_operand(
                &instr.instruction.operands[1],
                address,
                false,
                &mut comment,
            )),
        };
        let size_letter = format_size(instr.instruction.size);
        inst_opers = opers.join(", ");
        if let InstructionExtra::PackAdjustment(pa) = instr.instruction.extra {
            inst_opers += &format!(", #{}", pa);
        }
        inst_op = format!("{}{}", op.0, size_letter);
    } else {
        inst_op = format!(".short 0x{:04x}", words[0]);
        inst_opers = String::new();
    }
    Ok(PrintableInstruction {
        instruction: inst_op,
        operands: inst_opers,
        comment,
        words,
        next_address: address + instr.bytes_used,
    })
}

fn error_instr(code: i32) -> DecodedInstruction {
    DecodedInstruction {
        bytes_used: 2,
        instruction: Instruction {
            size: code,
            operation: Operation::NOP,
            operands: [Operand::NoOperand, Operand::NoOperand],
            extra: InstructionExtra::NoExtra,
        },
    }
}

fn format_op(op: &Operation, extra: &InstructionExtra) -> (String, &'static str) {
    match op {
        Operation::ANDITOCCR => ("ANDI".to_string(), "CCR"),
        Operation::ANDITOSR => ("ANDI".to_string(), "SR"),
        Operation::EORITOCCR => ("EORI".to_string(), "CCR"),
        Operation::EORITOSR => ("EORI".to_string(), "SR"),
        Operation::ORITOCCR => ("ORI".to_string(), "CCR"),
        Operation::ORITOSR => ("ORI".to_string(), "SR"),
        Operation::MOVEP => ("MOVEP".to_string(), ""),
        Operation::BTST => ("BTST".to_string(), ""),
        Operation::BCHG => ("BCHG".to_string(), ""),
        Operation::BCLR => ("BCLR".to_string(), ""),
        Operation::BSET => ("BSET".to_string(), ""),
        Operation::RTM => ("RTM".to_string(), ""),
        Operation::CALLM => ("CALLM".to_string(), ""),
        Operation::ADDI => ("ADDI".to_string(), ""),
        Operation::SUBI => ("SUBI".to_string(), ""),
        Operation::ANDI => ("ANDI".to_string(), ""),
        Operation::ORI => ("ORI".to_string(), ""),
        Operation::CMP2 => ("CMP2".to_string(), ""),
        Operation::CHK2 => ("CHK2".to_string(), ""),
        Operation::EORI => ("EORI".to_string(), ""),
        Operation::CMPI => ("CMPI".to_string(), ""),
        Operation::MOVES => ("MOVES".to_string(), ""),
        Operation::MOVE => ("MOVE".to_string(), ""),
        Operation::MOVEA => ("MOVEA".to_string(), ""),
        Operation::BGND => ("BGND".to_string(), ""),
        Operation::ILLEGAL => ("ILLEGAL".to_string(), ""),
        Operation::NOP => ("NOP".to_string(), ""),
        Operation::RESET => ("RESET".to_string(), ""),
        Operation::RTD => ("RTD".to_string(), ""),
        Operation::RTE => ("RTE".to_string(), ""),
        Operation::RTR => ("RTR".to_string(), ""),
        Operation::RTS => ("RTS".to_string(), ""),
        Operation::STOP => ("STOP".to_string(), ""),
        Operation::TRAPV => ("TRAPV".to_string(), ""),
        Operation::MOVEC => ("MOVEC".to_string(), ""),
        Operation::SWAP => ("SWAP".to_string(), ""),
        Operation::BKPT => ("BKPT".to_string(), ""),
        Operation::EXTW => ("EXTW".to_string(), ""),
        Operation::EXTL => ("EXTL".to_string(), ""),
        Operation::EXTBL => ("EXTBL".to_string(), ""),
        Operation::LEA => ("LEA".to_string(), ""),
        Operation::LINK => ("LINK".to_string(), ""),
        Operation::UNLK => ("UNLK".to_string(), ""),
        Operation::TRAP => ("TRAP".to_string(), ""),
        Operation::DIVSL => ("DIVSL".to_string(), ""),
        Operation::DIVSLL => ("DIVSLL".to_string(), ""),
        Operation::DIVUL => ("DIVUL".to_string(), ""),
        Operation::DIVULL => ("DIVULL".to_string(), ""),
        Operation::JMP => ("JMP".to_string(), ""),
        Operation::JSR => ("JSR".to_string(), ""),
        Operation::MULS => ("MULS".to_string(), ""),
        Operation::MULU => ("MULU".to_string(), ""),
        Operation::NBCD => ("NBCD".to_string(), ""),
        Operation::MOVEFROMSR => ("MOVE".to_string(), "SR"),
        Operation::MOVETOSR => ("MOVE".to_string(), "SR"),
        Operation::MOVETOUSP => ("MOVE".to_string(), "USP"),
        Operation::MOVEFROMUSP => ("MOVE".to_string(), "USP"),
        Operation::MOVEFROMCCR => ("MOVE".to_string(), "CCR"),
        Operation::MOVETOCCR => ("MOVE".to_string(), "CCR"),
        Operation::PEA => ("PEA".to_string(), ""),
        Operation::TAS => ("TAS".to_string(), ""),
        Operation::MOVEM => ("MOVEM".to_string(), ""),
        Operation::CLR => ("CLR".to_string(), ""),
        Operation::NEG => ("NEG".to_string(), ""),
        Operation::NEGX => ("NEGX".to_string(), ""),
        Operation::NOT => ("NOT".to_string(), ""),
        Operation::TST => ("TST".to_string(), ""),
        Operation::CHK => ("CHK".to_string(), ""),
        Operation::DBCC => (format!("DB{}", format_condition(extra)), ""),
        Operation::ADDQ => ("ADDQ".to_string(), ""),
        Operation::SUBQ => ("SUBQ".to_string(), ""),
        Operation::TRAPCC => (format!("TRAP{}", format_condition(extra)), ""),
        Operation::SCC => (format!("S{}", format_condition(extra)), ""),
        Operation::BRA => ("BRA".to_string(), ""),
        Operation::BSR => ("BSR".to_string(), ""),
        Operation::BCC => (format!("B{}", format_condition(extra)), ""),
        Operation::MOVEQ => ("MOVEQ".to_string(), ""),
        Operation::PACK => ("PACK".to_string(), ""),
        Operation::UNPK => ("UNPK".to_string(), ""),
        Operation::SBCD => ("SBCD".to_string(), ""),
        Operation::DIVS => ("DIVS".to_string(), ""),
        Operation::DIVU => ("DIVU".to_string(), ""),
        Operation::OR => ("OR".to_string(), ""),
        Operation::SUBX => ("SUBX".to_string(), ""),
        Operation::SUB => ("SUB".to_string(), ""),
        Operation::SUBA => ("SUBA".to_string(), ""),
        Operation::CMPA => ("CMPA".to_string(), ""),
        Operation::CMPM => ("CMPM".to_string(), ""),
        Operation::CMP => ("CMP".to_string(), ""),
        Operation::EOR => ("EOR".to_string(), ""),
        Operation::ABCD => ("ABCD".to_string(), ""),
        Operation::EXG => ("EXG".to_string(), ""),
        Operation::AND => ("AND".to_string(), ""),
        Operation::ADDX => ("ADDX".to_string(), ""),
        Operation::ADD => ("ADD".to_string(), ""),
        Operation::ADDA => ("ADDA".to_string(), ""),
        Operation::BFCHG => ("BFCHG".to_string(), ""),
        Operation::BFCLR => ("BFCLR".to_string(), ""),
        Operation::BFEXTS => ("BFEXTS".to_string(), ""),
        Operation::BFEXTU => ("BFEXTU".to_string(), ""),
        Operation::BFFFO => ("BFFFO".to_string(), ""),
        Operation::BFINS => ("BFINS".to_string(), ""),
        Operation::BFSET => ("BFSET".to_string(), ""),
        Operation::BFTST => ("BFTST".to_string(), ""),
        Operation::ASL => ("ASL".to_string(), ""),
        Operation::ASR => ("ASR".to_string(), ""),
        Operation::LSL => ("LSL".to_string(), ""),
        Operation::LSR => ("LSR".to_string(), ""),
        Operation::ROXL => ("ROXL".to_string(), ""),
        Operation::ROXR => ("ROXR".to_string(), ""),
        Operation::ROL => ("ROL".to_string(), ""),
        Operation::ROR => ("ROR".to_string(), ""),
        Operation::FMOVECR => ("FMOVECR".to_string(), "CR"),
        Operation::FABS => ("FABS".to_string(), ""),
        Operation::FSABS => ("FSABS".to_string(), ""),
        Operation::FDABS => ("FDABS".to_string(), ""),
        Operation::FACOS => ("FACOS".to_string(), ""),
        Operation::FADD => ("FADD".to_string(), ""),
        Operation::FSADD => ("FSADD".to_string(), ""),
        Operation::FDADD => ("FDADD".to_string(), ""),
        Operation::FASIN => ("FASIN".to_string(), ""),
        Operation::FATAN => ("FATAN".to_string(), ""),
        Operation::FATANH => ("FATANH".to_string(), ""),
        Operation::FNOP => ("FNOP".to_string(), ""),
        Operation::FBCC => ("FBCC".to_string(), ""),
        Operation::FCMP => ("FCMP".to_string(), ""),
        Operation::FCOS => ("FCOS".to_string(), ""),
        Operation::FCOSH => ("FCOSH".to_string(), ""),
        Operation::FDBCC => ("FDBCC".to_string(), ""),
        Operation::FDIV => ("FDIV".to_string(), ""),
        Operation::FSDIV => ("FSDIV".to_string(), ""),
        Operation::FDDIV => ("FDDIV".to_string(), ""),
        Operation::FETOX => ("FETOX".to_string(), ""),
        Operation::FETOXM1 => ("FETOXM1".to_string(), ""),
        Operation::FGETEXP => ("FGETEXP".to_string(), ""),
        Operation::FGETMAN => ("FGETMAN".to_string(), ""),
        Operation::FINT => ("FINT".to_string(), ""),
        Operation::FINTRZ => ("FINTRZ".to_string(), ""),
        Operation::FLOG10 => ("FLOG10".to_string(), ""),
        Operation::FLOG2 => ("FLOG2".to_string(), ""),
        Operation::FLOGN => ("FLOGN".to_string(), ""),
        Operation::FLOGNP1 => ("FLOGNP1".to_string(), ""),
        Operation::FMOD => ("FMOD".to_string(), ""),
        Operation::FMOVE => ("FMOVE".to_string(), ""),
        Operation::FSMOVE => ("FSMOVE".to_string(), ""),
        Operation::FDMOVE => ("FDMOVE".to_string(), ""),
        Operation::FMOVEM => ("FMOVEM".to_string(), ""),
        Operation::FMUL => ("FMUL".to_string(), ""),
        Operation::FSMUL => ("FSMUL".to_string(), ""),
        Operation::FDMUL => ("FDMUL".to_string(), ""),
        Operation::FNEG => ("FNEG".to_string(), ""),
        Operation::FSNEG => ("FSNEG".to_string(), ""),
        Operation::FDNEG => ("FDNEG".to_string(), ""),
        Operation::FREM => ("FREM".to_string(), ""),
        Operation::FSCALE => ("FSCALE".to_string(), ""),
        Operation::FTRAPCC => ("FTRAPCC".to_string(), ""),
        Operation::FSCC => ("FSCC".to_string(), ""),
        Operation::FSGLDIV => ("FSGLDIV".to_string(), ""),
        Operation::FSGLMUL => ("FSGLMUL".to_string(), ""),
        Operation::FSIN => ("FSIN".to_string(), ""),
        Operation::FSINCOS => ("FSINCOS".to_string(), ""),
        Operation::FSINH => ("FSINH".to_string(), ""),
        Operation::FSQRT => ("FSQRT".to_string(), ""),
        Operation::FSSQRT => ("FSSQRT".to_string(), ""),
        Operation::FDSQRT => ("FDSQRT".to_string(), ""),
        Operation::FSUB => ("FSUB".to_string(), ""),
        Operation::FSSUB => ("FSSUB".to_string(), ""),
        Operation::FDSUB => ("FDSUB".to_string(), ""),
        Operation::FTAN => ("FTAN".to_string(), ""),
        Operation::FTANH => ("FTANH".to_string(), ""),
        Operation::FTENTOX => ("FTENTOX".to_string(), ""),
        Operation::FTST => ("FTST".to_string(), ""),
        Operation::FTWOTOX => ("FTWOTOX".to_string(), ""),
    }
}

fn is_shift_instruction(op: &Operation) -> bool {
    match op {
        Operation::ASL => true,
        Operation::ASR => true,
        Operation::LSL => true,
        Operation::LSR => true,
        Operation::ROXL => true,
        Operation::ROXR => true,
        Operation::ROL => true,
        Operation::ROR => true,
        _ => false,
    }
}

fn format_condition(extra: &InstructionExtra) -> &'static str {
    match extra {
        InstructionExtra::Condition(cond) => match cond {
            m68kdecode::ConditionCode::CC_T => "T",
            m68kdecode::ConditionCode::CC_F => "F",
            m68kdecode::ConditionCode::CC_HI => "HI",
            m68kdecode::ConditionCode::CC_LS => "LS",
            m68kdecode::ConditionCode::CC_CC => "CC",
            m68kdecode::ConditionCode::CC_CS => "CS",
            m68kdecode::ConditionCode::CC_NE => "NE",
            m68kdecode::ConditionCode::CC_EQ => "EQ",
            m68kdecode::ConditionCode::CC_VC => "VC",
            m68kdecode::ConditionCode::CC_VS => "VS",
            m68kdecode::ConditionCode::CC_PL => "PL",
            m68kdecode::ConditionCode::CC_MI => "MI",
            m68kdecode::ConditionCode::CC_GE => "GE",
            m68kdecode::ConditionCode::CC_LT => "LT",
            m68kdecode::ConditionCode::CC_GT => "GT",
            m68kdecode::ConditionCode::CC_LE => "LE",
        },
        _ => "??",
    }
}

fn format_operand(operand: &Operand, address: u32, src: bool, comment: &mut String) -> String {
    match operand {
        Operand::NoOperand => "".to_string(),
        Operand::Implied => "".to_string(),
        Operand::IMM8(val) => {
            *comment += &format!(" #{}", val);
            format!("#${:02x}", val)
        }
        Operand::IMM16(val) => {
            *comment += &format!(" #{}", val);
            format!("#${:04x}", val)
        }
        Operand::IMM32(val) => {
            *comment += &format!(" #{}", val);
            format!("#${:08x}", val)
        }
        Operand::ABS16(val) => {
            *comment += &format!(" ${:08x}", (*val as i32) as u32);
            format!("(${:04x}).W", val)
        }
        Operand::ABS32(val) => {
            *comment += &format!(" ${:08x}", val);
            format!("(${:08x}).L", val)
        }
        Operand::DR(dreg) => format!("{}", format_data_register(dreg)),
        Operand::AR(areg) => format!("{}", format_addr_register(areg)),
        Operand::FR(freg) => format!("{:?}", freg),
        Operand::ARIND(areg) => format!("({})", format_addr_register(areg)),
        Operand::ARINC(areg) => format!("({})+", format_addr_register(areg)),
        Operand::ARDEC(areg) => format!("-({})", format_addr_register(areg)),
        Operand::ARDISP(areg, disp) => format!("{}", format_disp_addr(disp, areg)),
        Operand::PCDISP(offset, disp) => {
            format!("{}", format_disp_pc(address, *offset, disp, comment))
        }
        Operand::DISP(displacement) => format!("{}", format_disp(displacement)),
        Operand::DPAIR(dreg, dreg1) => format!(
            "{}-{}",
            format_data_register(dreg),
            format_data_register(dreg1)
        ),
        Operand::FPAIR(freg, freg1) => format!("{:?}{:?}???", freg, freg1),
        Operand::REGLIST(val) => format!("{}", format_rlist(*val, src)),
        Operand::CONTROLREG(val) => format!("${:08x}", val),
    }
}

fn format_rlist(mut val: u16, src: bool) -> String {
    if src {
        // reverse bits
        let mut orig = val;
        val = 0;
        for _ in 0..16 {
            val <<= 1;
            val |= orig & 1;
            orig >>= 1;
        }
    }

    let mut list_first = None;
    let mut result = vec![];

    // D0-D7
    for i in 0..8 {
        if val & 1 == 1 {
            if list_first.is_none() {
                list_first = Some(i);
            }
        } else {
            if let Some(lf) = list_first {
                if lf == i - 1 {
                    result.push(format!("D{}", lf));
                } else {
                    result.push(format!("D{}-D{}", lf, i - 1));
                }
            }
            list_first = None;
        }
        val >>= 1;
    }
    if let Some(lf) = list_first {
        if lf == 7 {
            result.push(format!("D{}", lf));
        } else {
            result.push(format!("D{}-D7", lf));
        }
    }

    // A0-A7
    list_first = None;
    for i in 0..8 {
        if val & 1 == 1 {
            if list_first.is_none() {
                list_first = Some(i);
            }
        } else {
            if let Some(lf) = list_first {
                if lf == i - 1 {
                    result.push(format!("A{}", lf));
                } else {
                    result.push(format!("A{}-A{}", lf, i - 1));
                }
            }
            list_first = None;
        }
        val >>= 1;
    }
    if let Some(lf) = list_first {
        if lf == 7 {
            result.push(format!("A{}", lf));
        } else {
            result.push(format!("A{}-A7", lf));
        }
    }

    result.join(",")
}

fn format_data_register(reg: &DataRegister) -> &'static str {
    match reg {
        DataRegister::D0 => "D0",
        DataRegister::D1 => "D1",
        DataRegister::D2 => "D2",
        DataRegister::D3 => "D3",
        DataRegister::D4 => "D4",
        DataRegister::D5 => "D5",
        DataRegister::D6 => "D6",
        DataRegister::D7 => "D7",
    }
}

fn format_addr_register(reg: &AddressRegister) -> &'static str {
    match reg {
        AddressRegister::A0 => "A0",
        AddressRegister::A1 => "A1",
        AddressRegister::A2 => "A2",
        AddressRegister::A3 => "A3",
        AddressRegister::A4 => "A4",
        AddressRegister::A5 => "A5",
        AddressRegister::A6 => "A6",
        AddressRegister::A7 => "A7",
    }
}

fn format_disp(disp: &Displacement) -> String {
    if disp.indirection != MemoryIndirection::NoIndirection || disp.outer_displacement != 0 {
        return "?".to_string();
    }
    if disp.indexer != Indexer::NoIndexer {
        return "?".to_string();
    }
    format!("({})", disp.base_displacement)
}

fn format_disp_addr(disp: &Displacement, reg: &AddressRegister) -> String {
    if disp.indirection != MemoryIndirection::NoIndirection || disp.outer_displacement != 0 {
        return "?".to_string();
    }
    match &disp.indexer {
        Indexer::NoIndexer => format!(
            "({:}, {})",
            disp.base_displacement,
            format_addr_register(reg)
        ),
        Indexer::DR(dreg, _) => format!(
            "({}, {}, {})",
            disp.base_displacement,
            format_addr_register(reg),
            format_data_register(dreg)
        ),
        Indexer::AR(areg, _) => format!(
            "({}, {}, {})",
            disp.base_displacement,
            format_addr_register(reg),
            format_addr_register(areg)
        ),
    }
}

fn format_disp_pc(address: u32, offset: u8, disp: &Displacement, comment: &mut String) -> String {
    if disp.indirection != MemoryIndirection::NoIndirection || disp.outer_displacement != 0 {
        return "?".to_string();
    }
    match &disp.indexer {
        Indexer::NoIndexer => {
            *comment += &format!(
                " ${:08x}",
                address
                    .wrapping_add_signed(disp.base_displacement)
                    .wrapping_add(offset as u32)
            );
            format!("({}, PC)", disp.base_displacement)
        }
        //Indexer::NoIndexer => format!("{}(PC)", disp.base_displacement),
        Indexer::DR(dreg, _) => {
            *comment += &format!(
                " {:08x}",
                address
                    .wrapping_add_signed(disp.base_displacement)
                    .wrapping_add(offset as u32)
            );
            format!(
                "({}, PC, {})",
                disp.base_displacement,
                format_data_register(dreg)
            )
        }
        Indexer::AR(areg, _) => {
            *comment += &format!(
                " {:08x}",
                address
                    .wrapping_add_signed(disp.base_displacement)
                    .wrapping_add(offset as u32)
            );
            format!(
                "({}, PC, {})",
                disp.base_displacement,
                format_addr_register(areg)
            )
        }
    }
}

fn format_size(sz: i32) -> &'static str {
    match sz {
        0 => "",
        1 => ".B",
        2 => ".W",
        4 => ".L",
        _ => ".?",
    }
}
