use std::env;
use std::fs::File;
use std::io::{Read, Seek};

mod disas;

#[derive(Debug, Clone)]
struct HunkUnit {
    unit_name: String,
    name: String,
    code: Vec<u8>,
    data: Vec<u8>,
    symbols: Vec<(String, i32)>,
    ext_symbols: Vec<(String, i32)>,
}

impl HunkUnit {
    fn new() -> HunkUnit {
        HunkUnit {
            unit_name: String::new(),
            name: String::new(),
            code: vec![],
            data: vec![],
            symbols: vec![],
            ext_symbols: vec![],
        }
    }
}

const HUNK_UNIT: u32 = 999; // 0x03E7
const HUNK_NAME: u32 = 1000;
const HUNK_CODE: u32 = 1001;
const HUNK_DATA: u32 = 1002;
const HUNK_BSS: u32 = 1003;
const HUNK_RELOC32: u32 = 1004;
const _HUNK_ABSRELOC32: u32 = HUNK_RELOC32;
const _HUNK_RELOC16: u32 = 1005;
const _HUNK_RELRELOC16: u32 = _HUNK_RELOC16;
const _HUNK_RELOC8: u32 = 1006;
const _HUNK_RELRELOC8: u32 = _HUNK_RELOC8;
const HUNK_EXT: u32 = 1007;
const HUNK_SYMBOL: u32 = 1008;
const HUNK_DEBUG: u32 = 1009;
const HUNK_END: u32 = 1010;
const _HUNK_HEADER: u32 = 1011;

const _HUNK_OVERLAY: u32 = 1013;
const _HUNK_BREAK: u32 = 1014;
const HUNK_DREL32: u32 = 1015;
const _HUNK_DREL16: u32 = 1016;
const _HUNK_DREL8: u32 = 1017;
const _HUNK_LIB: u32 = 1018;
const _HUNK_INDEX: u32 = 1019;
const _HUNK_RELOC32SHORT: u32 = 1020;
const _HUNK_RELRELOC32: u32 = 1021;
const _HUNK_ABSRELOC16: u32 = 1022;

fn main() -> std::io::Result<()> {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);
    let sort = if args.len() > 1 && args[0] == "--sort" {
        args.remove(0);
        true
    } else {
        false
    };
    if args.len() == 0 {
        println!("Usage: amiga-lib-dumper [--sort] amiga.lib");
        return Ok(());
    }
    let filename = &args[0];
    let mut file = File::open(filename)?;
    let units = parse_file(&mut file)?;
    print_units(&units, sort);
    Ok(())
}

struct StringGroup {
    key: String,
    lines: Vec<String>,
}

fn print_units(units: &Vec<HunkUnit>, sort: bool) {
    let mut groups = vec![];
    for unit in units {
        let mut lines = vec![];
        if !unit.unit_name.is_empty() {
            lines.push(format!("; unit, name: {}", unit.unit_name));
        } else {
            lines.push(format!("; unit"));
        }
        if !unit.name.is_empty() {
            lines.push(format!(";  name: {}", unit.name));
        }
        let mut key = String::new();
        // symbols as constants
        if !unit.ext_symbols.is_empty()
            && unit.code.is_empty()
            && unit.data.is_empty()
            && unit.symbols.is_empty()
        {
            for es in &unit.ext_symbols {
                if es.1 > 0x20_000 && es.1 <= 0xff_ffff {
                    lines.push(format!("{:12}  EQU    #${:06x}", es.0, es.1));
                } else {
                    lines.push(format!("{:12}  EQU    #{}", es.0, es.1));
                }
                groups.push(StringGroup {
                    key: es.0.clone(),
                    lines: lines.clone(),
                });
                lines.clear();
            }
            continue;
        } else if !unit.code.is_empty() {
            // code
            lines.push(format!(";  symbols: {:?}", unit.symbols));
            lines.push(format!(";  ext_symbols (hex): {:x?}", unit.ext_symbols));
            lines.push(format!(";    {:?}", unit.code));
            lines.push(format_disassembly(&unit.code, &unit.ext_symbols));
            // TODO: should split code into pieces so that functions can be sorted by name
            key = unit
                .ext_symbols
                .first()
                .unwrap_or(&("???".to_string(), 0))
                .0
                .clone();
        } else if !unit.data.is_empty() {
            // data
            lines.push(format!(";  symbols: {:?}", unit.symbols));
            lines.push(format!(";  ext_symbols (hex): {:x?}", unit.ext_symbols));
            if let Some(esym) = unit.ext_symbols.first() {
                lines.push(format!("{}:", esym.0));
                lines.push(format!("    {:?}", unit.data));
                key = esym.0.clone();
            }
        } else {
            lines.push(format!(";  code: {:?}", unit.code));
            lines.push(format!(";  data: {:?}", unit.data));
            lines.push(format!(";  symbols: {:?}", unit.symbols));
            lines.push(format!(";  ext_symbols (hex): {:x?}", unit.ext_symbols));
            key = unit.unit_name.clone();
        }
        groups.push(StringGroup { key, lines });
    }
    if sort {
        groups.sort_by(|a, b| a.key.cmp(&b.key));
    }
    for group in &groups {
        for line in &group.lines {
            println!("{}", line);
        }
    }
}

#[allow(unused_variables)]
fn parse_file(file: &mut File) -> std::io::Result<Vec<HunkUnit>> {
    let mut result = vec![];
    let mut hunk_unit = HunkUnit::new();

    let flen = file.seek(std::io::SeekFrom::End(0))?;
    file.seek(std::io::SeekFrom::Start(0))?;
    while file.stream_position()? < flen {
        let pos = file.stream_position()?;
        let htype = read_u32(file)?;
        match htype {
            HUNK_UNIT => {
                hunk_unit.unit_name = read_string(file)?;
            }
            HUNK_NAME => {
                if !hunk_unit.name.is_empty() {
                    eprintln!("WARNING: multiple HUNK_NAME hunks!");
                }
                hunk_unit.name = read_string(file)?;
            }
            HUNK_CODE => {
                if !hunk_unit.code.is_empty() {
                    eprintln!("WARNING: multiple HUNK_CODE hunks!");
                }
                let code = trim_end_zeros(&read_data(file)?);
                hunk_unit.code = code;
            }
            HUNK_DATA => {
                if !hunk_unit.data.is_empty() {
                    eprintln!("WARNING: multiple HUNK_DATA hunks!");
                }
                let data = read_data(file)?;
                hunk_unit.data = data;
            }
            HUNK_EXT => loop {
                let sym_type = read_u8(file)?;
                let sym_name_len = read_u24(file)?;
                if sym_name_len == 0 {
                    break;
                }
                let sym_name = read_bytes_as_string(file, 4 * sym_name_len as usize)?;
                if sym_type < 0x80 {
                    let sym_value = read_i32(file)?;
                    hunk_unit.ext_symbols.push((sym_name, sym_value));
                } else {
                    if sym_type == 0x82 || sym_type == 0x89 {
                        let _sc = read_u32(file)?;
                    }
                    let reflen = read_u32(file)?;
                    let mut offsets = Vec::new();
                    for _ in 0..reflen {
                        offsets.push(read_u32(file)?);
                    }
                }
            },
            HUNK_SYMBOL => loop {
                let _sym_type = read_u8(file)?;
                let sym_name_len = read_u24(file)?;
                if sym_name_len == 0 {
                    break;
                }
                let sym_name = read_bytes_as_string(file, 4 * sym_name_len as usize)?;
                let sym_value = read_i32(file)?;
                hunk_unit.symbols.push((sym_name, sym_value));
            },
            HUNK_RELOC32 => loop {
                let count = read_u32(file)?;
                if count == 0 {
                    break;
                }
                let _rel = read_u32(file)?;
                for _ in 0..count {
                    let _offset = read_u32(file)?;
                }
            },
            HUNK_DEBUG => {
                let mut l = read_u32(file)?;
                let _h0 = read_u32(file)?;
                let dtype = read_bytes(file, 4)?;
                if dtype == b"LINE" {
                    let ln = read_u32(file)?;
                    for _ in 0..ln {
                        let _nf = read_u32(file)?;
                    }
                    l -= 3 + ln;
                    while l > 0 {
                        let _dummy = read_u8(file)?;
                        let _linenumber = read_u24(file)?;
                        let _dummy2 = read_u8(file)?;
                        let _srcoffset = read_u24(file)?;
                        l -= 2;
                    }
                } else if dtype == b"OPTS" {
                    let _val = read_u32(file)?;
                }
            }
            HUNK_BSS => {
                let _segment_size = read_u32(file)?;
            }
            HUNK_DREL32 => {
                // TODO: this may be completely wrong
                let mut p = 1;
                loop {
                    let n = read_u32(file)?;
                    if n == 0 {
                        break;
                    }
                    p += n;
                    let _t = read_u32(file)?;
                    for _ in 0..n {
                        let _ = read_u32(file)?;
                    }
                }
                if p & 1 == 1 {
                    let _padding = read_u32(file)?;
                }
            }
            HUNK_END => {
                result.push(hunk_unit.clone());
                hunk_unit = HunkUnit::new();
            }
            _ => {
                eprintln!("error: unknown hunk: {}", htype);
                std::process::exit(1);
            }
        }
    }
    Ok(result)
}

fn read_bytes(read: &mut dyn Read, len: usize) -> std::io::Result<Vec<u8>> {
    let mut buf = Vec::new();
    buf.resize(len, 0);
    read.read_exact(&mut buf)?;
    Ok(buf)
}

fn read_bytes_as_string(read: &mut dyn Read, len: usize) -> std::io::Result<String> {
    let mut buf = Vec::new();
    buf.resize(len, 0);
    read.read_exact(&mut buf)?;
    Ok(String::from_utf8_lossy(&trim_end_zeros(&buf)).to_string())
}

fn read_string(read: &mut dyn Read) -> std::io::Result<String> {
    let mut lenbuf = [0u8; 4];
    read.read_exact(&mut lenbuf)?;
    lenbuf[0] = 0; // clear top byte, it is sometimes 1
    let mut s = String::new();
    let len = u32::from_be_bytes(lenbuf) * 4;
    for _ in 0..len {
        let mut chrbuf = [0u8; 1];
        read.read_exact(&mut chrbuf)?;
        if chrbuf[0] != 0 {
            s.push(chrbuf[0] as char);
        }
    }
    Ok(s)
}

fn read_data(read: &mut dyn Read) -> std::io::Result<Vec<u8>> {
    let mut lenbuf = [0u8; 4];
    read.read_exact(&mut lenbuf)?;
    lenbuf[0] = 0; // clear top byte, it is sometimes 1
    let mut s = Vec::new();
    let len = u32::from_be_bytes(lenbuf) * 4;
    for _ in 0..len {
        let mut chrbuf = [0u8; 1];
        read.read_exact(&mut chrbuf)?;
        s.push(chrbuf[0]);
    }
    Ok(s)
}

fn read_u8(read: &mut dyn Read) -> std::io::Result<u8> {
    let mut buf = [0u8; 1];
    read.read_exact(&mut buf)?;
    Ok(u8::from_be_bytes(buf))
}

fn read_u24(read: &mut dyn Read) -> std::io::Result<u32> {
    let mut buf = [0u8; 3];
    read.read_exact(&mut buf)?;
    let buf2 = [0, buf[0], buf[1], buf[2]];
    Ok(u32::from_be_bytes(buf2))
}

fn read_i32(read: &mut dyn Read) -> std::io::Result<i32> {
    let mut buf = [0u8; 4];
    read.read_exact(&mut buf)?;
    Ok(i32::from_be_bytes(buf))
}

fn read_u32(read: &mut dyn Read) -> std::io::Result<u32> {
    let mut buf = [0u8; 4];
    read.read_exact(&mut buf)?;
    Ok(u32::from_be_bytes(buf))
}

fn trim_end_zeros(data: &[u8]) -> Vec<u8> {
    let mut newdata = data.to_vec();
    loop {
        match newdata.last() {
            Some(v) => {
                if *v != 0 {
                    return newdata;
                }
                let _ = newdata.pop();
            }
            None => {
                return newdata;
            }
        }
    }
}

fn format_disassembly(code: &[u8], ext_symbols: &Vec<(String, i32)>) -> String {
    let mut address = 0;
    let mut result = vec![];
    loop {
        for es in ext_symbols {
            if es.1 as u32 == address {
                result.push(format!("{}:", es.0));
            }
        }
        match disas::disas(code, address) {
            Ok(instr) => {
                let mut line = String::new();
                line.push_str(&format!("{:08x} : ", address));
                for i in 0..5 {
                    if let Some(word) = instr.words.get(i) {
                        line.push_str(&format!("{:04x} ", word));
                    } else {
                        line.push_str(&format!("     "));
                    }
                }
                line.push_str(&format!("  {:8} {}", instr.instruction, instr.operands));
                if !instr.comment.is_empty() {
                    line.push_str(&format!("   ;{}", instr.comment));
                }
                result.push(format!("{}", line.trim().to_string()));
                address = instr.next_address;
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                // end of stream ends disassembling
                return result.join("\n");
            }
            Err(_) => {
                println!("error: can't print disassembly!");
                std::process::exit(1);
            }
        }
    }
}
