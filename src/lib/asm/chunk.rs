use std::collections::HashMap;
use std::fmt;
use std::intrinsics::transmute;
use std::sync::Once;

// 常量
#[derive(Debug, Clone)]
pub enum Const {
    String(String),
    Number(f64),
}

impl Const {
    pub fn new_str(s: &str) -> Self {
        Const::String(s.to_string())
    }

    pub fn new_num(n: f64) -> Self {
        Const::Number(n)
    }

    pub fn typ_id(&self) -> u8 {
        match self {
            Const::String(_) => 0,
            Const::Number(_) => 1,
        }
    }

    pub fn is_str(&self) -> bool {
        match self {
            Const::String(_) => true,
            _ => false,
        }
    }

    pub fn is_num(&self) -> bool {
        match self {
            Const::Number(_) => true,
            _ => false,
        }
    }

    pub fn num(&self) -> f64 {
        match self {
            Const::Number(v) => *v,
            _ => panic!(),
        }
    }

    pub fn str(&self) -> &str {
        match self {
            Const::String(v) => v.as_str(),
            _ => panic!(),
        }
    }

    pub fn eq(&self, c: &Const) -> bool {
        if self.typ_id() == c.typ_id() {
            let eq = match self {
                Const::Number(v) => *v == c.num(),
                Const::String(v) => v == c.str(),
            };
            return eq;
        }
        false
    }
}

// 一个带上upvalue的函数，是闭包，包含了名称、是否在堆栈上以及索引
#[derive(Debug, Clone)]
pub struct UpvalDesc {
    pub name: String,
    pub in_stack: bool,
    pub idx: u32,
}

// 变量
#[derive(Debug, Clone)]
pub struct Local {
    pub name: String,
}

/*
ABC 操作模式：
参数编码：这种模式的指令编码包括操作码、参数 A、参数 B、参数 C。
参数范围：参数 A、B、C 通常是 8 位（1 字节）的整数，因此每个参数的取值范围是 0 到 255。
用途：ABC 操作模式通常用于执行基本的操作，例如加载、存储、算术运算等。

ABx 操作模式：
参数编码：这种模式的指令编码包括操作码、参数 A 和参数 Bx。
参数范围：参数 A 通常是 8 位整数，而参数 Bx 通常是 18 位整数。参数 Bx 的范围更大，可以表示更大的常量索引或其他值。
用途：ABx 操作模式通常用于加载常量、跳转指令等需要较大参数范围的操作。

AsBx 操作模式：
参数编码：这种模式的指令编码包括操作码、参数 A 和参数 sBx。
参数范围：参数 A 通常是 8 位整数，而参数 sBx 通常是带符号的 18 位整数，允许正数和负数值。
用途：AsBx 操作模式通常用于有符号跳转指令，例如条件分支等。
*/
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum OpMode {
    ABC,
    ABx,
    AsBx,
}

#[derive(Clone)]
pub struct Inst {
    pub raw: u32,
}

impl Inst {
    pub fn new() -> Self {
        Inst { raw: 0 }
    }

    pub fn new_abc(op: OpCode, a: u32, b: u32, c: u32) -> Self {
        let mut inst = Inst::new();
        inst.set_op(op);
        inst.set_a(a);
        inst.set_b(b);
        inst.set_c(c);
        inst
    }

    pub fn new_a_bx(op: OpCode, a: u32, bx: u32) -> Self {
        let mut inst = Inst::new();
        inst.set_op(op);
        inst.set_a(a);
        inst.set_bx(bx);
        inst
    }

    pub fn new_a_sbx(op: OpCode, a: u32, sbx: i32) -> Self {
        let mut inst = Inst::new();
        inst.set_op(op);
        inst.set_a(a);
        inst.set_sbx(sbx);
        inst
    }

    pub fn a(&self) -> u32 {
        // 右移 6 位 (self.raw >> 6)：这将 self.raw 中的参数 A 位移到最低有效位，以便提取。
        // 与 0xff (& 0xff)：它将除参数 A 位之外的其他位都清零，保留参数 A 的值。0xff 是一个 8 位二进制数，所有位都为 1，因此这个操作保留了参数 A 的 8 位值。
        (self.raw >> 6) & 0xff
    }

    pub fn b(&self) -> u32 {
        // 右移 23 位 (self.raw >> 23)：这将 self.raw 中的参数 B 位移到最低有效位，以便提取。
        // 与 0x1ff (& 0x1ff)：它将除参数 B 位之外的其他位都清零，保留参数 B 的值。0x1ff 是一个 9 位二进制数，所有位都为 1，因此这个操作保留了参数 B 的 9 位值。
        (self.raw >> 23) & 0x1ff
    }

    pub fn c(&self) -> u32 {
        // 右移 14 位 (self.raw >> 14)：这将 self.raw 中的参数 C 位移到最低有效位，以便提取。
        // 与 0x1ff (& 0x1ff)：它将除参数 C 位之外的其他位都清零，保留参数 C 的值。0x1ff 是一个 9 位二进制数，所有位都为 1，因此这个操作保留了参数 C 的 9 位值。
        (self.raw >> 14) & 0x1ff
    }

    pub fn bx(&self) -> u32 {
        // 右移 14 位 (self.raw >> 14)：这将 self.raw 中的参数 Bx 位移到最低有效位，以便提取。
        // 与 0x3ffff (& 0x3ffff)：它将除参数 Bx 位之外的其他位都清零，保留参数 Bx 的值。0x3ffff 是一个 18 位二进制数，所有位都为 1，因此这个操作保留了参数 Bx 的 18 位值。
        (self.raw >> 14) & 0x3ffff
    }

    pub fn sbx(&self) -> i32 {
        // 右移 14 位 (self.raw >> 14)：这将 self.raw 中的参数 sBx 位移到最低有效位，以便提取。
        // 与 0x3ffff (& 0x3ffff)：它将除参数 sBx 位之外的其他位都清零，保留参数 sBx 的值。0x3ffff 是一个 18 位二进制数，所有位都为 1，因此这个操作保留了参数 sBx 的 18 位值。
        // as i32：这将结果转换为带符号的 32 位整数。
        // - 131071：这个操作是将参数 sBx 转换为有符号整数，因为它是相对于 131071 的偏移值。
        let t = ((self.raw >> 14) & 0x3ffff) as i32;
        t - 131071
    }

    pub fn op(&self) -> u32 {
        // 与 0x3f (& 0x3f)：它将 self.raw 中的操作码位之外的其他位都清零，保留操作码的值。0x3f 是一个 6 位二进制数，所有位都为 1，因此这个操作保留了操作码的 6 位值。
        self.raw & 0x3f
    }

    pub fn set_op(&mut self, op: OpCode) {
        // !0x3f 表示对 0x3f 取反，即将 0x3f 中的所有位设为 0，只保留其他位不变。
        // | (op as u32)：将新的操作码值 op 添加到 self.raw 中，从而更新操作码。
        self.raw = (self.raw & !0x3f) | (op as u32);
    }

    pub fn set_a(&mut self, a: u32) {
        // 0xff << 6 表示将 0xff 左移 6 位，得到一个掩码，用于清除参数 A 的位。
        // !(0xff << 6) 对这个掩码取反，即将除参数 A 位之外的其他位都清零，保留参数 A 的位。
        // (a << 6) 将参数 A 的值左移 6 位，以匹配参数 A 在 self.raw 中的位置。
        self.raw = (self.raw & !(0xff << 6)) | (a << 6);
    }

    pub fn set_b(&mut self, b: u32) {
        // 0x1ff << 23 表示将 0x1ff 左移 23 位，得到一个掩码，用于清除参数 B 的位。
        // !(0x1ff << 23) 对这个掩码取反，即将除参数 B 位之外的其他位都清零，保留参数 B 的位。
        // (b << 23) 将参数 B 的值左移 23 位，以匹配参数 B 在 self.raw 中的位置。
        self.raw = (self.raw & !(0x1ff << 23)) | (b << 23);
    }

    pub fn set_c(&mut self, c: u32) {
        // 0x1ff << 14 表示将 0x1ff 左移 14 位，得到一个掩码，用于清除参数 C 的位。
        // !(0x1ff << 14) 对这个掩码取反，即将除参数 C 位之外的其他位都清零，保留参数 C 的位。
        // (c << 14) 将参数 C 的值左移 14 位，以匹配参数 C 在 self.raw 中的位置。
        self.raw = (self.raw & !(0x1ff << 14)) | (c << 14);
    }

    pub fn set_bx(&mut self, bx: u32) {
        // 0x3ffff << 14 表示将 0x3ffff 左移 14 位，得到一个掩码，用于清除参数 Bx 的位。
        // !(0x3ffff << 14) 对这个掩码取反，即将除参数 Bx 位之外的其他位都清零，保留参数 Bx 的位。
        // (bx << 14) 将参数 Bx 的值左移 14 位，以匹配参数 Bx 在 self.raw 中的位置。
        self.raw = (self.raw & !(0x3ffff << 14)) | (bx << 14);
    }

    pub fn set_sbx(&mut self, mut sbx: i32) {
        // sbx += 131071：这是一个将参数 sbx 增加 131071 的操作。它将参数 sbx 转换为无符号整数并增加 131071，因为虚拟机中的 sBx 参数通常是相对于 131071 的偏移值。
        sbx += 131071;
        let sbx = sbx as u32;
        // 0x3ffff << 14 表示将 0x3ffff 左移 14 位，得到一个掩码，用于清除参数 sbx 的位。
        // !(0x3ffff << 14) 对这个掩码取反，即将除参数 sbx 位之外的其他位都清零，保留参数 sbx 的位。
        // (sbx << 14) 将参数 sbx 的值左移 14 位，以匹配参数 sbx 在 self.raw 中的位置。
        self.raw = (self.raw & !(0x3ffff << 14)) | (sbx << 14);
    }

    // converts an integer to a "floating point byte"
    pub fn int2fb(mut x: u32) -> u32 {
        let mut e = 0; /* exponent */
        if x < 8 {
            return x;
        }
        while x >= 8 << 4 {
            /* coarse steps */
            x = (x + 0xf) >> 4; /* x = ceil(x / 16) */
            e += 4;
        }
        while x >= 8 << 1 {
            /* fine steps */
            x = (x + 1) >> 1; /* x = ceil(x / 2) */
            e += 1;
        }
        ((e + 1) << 3) | (x - 8)
    }

    pub fn fb2int(x: u32) -> u32 {
        if x < 8 {
            return x;
        }
        ((x & 7) + 8) << ((x >> 3) - 1)
    }
}

impl fmt::Debug for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = OpCode::from_u32(self.op());
        match op.mode() {
            OpMode::ABC => write!(
                f,
                "{:#?}{{ A: {}, B: {}, C: {} }}",
                op,
                self.a(),
                self.b(),
                self.c()
            ),
            OpMode::ABx => write!(f, "{:#?}{{ A: {}, Bx: {} }}", op, self.a(), self.bx()),
            OpMode::AsBx => write!(f, "{:#?}{{ A: {}, sBx: {} }}", op, self.a(), self.sbx()),
        }
    }
}
// 函数模板
#[derive(Debug, Clone)]
pub struct FnTpl {
    // 参数数量
    pub param_cnt: u8,
    // 是否支持可变参数
    pub is_vararg: bool,
    // 指令序列
    pub code: Vec<Inst>,
    pub consts: Vec<Const>,
    // 记录函数中引用的上值（外部变量）
    pub upvals: Vec<UpvalDesc>,
    // 局部变量
    pub locals: Vec<Local>,
    // 表示在函数内定义的子函数
    pub fun_tpls: Vec<FnTpl>,
}

impl FnTpl {
    pub fn new() -> Self {
        FnTpl {
            param_cnt: 0,
            is_vararg: false,
            code: vec![],
            consts: vec![],
            upvals: vec![],
            locals: vec![],
            fun_tpls: vec![],
        }
    }
}

// 代码块
#[derive(Debug)]
pub struct Chunk {
    pub sig: &'static str,
    pub ver: u64,
    pub upval_cnt: u8,
    pub fun_tpl: FnTpl,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum OpCode {
    MOVE,
    LOADK,
    LOADKX,
    LOADBOO,
    LOADNUL,
    LOADUNDEF,
    GETUPVAL,
    GETTABUP,
    GETTABLE,
    SETTABUP,
    SETUPVAL,
    SETTABLE,
    NEWTABLE,
    NEWARRAY,
    INITARRAY,
    THIS,
    ADD,
    SUB,
    MUL,
    MOD,
    DIV,
    LT,
    LE,
    EQ,
    EQS,
    JMP,
    TEST,
    TESTSET,
    BITAND,
    BITOR,
    BITXOR,
    SHL,
    SAR,
    SHR,
    UNM,
    NOT,
    BITNOT,
    CLOSURE,
    CALL,
    RETURN,
    NEW,
}

static mut OPCODE_NAME: Option<HashMap<OpCode, &'static str>> = None;
static mut OPCODE_MODE: Option<HashMap<OpCode, OpMode>> = None;

macro_rules! gen_opcode_map {
  ($($op:expr => $name:expr, $mode:expr)*) => {
    {
      let mut op_name = HashMap::new();
      let mut op_mode = HashMap::new();
      $(
        op_name.insert($op, $name);
        op_mode.insert($op, $mode);
      )*
      (Some(op_name), Some(op_mode))
    }
  };
}

fn init_opcodes() {
    let (op_name, op_mode) = gen_opcode_map! {
      OpCode::MOVE => "MOVE", OpMode::ABC
      OpCode::LOADK => "LOADK", OpMode::ABx
      OpCode::LOADKX => "LOADKX", OpMode::ABx
      OpCode::LOADBOO => "LOADBOO", OpMode::ABC
      OpCode::LOADNUL => "LOADNUL", OpMode::ABC
      OpCode::LOADUNDEF => "LOADUNDEF", OpMode::ABC
      OpCode::GETUPVAL => "GETUPVAL", OpMode::ABC
      OpCode::GETTABUP => "GETTABUP", OpMode::ABC
      OpCode::GETTABLE => "GETTABLE", OpMode::ABC
      OpCode::SETTABUP => "SETTABUP", OpMode::ABC
      OpCode::SETUPVAL => "SETUPVAL", OpMode::ABC
      OpCode::SETTABLE => "SETTABLE", OpMode::ABC
      OpCode::NEWTABLE => "NEWTABLE", OpMode::ABC
      OpCode::NEWARRAY => "NEWARRAY", OpMode::ABC
      OpCode::INITARRAY => "INITARRAY", OpMode::ABC
      OpCode::THIS => "THIS", OpMode::ABC
      OpCode::ADD => "ADD", OpMode::ABC
      OpCode::SUB => "SUB", OpMode::ABC
      OpCode::MUL => "MUL", OpMode::ABC
      OpCode::MOD => "MOD", OpMode::ABC
      OpCode::DIV => "DIV", OpMode::ABC
      OpCode::LT => "LT", OpMode::ABC
      OpCode::LE => "LE", OpMode::ABC
      OpCode::EQ => "EQ", OpMode::ABC
      OpCode::EQS => "EQS", OpMode::ABC
      OpCode::JMP => "JMP", OpMode::AsBx
      OpCode::TEST => "TEST", OpMode::ABC
      OpCode::TESTSET => "TESTSET", OpMode::ABC
      OpCode::BITAND => "BITAND", OpMode::ABC
      OpCode::BITOR => "BITOR", OpMode::ABC
      OpCode::BITXOR => "BITXOR", OpMode::ABC
      OpCode::SHL => "SHL", OpMode::ABC
      OpCode::SAR => "SAR", OpMode::ABC
      OpCode::SHR => "SHR", OpMode::ABC
      OpCode::UNM => "UNM", OpMode::ABC
      OpCode::NOT => "NOT", OpMode::ABC
      OpCode::BITNOT => "BITNOT", OpMode::ABC
      OpCode::CLOSURE => "CLOSURE", OpMode::ABC
      OpCode::CALL => "CALL", OpMode::ABC
      OpCode::RETURN => "RETURN", OpMode::ABC
      OpCode::NEW => "NEW", OpMode::ABC
    };
    unsafe {
        OPCODE_NAME = op_name;
        OPCODE_MODE = op_mode;
    }
}

static INIT_OPCODE_DATA_ONCE: Once = Once::new();
pub fn init_opcode_data() {
    INIT_OPCODE_DATA_ONCE.call_once(|| {
        init_opcodes();
    });
}

pub fn op_to_name(op: &OpCode) -> &'static str {
    unsafe { OPCODE_NAME.as_ref().unwrap().get(op).unwrap() }
}

pub fn op_to_mode(op: &OpCode) -> &'static OpMode {
    unsafe { OPCODE_MODE.as_ref().unwrap().get(op).unwrap() }
}

impl OpCode {
    pub fn from_u32(x: u32) -> Self {
        unsafe { transmute(x as u8) }
    }

    pub fn mode(&self) -> OpMode {
        *op_to_mode(self)
    }

    pub fn eq(&self, op: u32) -> bool {
        OpCode::from_u32(op) == *self
    }
}

#[cfg(test)]
mod chunk_tests {
    use super::*;

    #[test]
    fn inst_test() {
        let mut inst = Inst::new();
        inst.set_op(OpCode::LOADUNDEF);
        inst.set_a(0);
        inst.set_b(1);
        assert_eq!(0, inst.a());
        assert_eq!(1, inst.b());

        let mut inst = Inst::new();
        inst.set_op(OpCode::LOADUNDEF);
        inst.set_a(1);
        inst.set_bx(20);
        assert_eq!(1, inst.a());
        assert_eq!(20, inst.bx());

        let mut inst = Inst::new();
        inst.set_op(OpCode::LOADUNDEF);
        inst.set_a(1);
        inst.set_sbx(-20);
        assert_eq!(1, inst.a());
        assert_eq!(-20, inst.sbx());
    }

    #[test]
    fn opcode_test() {
        init_opcodes();

        assert_eq!(OpMode::ABC, OpCode::from_u32(0).mode());
        assert_eq!(OpMode::ABx, OpCode::from_u32(1).mode());
    }
}
