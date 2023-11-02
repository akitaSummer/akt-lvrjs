use crate::source::*;
use crate::token::*;
use std::char;
use std::collections::VecDeque;
use std::rc::Rc;
use std::str;
use std::u32;
use unic_ucd::GeneralCategory;

// 判断字符是否是空白字符，排除行终止符。
fn is_whitespace(c: char) -> bool {
    !is_line_terminator(c) && c.is_whitespace()
}

// 判断一个字符是否是 Unicode 字母
fn is_unicode_letter(c: char) -> bool {
    // 是否为大写字母或小写字母
    if c.is_uppercase() || c.is_lowercase() {
        return true;
    }
    match GeneralCategory::of(c) {
    // 大写标题字母
    GeneralCategory::TitlecaseLetter
    // 修饰符字母
    | GeneralCategory::ModifierLetter
    // 其他字母，包括小写字母、大写字母以外的字母
    | GeneralCategory::OtherLetter
    // 字母数字，通常是字母与数字的混合
    | GeneralCategory::LetterNumber => true,
    _ => false,
  }
}

// 字符是否可以作为标识符的开头
fn is_id_start(c: char) -> bool {
    if is_unicode_letter(c) {
        return true;
    }
    match c {
        '$' | '_' | '\\' => true,
        _ => false,
    }
}

// 字符是否可以作为标识符的一部分
fn is_id_part(c: char) -> bool {
    if is_id_start(c) {
        return true;
    }
    let cc = c as u32;
    // Zero Width Non-Joiner and Zero Width Joiner
    if cc == 0x200c || cc == 0x200d {
        return true;
    }
    match GeneralCategory::of(c) {
    // 非间隔标记，包括一些变音符号等
    GeneralCategory::NonspacingMark
    // 间隔标记，包括一些空格修饰符等
    | GeneralCategory::SpacingMark
    // 十进制数字，包括数字 0 到 9
    | GeneralCategory::DecimalNumber
    // 连接标点，包括下划线等
    | GeneralCategory::ConnectorPunctuation => true,
    _ => false,
  }
}

// 是否是单字符转义字符
fn is_single_escape_ch(c: char) -> bool {
    match c {
        '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' => true,
        _ => false,
    }
}

// 将转义字符转换为实际字符
fn escape_ch(c: char) -> char {
    match c {
        '\'' => '\'',
        '"' => '"',
        '\\' => '\\',
        'b' => '\x08',
        'f' => '\x0c',
        'n' => '\x0a',
        'r' => '\x0d',
        't' => '\x09',
        'v' => '\x0b',
        _ => panic!(),
    }
}

// 字符是否不是单字符转义字符、行终止符、ASCII 数字、x、u
fn is_non_escape_ch(c: char) -> bool {
    !is_single_escape_ch(c) && !is_line_terminator(c) && !c.is_ascii_digit() && c != 'x' && c != 'u'
}

pub struct TokenNextNewline {
    tok: Rc<Token>,
    next_is_line_terminator: bool,
}

pub struct Lexer<'a> {
    src: Source<'a>,
    tok: Rc<Token>,
    pub next_is_line_terminator: bool,
    peeked: VecDeque<TokenNextNewline>,
}

#[derive(Debug)]
pub struct LexError {
    pub msg: String,
}

impl LexError {
    fn new(msg: String) -> Self {
        LexError { msg }
    }

    pub fn default() -> Self {
        LexError {
            msg: "".to_string(),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: Source<'a>) -> Self {
        Lexer {
            src,
            tok: Rc::new(Token::Nil),
            next_is_line_terminator: false,
            peeked: VecDeque::new(),
        }
    }

    fn next_(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();
        // 解析标识符、数字、字符串还是符号
        if self.ahead_is_id_start() {
            self.read_name()
        } else if self.ahead_is_decimal_int() {
            self.read_numeric()
        } else if self.ahead_is_string_start() {
            let t = self.src.read().unwrap();
            self.read_string(t)
        } else if !self.ahead_is_eof() {
            self.read_symbol()
        } else {
            Ok(Token::Eof(EofData {
                loc: self.loc().clone(),
            }))
        }
    }

    // 查看下一个token，但不会将其从输入中移除
    pub fn peek(&mut self) -> Result<Rc<Token>, LexError> {
        match self.peeked.front() {
            // 如果 peeked 中有已经查看的token，它会返回该token
            Some(tok) => Ok(tok.tok.clone()),
            // 调用 next_ 方法来获取下一个token，将其存储在 peeked 中，然后返回
            _ => match self.next_() {
                Ok(tok) => {
                    let tok = Rc::new(tok);
                    let next_is_line_terminator = self.ahead_is_line_terminator_or_eof();
                    self.peeked.push_back(TokenNextNewline {
                        tok: tok.clone(),
                        next_is_line_terminator,
                    });
                    Ok(tok)
                }
                Err(e) => Err(e),
            },
        }
    }

    // 获取下一个token，并将其从输入中移除
    pub fn next(&mut self) -> Result<Rc<Token>, LexError> {
        // 检查 peeked 中是否有已经查看的token
        match self.peeked.pop_front() {
            Some(tn) => {
                self.tok = tn.tok;
                self.next_is_line_terminator = tn.next_is_line_terminator;
                Ok(self.tok.clone())
            }
            // 调用 next_ 方法来获取下一个token，并存储在 peeked 中以备后续使用
            _ => match self.next_() {
                Ok(tok) => {
                    self.tok = Rc::new(tok);
                    self.next_is_line_terminator = self.ahead_is_line_terminator_or_eof();
                    Ok(self.tok.clone())
                }
                Err(e) => Err(e),
            },
        }
    }

    // 获取下一个token，但不会将其从输入中移除
    pub fn advance(&mut self) {
        match self.next() {
            Ok(_) => (),
            Err(e) => panic!("{}", e.msg),
        }
    }

    // 解析 Unicode 转义序列
    fn read_unicode_escape_seq(&mut self) -> Option<char> {
        let mut hex = [0, 0, 0, 0];
        for i in 0..hex.len() {
            match self.src.read() {
                Some(c) => {
                    if c.is_ascii_hexdigit() {
                        hex[i] = c as u8;
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        }
        let hex = str::from_utf8(&hex).unwrap();
        match u32::from_str_radix(hex, 16) {
            Ok(i) => match char::from_u32(i) {
                Some(c) => Some(c),
                _ => None, // deformed unicode
            },
            _ => None, // deformed hex digits
        }
    }

    // 检查下一个字符是否是标识符的起始字符
    fn ahead_is_id_start(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => is_id_start(c),
            _ => false,
        }
    }
    // 检查下一个字符是否是起始标识符的部分字符
    fn ahead_is_id_part(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => is_id_part(c),
            _ => false,
        }
    }

    fn errmsg(&self) -> String {
        format!(
            "Unexpected char at line: {} column: {}",
            self.src.line, self.src.column
        )
    }

    pub fn pos(&self) -> Position {
        Position {
            line: self.src.line,
            column: self.src.column,
        }
    }

    pub fn loc(&self) -> SourceLoc {
        SourceLoc {
            start: self.pos(),
            end: Position::new(),
        }
    }

    fn fin_loc(&self, loc: SourceLoc) -> SourceLoc {
        let mut loc = loc;
        loc.end = self.pos();
        loc
    }

    fn read_escape_unicode(&mut self, bs: char) -> Result<char, LexError> {
        // Unicode 转义序列以 \u 开头，后跟 4 个十六进制数字
        if bs == '\\' && self.src.test_ahead('u') {
            self.src.advance();
            match self.read_unicode_escape_seq() {
                Some(ec) => Ok(ec),
                _ => Err(LexError::new(self.errmsg())),
            }
        } else {
            Ok(bs)
        }
    }

    fn read_id_part(&mut self) -> Result<String, LexError> {
        let mut val = vec![];
        loop {
            if self.ahead_is_id_part() {
                let c = self.src.read().unwrap();
                match self.read_escape_unicode(c) {
                    Ok(cc) => val.push(cc),
                    Err(e) => return Err(e),
                }
            } else {
                break;
            }
        }
        Ok(val.into_iter().collect())
    }

    // 解析keyword bool null和标识符
    pub fn read_name(&mut self) -> Result<Token, LexError> {
        let loc = self.loc();
        let mut c = self.src.read().unwrap();
        match self.read_escape_unicode(c) {
            Ok(cc) => c = cc,
            Err(e) => return Err(e),
        }
        let mut val = vec![c];
        match self.read_id_part() {
            Ok(cc) => val.extend(cc.chars()),
            Err(e) => return Err(e),
        }
        let val: String = val.into_iter().collect();
        if is_keyword(&val) {
            Ok(Token::Keyword(KeywordData {
                kind: name_to_keyword(&val),
                loc: self.fin_loc(loc),
            }))
        } else if is_ctx_keyword(&val) {
            Ok(Token::ContextualKeyword(CtxKeywordData {
                kind: name_to_ctx_keyword(&val),
                loc: self.fin_loc(loc),
            }))
        } else if is_bool(&val) {
            Ok(Token::BooleanLiteral(BooleanLiteralData {
                kind: name_to_bool(&val),
                loc: self.fin_loc(loc),
            }))
        } else if is_null(&val) {
            Ok(Token::NullLiteral(NullLiteralData {
                loc: self.fin_loc(loc),
            }))
        } else {
            Ok(Token::Identifier(IdentifierData {
                value: val,
                loc: self.fin_loc(loc),
            }))
        }
    }

    // 解析数字
    fn read_decimal_digits(&mut self) -> String {
        let mut ret = String::new();
        loop {
            if let Some(c) = self.src.peek() {
                if c.is_ascii_digit() {
                    ret.push(self.src.read().unwrap());
                    continue;
                }
            }
            break;
        }
        ret
    }

    // 科学计算
    fn read_exponent(&mut self) -> Result<String, LexError> {
        let mut ret = String::new();
        // 使用 e|E
        ret.push(self.src.read().unwrap());
        if let Some(c) = self.src.peek() {
            if c == '+' || c == '-' {
                ret.push(self.src.read().unwrap());
            }
            let digits = self.read_decimal_digits();
            if digits.is_empty() {
                return Err(LexError::new(self.errmsg()));
            } else {
                ret.push_str(digits.as_str());
            }
        }
        Ok(ret)
    }

    fn read_decimal_int_part(&mut self) -> String {
        let mut ret = String::new();
        let c = self.src.read().unwrap();
        ret.push(c);
        if c == '0' {
            return ret;
        }
        ret.push_str(self.read_decimal_digits().as_str());
        ret
    }

    // 解析字符串
    fn read_decimal(&mut self) -> Result<String, LexError> {
        let c = self.src.peek().unwrap();
        let mut ret = String::new();
        let digits_opt = c != '.';
        if c.is_ascii_digit() {
            ret.push_str(self.read_decimal_int_part().as_str());
        }
        // 处理小数部分：这部分代码涉及解析浮点数，即具有小数部分的数字。根据 JavaScript 语法，浮点数的小数部分是可选的。
        // 如果浮点数以小数点 . 开头，那么下一个字符（或字符序列）必须是小数部分。这意味着小数点后面必须紧跟着数字。
        if self.src.test_ahead('.') {
            ret.push(self.src.read().unwrap());
            let digits = self.read_decimal_digits();
            if digits.is_empty() && !digits_opt {
                return Err(LexError::new(self.errmsg()));
            }
            ret.push_str(digits.as_str());
        }
        //如果浮点数以非零数字开头，那么小数部分中的数字是可选的。这意味着可以有整数值而无小数部分。
        if self.src.test_ahead_or('e', 'E') {
            match self.read_exponent() {
                Ok(s) => ret.push_str(s.as_str()),
                err @ Err(_) => return err,
            }
        }

        Ok(ret)
    }

    // 解析十六进制
    fn read_hex(&mut self) -> Result<String, LexError> {
        let mut ret = String::new();
        ret.push(self.src.read().unwrap());
        ret.push(self.src.read().unwrap());
        let mut digits = vec![];
        loop {
            match self.src.peek() {
                Some(c) => {
                    if c.is_ascii_hexdigit() {
                        digits.push(self.src.read().unwrap());
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        if digits.len() == 0 {
            Err(LexError::new(self.errmsg()))
        } else {
            let digits: String = digits.iter().collect();
            ret.push_str(digits.as_str());
            Ok(ret)
        }
    }

    fn ahead_is_decimal_int(&mut self) -> bool {
        if let Some(c) = self.src.peek() {
            if c == '.' {
                match self.src.chs.next() {
                    Some(cc) => {
                        self.src.peeked.push_back(cc);
                        cc.is_ascii_digit()
                    }
                    _ => false,
                }
            } else {
                c.is_ascii_digit()
            }
        } else {
            false
        }
    }

    // 解析数字
    pub fn read_numeric(&mut self) -> Result<Token, LexError> {
        let loc = self.loc();
        let value: Result<String, LexError>;
        let mut is_hex = false;
        if self.src.test_ahead('0') {
            if let Some(c) = self.src.chs.next() {
                self.src.peeked.push_back(c);
                if c == 'x' || c == 'X' {
                    is_hex = true;
                }
            }
        }
        if is_hex {
            value = self.read_hex();
        } else {
            value = self.read_decimal();
        }
        match value {
            Ok(v) => Ok(Token::NumericLiteral(NumericLiteralData {
                value: v,
                loc: self.fin_loc(loc),
            })),
            Err(e) => Err(e),
        }
    }

    fn ahead_is_string_start(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => c == '\'' || c == '"',
            _ => false,
        }
    }

    // 解析转译
    fn read_string_escape_seq(&mut self) -> Result<Option<char>, LexError> {
        self.src.advance(); // 反斜杠
        match self.src.read() {
            Some(mut c) => {
                // 单字符转义字符
                if is_single_escape_ch(c) {
                    c = escape_ch(c);
                    // \0：表示空字符 (NUL)
                } else if c == '0' {
                    c = '\0';
                    // 0 [lookahead ∉ DecimalDigit]
                    if let Some(c) = self.src.peek() {
                        if c.is_ascii_digit() {
                            return Err(LexError::new(self.errmsg()));
                        }
                    }
                    // \x：表示十六进制转义
                } else if c == 'x' {
                    let mut hex = [0, 0];
                    for i in 0..hex.len() {
                        if let Some(cc) = self.src.read() {
                            if cc.is_ascii_hexdigit() {
                                hex[i] = cc as u8;
                                continue;
                            }
                        }
                        return Err(LexError::new(self.errmsg()));
                    }
                    let hex = str::from_utf8(&hex).unwrap();
                    c = char::from_u32(u32::from_str_radix(hex, 16).ok().unwrap()).unwrap()
                    // \u：表示 Unicode 转义
                } else if c == 'u' {
                    match self.read_unicode_escape_seq() {
                        Some(ec) => c = ec,
                        _ => return Err(LexError::new(self.errmsg())),
                    }
                    // 换行符
                } else if is_line_terminator(c) {
                    if c == '\r' && self.src.test_ahead('\n') {
                        self.src.advance();
                        return Err(LexError::new(self.errmsg()));
                    }
                    return Ok(None);
                } else if is_non_escape_ch(c) {
                    // todo
                } else {
                    return Err(LexError::new(self.errmsg()));
                }
                Ok(Some(c))
            }
            _ => Err(LexError::new(self.errmsg())),
        }
    }

    // 解析字符串
    fn read_string(&mut self, t: char) -> Result<Token, LexError> {
        let loc = self.loc();
        let mut ret = String::new();
        loop {
            match self.src.peek() {
                Some(c) => {
                    if c == t {
                        self.src.advance();
                        break;
                    } else if c == '\\' {
                        match self.read_string_escape_seq() {
                            Ok(Some(c)) => ret.push(c),
                            Err(e) => return Err(e),
                            _ => (),
                        }
                    } else {
                        ret.push(self.src.read().unwrap());
                    }
                }
                _ => break,
            }
        }
        Ok(Token::StringLiteral(StringLiteralData {
            value: ret,
            loc: self.fin_loc(loc),
        }))
    }

    fn ahead_is_regexp_start(&mut self) -> bool {
        match self.src.peek() {
            Some('/') => self.tok.is_before_expr(),
            _ => false,
        }
    }

    fn ahead_is_regexp_backslash_seq(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => c == '\\',
            _ => false,
        }
    }

    fn read_regexp_backslash_seq(&mut self) -> Result<String, LexError> {
        let mut ret = vec![self.src.read().unwrap()];
        if self.ahead_is_line_terminator_or_eof() {
            Err(LexError::new(self.errmsg()))
        } else {
            ret.push(self.src.read().unwrap());
            Ok(ret.into_iter().collect())
        }
    }

    fn ahead_is_regexp_class(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => c == '[',
            _ => false,
        }
    }

    fn read_regexp_class(&mut self) -> Result<String, LexError> {
        let mut ret = vec![self.src.read().unwrap()];
        loop {
            if self.ahead_is_regexp_backslash_seq() {
                match self.read_regexp_backslash_seq() {
                    Ok(s) => ret.extend(s.chars()),
                    Err(e) => return Err(e),
                }
            } else if self.ahead_is_line_terminator_or_eof() {
                return Err(LexError::new(self.errmsg()));
            } else {
                let c = self.src.read().unwrap();
                ret.push(c);
                if c == ']' {
                    break;
                }
            };
        }
        Ok(ret.into_iter().collect())
    }

    fn read_regexp_body(&mut self) -> Result<String, LexError> {
        let mut ret = vec![self.src.read().unwrap()];
        loop {
            if self.ahead_is_regexp_backslash_seq() {
                match self.read_regexp_backslash_seq() {
                    Ok(s) => ret.extend(s.chars()),
                    Err(e) => return Err(e),
                }
            } else if self.ahead_is_regexp_class() {
                match self.read_regexp_class() {
                    Ok(s) => ret.extend(s.chars()),
                    Err(e) => return Err(e),
                }
            } else if self.ahead_is_line_terminator_or_eof() {
                return Err(LexError::new(self.errmsg()));
            } else {
                let c = self.src.read().unwrap();
                ret.push(c);
                if c == '/' {
                    break;
                }
            }
        }
        Ok(ret.into_iter().collect())
    }

    fn read_regexp_flags(&mut self) -> Result<String, LexError> {
        let mut ret = vec![];
        loop {
            if self.ahead_is_id_part() {
                match self.read_id_part() {
                    Ok(s) => ret.extend(s.chars()),
                    Err(e) => return Err(e),
                }
            } else {
                break;
            }
        }
        Ok(ret.into_iter().collect())
    }

    // 解析正则
    fn read_regexp(&mut self) -> Result<Token, LexError> {
        let loc = self.loc();
        match self.read_regexp_body() {
            Ok(mut body) => match self.read_regexp_flags() {
                Ok(flags) => {
                    body.push_str(flags.as_str());
                    Ok(Token::RegExpLiteral(RegExpLiteralData {
                        value: body,
                        loc: self.fin_loc(loc),
                    }))
                }
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        }
    }

    // 解析符号
    fn read_symbol(&mut self) -> Result<Token, LexError> {
        if self.ahead_is_regexp_start() {
            return self.read_regexp();
        }
        let loc = self.loc();
        let mut s = vec![];
        loop {
            if self.ahead_is_whitespace_or_eof() {
                break;
            }
            let c = self.src.peek().unwrap();
            match c {
                '{' | '}' | '(' | ')' | '[' | ']' | '.' | ';' | ',' | '?' | ':' => {
                    s.push(self.src.read().unwrap());
                    break;
                }
                '<' => {
                    // < <<= << <=
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead2('<', '=') {
                        s.push(self.src.read().unwrap());
                        s.push(self.src.read().unwrap());
                    } else if self.src.test_ahead_or('<', '=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                '>' => {
                    // > >>>= >>= >> >=
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead3('>', '>', '=') {
                        s.push(self.src.read().unwrap());
                        s.push(self.src.read().unwrap());
                        s.push(self.src.read().unwrap());
                    } else if self.src.test_ahead2('>', '=') {
                        s.push(self.src.read().unwrap());
                        s.push(self.src.read().unwrap());
                    } else if self.src.test_ahead_or('>', '=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                '=' => {
                    // = === ==
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead2('=', '=') {
                        s.push(self.src.read().unwrap());
                        s.push(self.src.read().unwrap());
                    } else if self.src.test_ahead('=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                '!' => {
                    // ! != !==
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead2('=', '=') {
                        s.push(self.src.read().unwrap());
                        s.push(self.src.read().unwrap());
                    } else if self.src.test_ahead('=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                '+' => {
                    // + ++ +=
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead_or('+', '=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                '-' => {
                    // - -- -=
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead_or('-', '=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                '&' => {
                    // & && &=
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead_or('&', '=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                '|' => {
                    // | || |=
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead_or('|', '=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                '*' | '/' | '%' | '^' | '~' => {
                    // pattern pattern=
                    s.push(self.src.read().unwrap());
                    if self.src.test_ahead('=') {
                        s.push(self.src.read().unwrap());
                    }
                    break;
                }
                _ => return Err(LexError::new(self.errmsg())),
            }
        }
        let s: String = s.into_iter().collect();
        if is_symbol(&s) {
            Ok(Token::Symbol(SymbolData {
                kind: name_to_symbol(&s),
                loc: self.fin_loc(loc),
            }))
        } else {
            Err(LexError::new(self.errmsg()))
        }
    }

    // 跳过注释
    fn skip_comment_single(&mut self) {
        self.src.advance2();
        loop {
            match self.src.read() {
                Some(EOL) | None => break,
                _ => (),
            };
        }
    }

    fn skip_comment_multi(&mut self) {
        self.src.advance2();
        loop {
            match self.src.read() {
                Some('*') => {
                    if self.src.test_ahead('/') {
                        self.src.advance();
                        break;
                    }
                }
                None => break,
                _ => (),
            };
        }
    }

    pub fn ahead_is_line_terminator_or_eof(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => is_line_terminator(c),
            _ => true,
        }
    }

    pub fn ahead_is_eof(&mut self) -> bool {
        match self.src.peek() {
            Some(_) => false,
            _ => true,
        }
    }

    pub fn ahead_is_whitespace(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => is_whitespace(c),
            _ => false,
        }
    }

    pub fn ahead_is_whitespace_or_line_terminator(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => c.is_whitespace(),
            _ => false,
        }
    }

    pub fn ahead_is_whitespace_or_eof(&mut self) -> bool {
        match self.src.peek() {
            Some(c) => is_whitespace(c),
            _ => true,
        }
    }

    // 跳过空白字符
    pub fn skip_whitespace(&mut self) {
        loop {
            if self.ahead_is_whitespace_or_line_terminator() {
                self.src.read();
            } else if self.src.test_ahead2('/', '/') {
                self.skip_comment_single();
            } else if self.src.test_ahead2('/', '*') {
                self.skip_comment_multi();
            } else {
                break;
            }
        }
    }
}


#[cfg(test)]
mod lexer_tests {
  use super::*;

  #[test]
  fn skip_whitespace() {
    let code = String::from(
      " // this is a single-line comment
       /*
        * this is a multiline comment
        */ hello world
       ",
    );
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    lex.skip_whitespace();
    assert_eq!('h', lex.src.read().unwrap());
  }

  #[test]
  fn unicode_letter() {
    let mut c: char = '\u{01c5}'; // title case
    assert!(is_unicode_letter(c));
    c = '\u{1C90}'; // uppercase
    assert!(is_unicode_letter(c));
    c = '\u{10D0}'; // lowercase
    assert!(is_unicode_letter(c));
    c = '\u{0559}'; // modifier
    assert!(is_unicode_letter(c));
    c = '\u{0920}'; // other letter
    assert!(is_unicode_letter(c));
    c = '\u{2165}'; // letter number
    assert!(is_unicode_letter(c));
  }

  #[test]
  fn unicode_escape_seq() {
    let code = String::from("\\u01c5\\u0920\\u1x23");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    lex.src.advance2();
    assert_eq!('\u{01c5}', lex.read_unicode_escape_seq().unwrap());
    lex.src.advance2();
    assert_eq!('\u{0920}', lex.read_unicode_escape_seq().unwrap());
    lex.src.advance2();
    assert_eq!(None, lex.read_unicode_escape_seq());
  }

  #[test]
  fn read_name() {
    init_token_data();

    let code = String::from("\\u01c5\\u0920 a aᢅ break let true null");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    let mut tok = lex.read_name().ok().unwrap();
    assert_eq!("\u{01c5}\u{0920}", tok.id_data().value);

    lex.skip_whitespace();
    tok = lex.read_name().ok().unwrap();
    assert_eq!("a", tok.id_data().value);

    lex.skip_whitespace();
    tok = lex.read_name().ok().unwrap();
    assert_eq!("a\u{1885}", tok.id_data().value);

    lex.skip_whitespace();
    tok = lex.read_name().ok().unwrap();
    assert_eq!("break", tok.keyword_data().kind.name());

    lex.skip_whitespace();
    tok = lex.read_name().ok().unwrap();
    assert_eq!("let", tok.ctx_keyword_data().kind.name());

    lex.skip_whitespace();
    tok = lex.read_name().ok().unwrap();
    assert_eq!("true", tok.bool_data().kind.name());

    lex.skip_whitespace();
    tok = lex.read_name().ok().unwrap();
    assert!(tok.is_null());
  }

  #[test]
  fn read_decimal() {
    let code = String::from("1 .1e1 1.e1 1.e+1 .1e-1");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    let mut val = lex.read_decimal().ok().unwrap();
    assert_eq!("1", val);
    lex.skip_whitespace();
    val = lex.read_decimal().ok().unwrap();
    assert_eq!(".1e1", val);
    lex.skip_whitespace();
    val = lex.read_decimal().ok().unwrap();
    assert_eq!("1.e1", val);
    lex.skip_whitespace();
    val = lex.read_decimal().ok().unwrap();
    assert_eq!("1.e+1", val);
    lex.skip_whitespace();
    val = lex.read_decimal().ok().unwrap();
    assert_eq!(".1e-1", val);
  }

  #[test]
  fn read_numeric() {
    let code = String::from("1 .1e1 0xa1 0X123");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    let mut tok = lex.read_numeric().ok().unwrap();
    assert_eq!("1", tok.num_data().value);

    lex.skip_whitespace();
    tok = lex.read_numeric().ok().unwrap();
    assert_eq!(".1e1", tok.num_data().value);

    lex.skip_whitespace();
    tok = lex.read_numeric().ok().unwrap();
    assert_eq!("0xa1", tok.num_data().value);

    lex.skip_whitespace();
    tok = lex.read_numeric().ok().unwrap();
    assert_eq!("0X123", tok.num_data().value);
  }

  #[test]
  fn read_string() {
    let code = String::from("'hello world' \"hello \\\n\\u4E16\\u754C\"");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    lex.src.advance();
    let mut tok = lex.read_string('\'').ok().unwrap();
    if let Token::StringLiteral(s) = tok {
      assert_eq!("hello world", s.value);
    }

    lex.skip_whitespace();
    lex.src.advance();
    tok = lex.read_string('"').ok().unwrap();
    if let Token::StringLiteral(s) = tok {
      assert_eq!("hello 世界", s.value);
    }
  }

  #[test]
  fn next() {
    init_token_data();

    let code = String::from("'hello world' break {} /test/ig");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    let mut tok = lex.next();
    assert_eq!("hello world", tok.ok().unwrap().str_data().value);

    tok = lex.next();
    assert_eq!("break", tok.ok().unwrap().keyword_data().kind.name());

    tok = lex.next();
    assert_eq!("{", tok.ok().unwrap().symbol_data().kind.name());

    tok = lex.next();
    assert_eq!("}", tok.ok().unwrap().symbol_data().kind.name());

    tok = lex.next();
    assert_eq!("/test/ig", tok.ok().unwrap().regexp_data().value);
  }

  #[test]
  fn loc() {
    init_token_data();

    let code = String::from("a\n  bcd");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    let mut tok = lex.next().ok().unwrap();
    let mut td = tok.id_data();
    assert_eq!("a", td.value);
    let mut loc = &td.loc;
    assert_eq!(1, loc.start.line);
    assert_eq!(0, loc.start.column);
    assert_eq!(1, loc.end.line);
    assert_eq!(1, loc.end.column);

    tok = lex.next().ok().unwrap();
    td = tok.id_data();
    loc = &td.loc;
    assert_eq!(2, loc.start.line);
    assert_eq!(2, loc.start.column);
    assert_eq!(2, loc.end.line);
    assert_eq!(5, loc.end.column);
  }
}
