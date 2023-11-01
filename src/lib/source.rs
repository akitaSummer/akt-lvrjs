use std::collections::VecDeque;
use std::str::Chars;

// 处理源代码的字符流
pub struct Source<'a> {
    pub chs: Chars<'a>,
    pub peeked: VecDeque<char>,
    pub line: i32,
    pub column: i32,
}

pub const EOL: char = '\n';

// '\n'、 '\r'、 U+2028 or U+2029
pub fn is_line_terminator(c: char) -> bool {
    let cc = c as u32;
    cc == 0x0a || cc == 0x0d || cc == 0x2028 || cc == 0x2029
}

impl<'a> Source<'a> {
    pub fn new(code: &'a String) -> Self {
        Source {
            chs: code.chars(),
            peeked: VecDeque::with_capacity(3),
            line: 1,
            column: 0,
        }
    }

    // 用于从字符流中获取下一个字符
    fn next_join_crlf(&mut self) -> Option<char> {
        match self.chs.next() {
            Some(c) => {
                if is_line_terminator(c) {
                    // 如果是回车符 '\r'，则进一步检查下一个字符
                    if c == '\r' {
                        // 如果下一个字符是换行符 '\n'，则跳过回车符 '\r'，并将换行符 '\n' 存储在 peeked 队列中
                        if let Some(c) = self.chs.next() {
                            // 如果下一个字符不是换行符 '\n'，则将下一个字符存储在 peeked 队列中，以便后续读取
                            if c != '\n' {
                                self.peeked.push_back(c);
                            }
                        }
                    }
                    Some(EOL)
                } else {
                    Some(c)
                }
            }
            _ => None,
        }
    }
    // 用于读取下一个字符，优先从 peeked 中读取
    pub fn read(&mut self) -> Option<char> {
        let c = match self.peeked.pop_front() {
            Some(c) => Some(c),
            _ => self.next_join_crlf(),
        };
        if let Some(c) = c {
            if c == EOL {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }
        c
    }

    // 用于预览下一个字符，如果 peeked 非空，则返回队列头部字符，否则通过 next_join_crlf 预读取字符，并将其存储在 peeked 中
    pub fn peek(&mut self) -> Option<char> {
        match self.peeked.front().cloned() {
            Some(c) => Some(c),
            _ => match self.next_join_crlf() {
                Some(c) => {
                    self.peeked.push_back(c);
                    Some(c)
                }
                _ => None,
            },
        }
    }

    // 测试预读的字符是否与指定的字符或字符序列匹配
    pub fn test_ahead(&mut self, ch: char) -> bool {
        match self.peek() {
            Some(c) => c == ch,
            _ => false,
        }
    }

    pub fn test_ahead_or(&mut self, c1: char, c2: char) -> bool {
        match self.peek() {
            Some(c) => c == c1 || c == c2,
            _ => false,
        }
    }

    pub fn test_ahead_chs(&mut self, chs: &[char]) -> bool {
        let mut pass = true;
        for i in 0..self.peeked.len() {
            pass = match self.peeked.get(i) {
                Some(c) => *c == chs[i],
                _ => false,
            };
            if !pass {
                return false;
            }
        }
        for i in self.peeked.len()..chs.len() {
            pass = match self.next_join_crlf() {
                Some(c) => {
                    self.peeked.push_back(c);
                    c == chs[i]
                }
                _ => false,
            };
            if !pass {
                return false;
            }
        }
        pass
    }

    pub fn test_ahead2(&mut self, c1: char, c2: char) -> bool {
        self.test_ahead_chs(&[c1, c2])
    }

    pub fn test_ahead3(&mut self, c1: char, c2: char, c3: char) -> bool {
        self.test_ahead_chs(&[c1, c2, c3])
    }

    // 前进到下一个字符
    pub fn advance(&mut self) {
        self.read();
    }

    pub fn advance2(&mut self) {
        self.read();
        self.read();
    }
}

#[cfg(test)]
mod source_tests {
    use super::*;

    #[test]
    fn peekable_peek() {
        let code = String::from("hello world");
        let mut src = Source::new(&code);
        assert_eq!('h', src.peek().unwrap());
        assert_eq!('h', src.peek().unwrap());
        src.read();
        assert_eq!('e', src.peek().unwrap());
    }

    #[test]
    fn peekable_ahead() {
        let code = String::from("hello world");
        let mut src = Source::new(&code);
        assert!(src.test_ahead('h'));
        assert_eq!('h', src.peek().unwrap());
        assert!(src.test_ahead_chs(&['h', 'e']));
        assert_eq!('h', src.peek().unwrap());
        src.read();
        assert_eq!('e', src.peek().unwrap());
        assert!(src.test_ahead_chs(&['e', 'l', 'l']));
        src.read();
        src.read();
        src.read();
        src.read();
        assert_eq!(' ', src.peek().unwrap());
    }

    #[test]
    fn join_crlf() {
        let code = String::from("1\u{0d}\u{0a}2\u{0d}3\u{0a}");
        let mut src = Source::new(&code);
        assert!(src.test_ahead_chs(&['1', EOL]));
        src.read();
        assert!(src.test_ahead(EOL));
        assert_eq!(EOL, src.read().unwrap());
        assert_eq!((2, 0), (src.line, src.column));
        src.read();
        src.read();
        assert_eq!((3, 0), (src.line, src.column));
        src.read();
        src.read();
        assert_eq!((4, 0), (src.line, src.column));
        assert_eq!(None, src.read());
    }

    #[test]
    fn line_terminator() {
        let code = String::from("\u{2028}\u{0a}\u{0d}\u{0a}");
        let mut src = Source::new(&code);
        assert_eq!((1, 0), (src.line, src.column));
        assert_eq!(EOL, src.read().unwrap());
        assert_eq!((2, 0), (src.line, src.column));
        assert_eq!(EOL, src.read().unwrap());
        assert_eq!((3, 0), (src.line, src.column));
        assert_eq!(EOL, src.read().unwrap());
        assert_eq!((4, 0), (src.line, src.column));
    }

    #[test]
    fn peek() {
        let code = String::from("\u{2028}\u{0a}\u{0d}\u{0a}");
        let mut src = Source::new(&code);
        assert_eq!(EOL, src.peek().unwrap());
        src.read();
        assert_eq!(EOL, src.peek().unwrap());
        src.read();
        assert_eq!(EOL, src.peek().unwrap());
    }
}
