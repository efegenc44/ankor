use crate::token::Token;

pub struct Lexer {
    chars: Vec<char>,
    index: usize,
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;

        if self.index >= self.chars.len() {
            return None;
        }

        let token = match self.current_char() {
            '+' => Plus,
            '-' => Minus,
            '*' => Star,
            '(' => LParen,
            ')' => RParen,
            '[' => LSquare,
            ']' => RSquare,
            '{' => LCurly,
            '}' => RCurly,
            '|' => Pipe,
            ',' => Comma,
            #[allow(clippy::suspicious_else_formatting)]
            '=' => if self.peek_is('=') { DoubleEqual } else 
                   if self.peek_is('>') { FatArrow } else { Equal },
            '!' => if self.peek_is('=') { BangEqual } else { Bang },
            '<' => if self.peek_is('=') { LessEqual } else { Less },
            '>' => if self.peek_is('=') { GreaterEqual } else { Greater },
            ';' => Semicolon,
            ':' => Colon,
            '.' => Dot,
            '\0' => End,

            non_trivial => return match non_trivial {
                ' ' | '\t' | '\r' | '\n' => {
                    self.advance();
                    self.next()
                }
                '0'..='9' => {
                    Some(self.lex_number())
                }
                ch if Self::valid_symbol_character(ch) => {
                    Some(self.lex_symbol())
                }
                _ => todo!("Error handling")
            }
        };
        self.advance();
        Some(token)
    }
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let chars = {
            let mut chars: Vec<_> = source.chars().collect();
            chars.push('\0');
            chars
        };
        Self { chars, index: 0 }
    }

    fn current_char(&self) -> &char {
        self.chars.get(self.index).unwrap()
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn peek_is(&mut self, ch: char) -> bool {
        if self.chars.get(self.index + 1).unwrap() == &ch {
            self.advance();
            true
        } else {
            false
        }
    }

    fn lex_number(&mut self) -> Token {
        use Token::*;

        let start = self.index;

        while let '0'..='9' = self.current_char() {
            self.advance();
        }

        if Self::valid_symbol_character(self.current_char()) {
            todo!("Error handling");
        }

        let number = self.chars[start..self.index].iter().collect();

        Integer(number)
    }

    fn lex_symbol(&mut self) -> Token {
        use Token::*;

        let start = self.index;

        while Self::valid_symbol_character(self.current_char()) {
            self.advance();
        }

        let symbol = self.chars[start..self.index].iter().collect::<String>();

        match symbol.as_str() {
            "import" => Kimport,
            "let" => Klet,
            "def" => Kdef,
            "in" => Kin,
            "match" => Kmatch,
            "true" => Ktrue,
            "false" => Kfalse,
            "and" => Kand,
            "or" => Kor,
            "return" => Kreturn,
            _ => Identifier(symbol),
        }
    }

    fn valid_symbol_character(ch: &char) -> bool {
        ch.is_alphanumeric() || ch == &'_'
    }
}
