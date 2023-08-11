use crate::{token::Token, span::{Spanned, HasSpan, Span}, error::Error};

pub struct Lexer {
    chars: Vec<char>,
    index: usize,

    col: usize, 
    row: usize, 
}

type LexResult = Result<Spanned<Token>, Error>; 

impl Iterator for Lexer {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;

        let start_col = self.col;
        let start_line = self.row;

        if self.index >= self.chars.len() {
            return None;
        }

        let token = match self.current_char() {
            '+' => Plus,
            '-' => Minus,
            '*' => Star,
            '/' => Slash,
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
            #[allow(clippy::suspicious_else_formatting)]
            '<' => if self.peek_is('=') { LessEqual } else
                   if self.peek_is('-') { LeftArrow } else { Less },
            '>' => if self.peek_is('=') { GreaterEqual } else { Greater },
            ';' => Semicolon,
            ':' => Colon,
            '.' => Dot,
            '\0' => End,

            non_trivial => return match non_trivial {
                ' ' | '\t' | '\r' => {
                    self.advance();
                    self.next()
                }
                '\n' => {
                    self.advance();
                    self.row += 1;
                    self.col = 1;
                    self.next()
                }
                '#' => {
                    while self.current_char() != &'\n' && self.current_char() != &'\0' {
                        self.advance();
                    }
                    self.next()
                }
                '"' => {
                    Some(self.lex_string())
                }
                '0'..='9' => {
                    Some(self.lex_number())
                }
                ch if Self::valid_symbol_character(ch) => {
                    Some(Ok(self.lex_symbol()))
                }
                unknown => Some(Error::make(
                    format!("Unknown Start of a Token: {unknown}"),
                    Span::new(start_line, self.row, start_col, self.col + 1)
                ))
            }
        };
        self.advance();

        Some(Ok(token.with_span(Span::new(start_line, self.row, start_col, self.col))))
    }
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let chars = {
            let mut chars: Vec<_> = source.chars().collect();
            chars.push('\0');
            chars
        };
        Self { chars, index: 0, col: 1, row: 1 }
    }

    fn current_char(&self) -> &char {
        self.chars.get(self.index).unwrap()
    }

    fn advance(&mut self) {
        self.col += 1;
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

    fn lex_string(&mut self) -> LexResult {
        let start_col = self.col;
        let start_line = self.row;

        let mut string = String::new();

        self.advance();
        while self.current_char() != &'"' && self.current_char() != &'\0' {
            let ch = match self.current_char() {
                '\n' => {
                    self.col = 1;
                    self.row += 1;
                    '\n'
                }
                '\\' => {
                    self.advance();
                    match self.current_char() {
                        'n'  => '\n',
                        'r'  => '\r',
                        't'  => '\t',
                        '0'  => '\0',
                        '\\' => '\\',
                        '\"' => '\"',
                        // '\0' => todo!("Error handling")
                        unknown => return Error::make(
                            format!("Unknown Escape Sequence: {unknown}"), 
                            Span::new(self.row, self.row, self.col, self.col + 1)
                        )
                    }
                },
                ch => *ch
            };
            string.push(ch);
            self.advance();
        }

        if self.current_char() == &'\0' {
            return Error::make(
                "Unterminated String Literal",
                Span::new(start_line, self.row, start_col, self.col)
            )
        }

        self.advance();

        Ok(Token::String(string).with_span(Span::new(start_line, self.row, start_col, self.col)))
    }

    fn lex_number(&mut self) -> LexResult {
        let start_col = self.col;
        let start_line = self.row;

        let mut number = String::new();

        while let '0'..='9' | '_' = self.current_char() {
            if let digit @ '0'..='9' = self.current_char() {
                number.push(*digit);
            }
            self.advance();
        }

        let token = if self.current_char() == &'.' {
            self.advance();

            number.push('.');
            while let '0'..='9' | '_' = self.current_char() {
                if let digit @ '0'..='9' = self.current_char() {
                    number.push(*digit);
                }
                self.advance();
            }

            Token::Float
        } else {
            Token::Integer
        };

        if Self::valid_symbol_character(self.current_char()) {
            return Error::make(
                "Invalid Number Literal",
                Span::new(start_line, self.row, start_col, self.col + 1)
            )
        }

        Ok(token(number).with_span(Span::new(start_line, self.row, start_col, self.col)))
    }

    fn lex_symbol(&mut self) -> Spanned<Token> {
        use Token::*;

        let start = self.index;
        let start_col = self.col;
        let start_line = self.row;

        while Self::valid_symbol_character(self.current_char()) {
            self.advance();
        }

        let symbol = self.chars[start..self.index]
            .iter()
            .collect::<std::string::String>();

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
            "module" => Kmodule,
            "while" => Kwhile,
            "for" => Kfor,
            "if" => Kif,
            "else" => Kelse,
            "do" => Kdo,
            "then" => Kthen,
            "return" => Kreturn,
            "break" => Kbreak,
            "continue" => Kcontinue,
            "try" => Ktry,
            "handle" => Khandle,
            "raise" => Kraise,
            "o" => Ko,
            _ => Identifier(symbol),
        }
        .with_span(Span::new(start_line, self.row, start_col, self.col))
    }

    fn valid_symbol_character(ch: &char) -> bool {
        ch.is_alphanumeric() || ch == &'_'
    }
}
