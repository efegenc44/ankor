use crate::span::HasSpan;

#[derive(PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Integer(String),
    Float(String),
    String(String),

    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    DoubleEqual,
    BangEqual,
    Bang,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LParen,
    RParen,
    LSquare,
    RSquare,
    LCurly,
    RCurly,
    Pipe,
    Comma,
    Semicolon,
    Colon,
    FatArrow,
    Dot,

    Kimport,
    Klet,
    Kdef,
    Kin,
    Kmatch,
    Ktrue,
    Kfalse,
    Kand,
    Kor,
    Kmodule,
    Kwhile,
    Kfor,
    Kif,
    Kelse,
    Kdo,
    Kthen,
    Kreturn,
    Kbreak,
    Kcontinue,
    Ktry,
    Khandle,
    Kraise,
    Ko,

    End,
}

impl HasSpan for Token {}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;

        match self {
            Identifier(_) => write!(f, "Identifier"),
            Integer(int) => write!(f, "{int}"),
            Float(float) => write!(f, "{float}"),
            String(string) => write!(f, "\"{string}\""),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Equal => write!(f, "="),
            DoubleEqual => write!(f, "=="),
            BangEqual => write!(f, "!="),
            Bang => write!(f, "!"),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LSquare => write!(f, "["),
            RSquare => write!(f, "]"),
            LCurly => write!(f, "{{"),
            RCurly => write!(f, "}}"),
            Pipe => write!(f, "|"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            Colon => write!(f, ":"),
            FatArrow => write!(f, "=>"),
            Dot => write!(f, "."),
            Kimport => write!(f, "import"),
            Klet => write!(f, "let"),
            Kdef => write!(f, "def"),
            Kin => write!(f, "in"),
            Kmatch => write!(f, "match"),
            Ktrue => write!(f, "true"),
            Kfalse => write!(f, "false"),
            Kand => write!(f, "and"),
            Kor => write!(f, "or"),
            Kmodule => write!(f, "module"),
            Kwhile => write!(f, "while"),
            Kfor => write!(f, "for"),
            Kif => write!(f, "if"),
            Kelse => write!(f, "else"),
            Kdo => write!(f, "do"),
            Kthen => write!(f, "then"),
            Kreturn => write!(f, "return"),
            Kbreak => write!(f, "break"),
            Kcontinue => write!(f, "continue"),
            Ktry => write!(f, "try"),
            Khandle => write!(f, "handle"),
            Kraise => write!(f, "raise"),
            Ko => write!(f, "o"),
            End => write!(f, "END"),
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
