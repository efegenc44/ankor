#[derive(PartialEq, Clone)]
pub enum Token {
    Integer(String),
    Identifier(String),

    Plus,
    Minus,
    Star,
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
    Pipe,
    Comma,
    Semicolon,
    FatArrow,

    Klet,
    Kdef,
    Kin,

    End,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;

        match self {
            Integer(int) => write!(f, "{int}"),
            Identifier(ident) => write!(f, "{ident}"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
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
            Pipe => write!(f, "|"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            FatArrow => write!(f, "=>"),
            Klet => write!(f, "let"),
            Kdef => write!(f, "def"),
            Kin => write!(f, "in"),
            End => write!(f, "END"),
        }
    }
}
