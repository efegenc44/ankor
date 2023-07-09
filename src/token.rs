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
    LSquare,
    RSquare,
    Pipe,
    Comma,
    Semicolon,
    FatArrow,
    Dot,

    Kimport,
    Klet,
    Kdef,
    Kin,
    Kmatch,
    Ktrue,
    Kfalse,

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
            LSquare => write!(f, "["),
            RSquare => write!(f, "]"),
            Pipe => write!(f, "|"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            FatArrow => write!(f, "=>"),
            Dot => write!(f, "."),
            Kimport => write!(f, "import"),
            Klet => write!(f, "let"),
            Kdef => write!(f, "def"),
            Kin => write!(f, "in"),
            Kmatch => write!(f, "match"),
            Ktrue => write!(f, "true"),
            Kfalse => write!(f, "false"),
            End => write!(f, "END"),
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
