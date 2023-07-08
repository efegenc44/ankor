#[derive(PartialEq, Clone)]
pub enum Token {
    Integer(String),
    Identifier(String),

    Plus,
    Minus,
    Star,
    Equal,
    LParen,
    RParen,
    Pipe,
    Comma,
    Semicolon,

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
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            Pipe => write!(f, "|"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            Klet => write!(f, "let"),
            Kdef => write!(f, "def"),
            Kin => write!(f, "in"),
            End => write!(f, "END"),
        }
    }
}
