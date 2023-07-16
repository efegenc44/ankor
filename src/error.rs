use crate::span::Span;

pub struct Error {
    pub msg: String,
    pub span: Span,
    pub origin: Option<(Box<Error>, String)>
}

impl Error {
    pub fn new<S>(msg: S, span: Span, origin: Option<(Box<Error>, String)>) -> Error 
    where
        S: Into<String>
    {
        Error { msg: msg.into(), span, origin }
    }

    pub fn make<S, T>(msg: S, span: Span) -> Result<T, Error> 
    where
        S: Into<String>
    {
        Err(Error { msg: msg.into(), span, origin: None })
    }
}