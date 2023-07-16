#[derive(Clone, Copy)]
pub struct Span {
    pub line_start: usize,
    pub line_end: usize,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(line_start: usize, line_end: usize, start: usize, end: usize) -> Self {
        Span { line_start, line_end, start, end }
    }
}

#[derive(Clone, Copy)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span
}

pub trait HasSpan 
where 
    Self: Sized
{
    fn with_span(self, span: Span) -> Spanned<Self> {
        Spanned { data: self, span }
    }

    fn start_end(self, start: Span, end: Span) -> Spanned<Self> {
        Spanned { 
            data: self, 
            span: Span { 
                line_start: start.line_start, 
                line_end: end.line_end, 
                start: start.start, 
                end: end.end 
            } 
        }
    }
}