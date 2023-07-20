use super::{integer::Integer, Value};

#[derive(Clone, PartialEq, Eq)]
pub struct Range {
    pub start: Integer,
    pub end: Integer,
    pub step: Integer
}

impl Iterator for Range {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None
        }

        let value = self.start.clone();
        self.start = &self.start + &self.step;

        Some(Value::Integer(value))
    }
}

impl std::fmt::Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<range from: {}, to: {}, by: {}>", self.start, self.end, self.step)
    }
} 


