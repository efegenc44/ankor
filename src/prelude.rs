use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::value::{Value, Module, self};

macro_rules! env {
    ($( $name:literal -> $func:expr )*) => {
        HashMap::from([$((String::from($name), Value::Native($func))),*])           
    };
}

macro_rules! two_values {
    ($values:ident) => {{
        let [left, right] = $values else { 
            unreachable!() 
        };
        (left, right)
    }};

    ($values:ident, $method:ident) => {{
        let [left, right] = $values else { 
            unreachable!() 
        };
        (left.$method(), right.$method())
    }};
}

macro_rules! one_value {
    ($values:ident) => {{
        let [left] = $values else { 
            unreachable!() 
        };
        left
    }};
}

macro_rules! arithmetic {
    ($values:ident, $op:tt) => {
        match two_values!($values) {
            (Value::Integer(lint), Value::Integer(rint)) => Value::Integer(lint $op rint),
            (Value::Float(lfloat), Value::Float(rfloat)) => Value::Float(lfloat $op rfloat),
            _ => todo!("Error handling")
        }
    };
}

macro_rules! comparison {
    ($values:ident, $op:tt) => {
        match two_values!($values) {
            (Value::Integer(lint), Value::Integer(rint)) => Value::Bool(lint $op rint),
            (Value::Float(lfloat), Value::Float(rfloat)) => Value::Bool(lfloat $op rfloat),
            _ => todo!("Error handling")
        }
    };
}

fn prelude() -> HashMap<String, Value> {
    env! {
        "println" -> |values| {
            match values {
                [] => (),
                [x, xs @ ..] => {
                    print!("{x}");
                    for value in xs {
                        print!(", {value}")
                    }
                }
            }
            println!();
            Value::Unit
        }

        "+" -> |values| arithmetic!(values, +)
        "*" -> |values| arithmetic!(values, *)
        "/" -> |values| arithmetic!(values, /)
        "-" -> |values| {
            use Value::*;

            match values {
                [operand] => match operand {
                    Integer(int) => Integer(-int),
                    Float(float) => Float(-float),
                    _ => todo!("Error handling")
                },
                [_, _] => arithmetic!(values, -),
                _ => unreachable!()
            }
        }

        "<"  -> |values| comparison!(values, <)
        "<=" -> |values| comparison!(values, <=)
        ">"  -> |values| comparison!(values, >)
        ">=" -> |values| comparison!(values, >=)

        "==" -> |values| {
            let (left, right) = two_values!(values);
            Value::Bool(left == right)
        }

        "!=" -> |values| {
            let (left, right) = two_values!(values);
            Value::Bool(left != right)
        }

        "!" -> |values| {
            let operand = one_value!(values).to_bool();
            Value::Bool(!operand)
        }

        "and" -> |values| {
            let (left, right) = two_values!(values, to_bool);
            Value::Bool(left && right)
        }

        "or" -> |values| {
            let (left, right) = two_values!(values, to_bool);
            Value::Bool(left || right)
        }

        "float" -> |values| {
            use Value::*;

            match one_value!(values) {
                Float(float) => Float(*float),
                Integer(int) => Float(*int as value::Float),
                _ => todo!("Error handling")
            }
        }
    }
}

pub fn get_prelude() -> Module {
    Rc::new(RefCell::new(prelude()))
} 
