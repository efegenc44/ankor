use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::value::{Value, self, ModuleValue};

macro_rules! env {
    ($( $name:literal -> $func:expr )*) => {
        HashMap::from([$((String::from($name), Value::Native($func))),*])           
    };
}

macro_rules! two_values {
    ($values:ident) => {{
        let [left, right] = $values else { 
            return Err("Expected two value".to_string())
        };
        (left, right)
    }};

    ($values:ident, $method:ident) => {{
        let [left, right] = $values else { 
            return Err("Expected two value".to_string())
        };
        (left.$method(), right.$method())
    }};
}

macro_rules! one_value {
    ($values:ident) => {{
        let [left] = $values else { 
            return Err("Expected one value".to_string())
        };
        left
    }};
}

macro_rules! unary {
    ($values:ident, $op:tt) => {{
        let [operand] = $values else { 
            unreachable!() 
        };
        match ($op operand) {
            Some(value) => Ok(value),
            None => Err("Type Error at Unary Operation".to_string())
        }    
    }};
}

macro_rules! binary {
    ($values:ident, $op:tt) => {{
        let (left, right) = two_values!($values);
        match (left $op right) {
            Some(value) => Ok(value),
            None => Err("Type Error at Binary Operation".to_string())
        }
    }};
}

macro_rules! comparison {
    ($values:ident, $op:tt) => {{
        let (left, right) = two_values!($values);
        Ok(Value::Bool(left $op right))
    }};
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
            Ok(Value::Unit)
        }

        "+" -> |values| binary!(values, +)
        "*" -> |values| binary!(values, *)
        "/" -> |values| binary!(values, /)
        "-" -> |values| match values {
            [_]    => unary!(values, !),
            [_, _] => binary!(values, -),
            _ => unreachable!()
        }

        "and" -> |values| binary!(values, &)
        "or"  -> |values| binary!(values, -)

        "!" -> |values| unary!(values, !)

        "<"  -> |values| comparison!(values, <)
        "<=" -> |values| comparison!(values, <=)
        ">"  -> |values| comparison!(values, >)
        ">=" -> |values| comparison!(values, >=)
        "==" -> |values| comparison!(values, ==)
        "!=" -> |values| comparison!(values, ==)

        "float" -> |values| {
            use Value::*;

            Ok(match one_value!(values) {
                Float(float) => Float(*float),
                Integer(int) => Float(*int as value::Float),
                // TODO: Proper float conversion
                BigInteger(bigint) => Float(bigint.to_string().parse().unwrap()),
                _ => return Err("Value is not convertable to float".to_string())
            })
        }
    }
}

pub fn get_prelude(source: &str) -> ModuleValue {
    ModuleValue {
        source: source.into(),
        map: Rc::new(RefCell::new(prelude()))
    }
}
