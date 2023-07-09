use std::collections::HashMap;

use crate::value::Value;

macro_rules! env {
    ($( $name:literal -> $func:expr )*) => {
        HashMap::from([$((String::from($name), Value::Native($func))),*])           
    };
}

macro_rules! two_integers {
    ($values:ident) => {{
        let [left, right] = $values else { 
            unreachable!() 
        };
        (left.to_integer(), right.to_integer())
    }};
}

macro_rules! two_values {
    ($values:ident) => {{
        let [left, right] = $values else { 
            unreachable!() 
        };
        (left, right)
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

pub fn get_global() -> HashMap<String, Value> {
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

        "+" ->  |values| {
            let (left, right) = two_integers!(values);
            Value::Integer(left + right)
        }
        
        "-" ->  |values| {
            Value::Integer(match values {
                [operand] => -operand.to_integer(),
                [left, right] => left.to_integer() - right.to_integer(),
                _ => unreachable!()
            })
        }

        "*" ->  |values| {
            let (left, right) = two_integers!(values);
            Value::Integer(left * right)
        }

        "<" ->  |values| {
            let (left, right) = two_integers!(values);
            Value::Bool(left < right)
        }

        "<=" ->  |values| {
            let (left, right) = two_integers!(values);
            Value::Bool(left <= right)
        }

        ">" ->  |values| {
            let (left, right) = two_integers!(values);
            Value::Bool(left > right)
        }

        ">=" ->  |values| {
            let (left, right) = two_integers!(values);
            Value::Bool(left >= right)
        }

        "==" ->  |values| {
            let (left, right) = two_values!(values);
            Value::Bool(left == right)
        }

        "!=" ->  |values| {
            let (left, right) = two_values!(values);
            Value::Bool(left != right)
        }

        "!" ->  |values| {
            let operand = one_value!(values).to_bool();
            Value::Bool(!operand)
        }
    }
}
