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
            todo!("Error handling")
        };
        (left, right)
    }};

    ($values:ident, $method:ident) => {{
        let [left, right] = $values else { 
            todo!("Error handling")
        };
        (left.$method(), right.$method())
    }};
}

macro_rules! one_value {
    ($values:ident) => {{
        let [left] = $values else { 
            todo!("Error handling")
        };
        left
    }};
}

macro_rules! unary {
    ($values:ident, $op:tt) => {{
        let [left] = $values else { 
            unreachable!() 
        };
        $op left
    }};
}

macro_rules! binary {
    ($values:ident, $op:tt) => {{
        let (left, right) = two_values!($values);
        left $op right
    }};
}

macro_rules! comparison {
    ($values:ident, $op:tt) => {{
        let (left, right) = two_values!($values);
        Value::Bool(left $op right)
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
            Value::Unit
        }

        "+" -> |values| binary!(values, +)
        "*" -> |values| binary!(values, *)
        "/" -> |values| binary!(values, /)
        "-" -> |values| match values {
            [operand] => !operand,
            [_, _]    => binary!(values, -),
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

            match one_value!(values) {
                Float(float) => Float(*float),
                Integer(int) => Float(*int as value::Float),
                // TODO: Proper float conversion
                BigInteger(bigint) => Float(bigint.to_string().parse().unwrap()),
                _ => todo!("Error handling")
            }
        }
    }
}

pub fn get_prelude() -> Module {
    Rc::new(RefCell::new(prelude()))
} 
