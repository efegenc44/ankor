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
        (left.$method()?, right.$method()?)
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
        let operand = one_value!($values);
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
    ($values:ident, $res:pat) => {{
        use std::cmp::Ordering::*;

        let (left, right) = two_values!($values);

        match left.partial_cmp(right) {
            Some(result) => Ok(Value::Bool(match result {
                $res => true,
                _ => false
            })),
            None => Err("Comparison of different types".to_string())
        }
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

        "o" -> |values| {
            let (left, right) = two_values!(values); 
            Ok(Value::ComposedFunctions(Box::new(left.clone()), Box::new(right.clone())))
        }

        "<-" -> |values| {
            let (left, right) = two_values!(values); 
            Ok(Value::ParitalFunction(Box::new(left.clone()), Box::new(right.clone())))
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

        "<"  -> |values| comparison!(values, Less)
        "<=" -> |values| comparison!(values, Less | Equal)
        ">"  -> |values| comparison!(values, Greater)
        ">=" -> |values| comparison!(values, Greater | Equal)
        "==" -> |values| comparison!(values, Equal)
        "!=" -> |values| comparison!(values, Less | Greater)

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

fn list() -> HashMap<String, Value> {
    env! {
        "get" -> |values| {
            let (list, index) = two_values!(values);
            let list = list.as_list()?;
            match index {
                Value::Integer(int) => match list.borrow().get(*int as usize) {
                    Some(value) => Ok(value.clone()),
                    None => Err("Index out of bounds".to_string()),
                },
                // TODO: Big Integer list indexing
                Value::BigInteger(_) => Err("Cannot index using BigInteger".to_string()),
                _ => Err("Index has to be Integer".to_string())
            }
        }

        "push" -> |values| {
            let [list, args @ ..] = values else {
                return Err("Expected at least List".to_string())
            };

            let list = list.as_list()?;
            list.borrow_mut().extend_from_slice(args);

            Ok(Value::Unit)
        }

        "append" -> |values| {
            let [list, args @ ..] = values else {
                return Err("Expected at least List".to_string())
            };

            let list = list.as_list()?;
            list.borrow_mut().extend_from_slice(args);

            Ok(Value::List(list))
        }

        "pop" -> |values| {
            let list = one_value!(values).as_list()?;
            let mut list = list.borrow_mut();
            list.pop().ok_or("List is empty, nothing to pop".to_string())
        }
    }
}

fn string() -> HashMap<String, Value> {
    env! {
        "concat" -> |values| {
            let mut string = String::new();
            for value in values {
                string += value.as_str()?;
            }
            Ok(Value::String(string))
        }
    }
}

pub fn get_prelude(source: &str) -> ModuleValue {
    let mut prelude = prelude();
    
    prelude.insert(
        "List".to_string(), 
        Value::Module(ModuleValue {
            source: source.into(),
            map: Rc::new(RefCell::new(list()))
        })
    );

    prelude.insert(
        "String".to_string(), 
        Value::Module(ModuleValue {
            source: source.into(),
            map: Rc::new(RefCell::new(string()))
        })
    );

    ModuleValue {
        source: source.into(),
        map: Rc::new(RefCell::new(prelude))
    }
}
