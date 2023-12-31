use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::{value::{Value, self, Module}, value::{integer::{Integer, self}, range::Range, function::Function}};

macro_rules! env {
    ($( $name:literal -> $func:expr )*) => {
        HashMap::from([$((String::from($name), Value::Function(Function::Native($func)))),*])
    };
}

macro_rules! three_values {
    ($values:ident) => {{
        let [first, second, third] = $values else {
            return Err(format!("Expected `3` Arguments, Instead Found `{}`", $values.len()))
        };
        (first, second, third)
    }};

    ($values:ident, $method:ident) => {{
        let (first, second, third) = three_values!($values);
        (first.$method()?, second.$method()?, third.$method()?)
    }};
}

macro_rules! two_values {
    ($values:ident) => {{
        let [left, right] = $values else { 
            return Err(format!("Expected `2` Arguments, Instead Found `{}`", $values.len()))
        };
        (left, right)
    }};

    ($values:ident, $method:ident) => {{
        let (left, right) = two_values!($values);
        (left.$method()?, right.$method()?)
    }};
}

macro_rules! one_value {
    ($values:ident) => {{
        let [left] = $values else { 
            return Err(format!("Expected `1` Arguments, Instead Found `{}`", $values.len()))
        };
        left
    }};
}

macro_rules! unary {
    ($values:ident, $op:tt) => {{
        let operand = one_value!($values);
        $op operand
    }};
}

macro_rules! binary {
    ($values:ident, $op:tt) => {{
        let (left, right) = two_values!($values);
        left $op right
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
            None => Err(format!("Comparison of different types: {}, {}", left.type_name(), right.type_name()))
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
            let (left, right) = two_values!(values, as_function); 
            Ok(Value::Function(Function::Composed(Box::new(left.clone()), Box::new(right.clone()))))
        }

        "<-" -> |values| {
            let (left, right) = two_values!(values);
            Ok(Value::Function(Function::Partial(Box::new(left.as_function()?), Box::new(right.clone()))))
        }
        
        "+" -> |values| binary!(values, +)
        "*" -> |values| binary!(values, *)
        "/" -> |values| binary!(values, /)
        "-" -> |values| match values {
            [_]    => unary!(values, -),
            [_, _] => binary!(values, -),
            _ => Err("Expected at least one value, at max two values for (-)".to_string())
        }

        "and" -> |values| binary!(values, &)
        "or"  -> |values| binary!(values, |)

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
                Integer(int) => Float(match int {
                    integer::Integer::Small(small) => *small as value::Float,
                    // TODO: Proper float conversion
                    integer::Integer::Big(bigint) => bigint.to_string().parse().unwrap()
                }),
                value => return Err(format!("`{}` is not convertable to float", value.type_name()))
            })
        }
    
        "range" -> |values| {
            Ok(Value::Range(match values {
                [end] => Range {
                    start: Integer::Small(0),
                    end: end.as_integer()?,
                    step: Integer::Small(1)
                },
                [start, end] => Range {
                    start: start.as_integer()?,
                    end: end.as_integer()?,
                    step: Integer::Small(1)
                },
                [start, end, step] => Range {
                    start: start.as_integer()?,
                    end: end.as_integer()?,
                    step: step.as_integer()?
                },
                _ => return Err("At least 1 argument expected".to_string())
            }))
        }
    }
}

fn list() -> HashMap<String, Value> {
    env! {
        "get" -> |values| {
            let (list, index) = two_values!(values);

            let index = match index {
                Value::Integer(Integer::Small(int)) => *int,
                Value::Integer(Integer::Big(bigint)) => bigint.try_into().map_err(|_| "Cannot index using BigInteger".to_string())?,
                _ => return Err(format!("Index expected to be `Integer` found {}", index.type_name()))
            };

            match list.as_list()?.borrow().get(index as usize) {
                Some(value) => Ok(value.clone()),
                None => return Err("Index out of bounds".to_string()),
            }
        }

        "set" -> |values| {
            let (list, index, value) = three_values!(values);

            let index = match index {
                Value::Integer(Integer::Small(int)) => *int,
                Value::Integer(Integer::Big(bigint)) => bigint.try_into().map_err(|_| "Cannot index using BigInteger".to_string())?,
                _ => return Err(format!("Index expected to be `Integer` found {}", index.type_name()))
            };

            match list.as_list()?.borrow_mut().get_mut(index as usize) {
                Some(lvalue) => *lvalue = value.clone(),
                None => return Err("Index out of bounds".to_string()),
            };

            Ok(Value::Unit)
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

pub fn get_prelude(source: &str) -> Module {
    let mut prelude = prelude();
    
    prelude.insert(
        "List".to_string(), 
        Value::Module(Module {
            source: source.into(),
            map: Rc::new(RefCell::new(list()))
        })
    );

    prelude.insert(
        "String".to_string(), 
        Value::Module(Module {
            source: source.into(),
            map: Rc::new(RefCell::new(string()))
        })
    );

    Module {
        source: source.into(),
        map: Rc::new(RefCell::new(prelude))
    }
}
