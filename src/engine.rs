use std::rc::Rc;

use crate::{
    expr::{
        AccessExpr, ApplicationExpr, Expr, FunctionExpr, ImportExpr,
        LetExpr, ListExpr, MatchExpr, Pattern, SequenceExpr,
    },
    lexer::Lexer, parser::Parser,
    prelude::get_prelude,
    value::{FunctionValue, Module, Value},
};

pub struct Engine {
    locals: Vec<(String, Value)>,
}

impl Engine {
    pub fn new() -> Self {
        Self { locals: vec![] }
    }

    fn define_local(&mut self, name: String, value: Value) {
        self.locals.push((name, value));
    }

    fn remove_local(&mut self, n: usize) {
        for _ in 0..n {
            self.locals.pop().unwrap();
        }
    }

    fn resolve_identifier(&self, ident: &str, module: &Module) -> Value {
        for (name, value) in self.locals.iter().rev() {
            if name == ident {
                return value.clone();
            }
        }

        match module.borrow().get(ident) {
            Some(value) => value.clone(),
            None => todo!("Error handling"),
        } 
    }

    pub fn evaluate(&mut self, expr: &Expr, module: &Module) -> Value {
        use Expr::*;

        match expr {
            Integer(int) => Value::Integer(int.parse().unwrap()),
            Bool(bool) => Value::Bool(*bool),
            UnitValue => Value::Unit,
            Identifier(ident) => self.resolve_identifier(ident, module),
            Let(let_expr) => self.evaluate_let_expr(let_expr, module),
            Function(function_expr) => self.evaluate_function_expr(function_expr, module),
            Application(application_expr) => self.evaluate_application_expr(application_expr, module),
            Sequence(sequnce_expr) => self.evaluate_sequence_expr(sequnce_expr, module),
            Match(match_expr) => self.evaluate_match_expr(match_expr, module),
            Import(import_expr) => self.evaluate_import_expr(import_expr),
            Access(access_expr) => self.evaluate_access_expr(access_expr, module),
            List(list_expr) => self.evaluate_list_expr(list_expr, module),
        }
    }

    fn evaluate_let_expr(&mut self, let_expr: &LetExpr, module: &Module) -> Value {
        let LetExpr { name, vexp, expr } = let_expr;

        let value = self.evaluate(vexp, module);
        self.define_local(name.clone(), value);
        let result = self.evaluate(expr, module);
        self.remove_local(1);
        result
    }

    fn evaluate_function_expr(&mut self, function_expr: &FunctionExpr, module: &Module) -> Value {
        let function_value = FunctionValue {
            args: function_expr.args.clone(),
            expr: function_expr.expr.clone(),
            clos: function_expr.clos
                .as_ref()
                .map(|clos| clos
                    .iter()
                    .map(|ident| (ident.clone(), self.resolve_identifier(ident, module))).collect()),
            modl: module.clone()
        };
        
        Value::Function(Rc::new(function_value))
    }

    fn evaluate_application_expr(&mut self, application_expr: &ApplicationExpr, module: &Module) -> Value {
        let ApplicationExpr { func, args } = application_expr;

        let arg_values: Vec<_> = args.iter().map(|expr| self.evaluate(expr, module)).collect();

        match self.evaluate(func, module) {
            Value::Native(func) => func(&arg_values),
            Value::Function(func) => {
                if args.len() != func.args.len() {
                    todo!("Error handling")
                }

                let clos_count = (&func.clos)
                    .as_ref()
                    .map(|clos| {
                        for (arg, value) in clos {
                            self.define_local(arg.clone(), value.clone());
                        }
                        clos.len()
                    })
                    .unwrap_or(0);

                for (arg, value) in std::iter::zip(func.args.clone(), arg_values) {
                    self.define_local(arg, value);
                }

                let result = self.evaluate(&func.expr, &func.modl);
                self.remove_local(args.len() + clos_count);
                result
            },
            _ => todo!("Error handling")
        }
    }

    fn evaluate_sequence_expr(&mut self, sequnce_expr: &SequenceExpr, module: &Module) -> Value {
        let SequenceExpr { lhs, rhs } = sequnce_expr;

        self.evaluate(lhs, module);
        self.evaluate(rhs, module)
    }

    fn evaluate_match_expr(&mut self, match_expr: &MatchExpr, module: &Module) -> Value {
        let MatchExpr { expr, arms } = match_expr;

        let value = self.evaluate(expr, module);
        for (pattern, expr) in arms {
            let (fits, local_count) = self.fits_pattern(&value, pattern);
            if fits {
                let result = self.evaluate(expr, module);
                self.remove_local(local_count);
                return result
            }
            self.remove_local(local_count);
        }

        todo!("Error handling")
    }

    fn fits_pattern(&mut self, value: &Value, pattern: &Pattern) -> (bool, usize) {
        let mut local_count = 0;

        (match (value, pattern) {
            (Value::Integer(lint), Pattern::NonNegativeInteger(rint)) => lint == &rint.parse::<isize>().unwrap(),
            (Value::Integer(lint), Pattern::NegativeInteger(rint)) => lint == &-rint.parse::<isize>().unwrap(),
            (Value::Bool(lbool), Pattern::Bool(rbool)) => lbool == rbool,
            (Value::Unit, Pattern::Unit) => true,
            (Value::List(list), Pattern::List(patterns)) => {
                // Definitely, there is a better way to do it
                if patterns.is_empty() && !list.is_empty() {
                    return (false, local_count)
                }

                for (index, pattern) in patterns.iter().enumerate() {
                    if let Pattern::Rest(name) = pattern {
                        let remaining_pattern_len = patterns[index+1..].len();
                        let remaining_value_len = list[index..].len();
                        
                        if remaining_pattern_len > remaining_value_len {
                            return (false, local_count)
                        }                        
                        
                        if let Some(name) = name {
                            let rest = list[index..list.len() - remaining_pattern_len].to_vec();
                            self.define_local(name.clone(), Value::List(Rc::new(rest)));
                            local_count += 1;
                        }

                        for i in 0..remaining_pattern_len {
                            let value = list.get(list.len() - 1 - i).unwrap();
                            let pattern = patterns.get(patterns.len() - 1 - i).unwrap();
                        
                            if let Pattern::Rest(_) = pattern {
                                todo!("Error handling")
                            } 

                            if let (false, local_count) = self.fits_pattern(value, pattern) {
                                return (false, local_count)
                            }
                        }
                        break
                    }

                    let value = match list.get(index) {
                        Some(value) => value,
                        None => return (false, local_count),
                    };
                    if let (false, local_count) = self.fits_pattern(value, pattern) {
                        return (false, local_count)
                    }
                }

                true
            }
            (_, Pattern::Identifier(ident)) => {
                self.define_local(ident.clone(), value.clone());
                local_count += 1;
                true
            },
            _ => false
        }, local_count)
    } 

    fn evaluate_import_expr(&mut self, import_expr: &ImportExpr) -> Value {
        let ImportExpr { parts } = import_expr;
    
        let file_path = parts.join("/") + ".ank";
        let file = std::fs::read_to_string(file_path).expect("Error handling");
        let tokens = Lexer::new(&file).collect();
        let astree = Parser::new(tokens).parse_module();
        Value::Module(Self::evaluate_module(&astree))
    }

    fn evaluate_list_expr(&mut self, list_expr: &ListExpr, module: &Module) -> Value {
        let ListExpr { exprs } = list_expr;

        Value::List(Rc::new(exprs.iter().map(|expr| self.evaluate(expr, module)).collect()))
    }

    fn evaluate_access_expr(&mut self, access_expr: &AccessExpr, module: &Module) -> Value {
        let AccessExpr { expr, name } = access_expr;

        let Value::Module(module) = self.evaluate(expr, module) else {
            todo!("Error handling")
        };

        let module = module.borrow();
        match module.get(name) {
            Some(value) => value.clone(),
            None => todo!("Error handling"),
        }   
    }

    pub fn evaluate_module(definitions: &Vec<(String, Expr)>) -> Module {
        let mut engine = Self::new();
        let module = get_prelude();
        for (name, expr) in definitions {
            let value = engine.evaluate(expr, &module);
            module.borrow_mut().insert(name.clone(), value);
        }

        module
    }

    pub fn run_from_entry(definitions: &Vec<(String, Expr)>) -> Value {
        let module = Self::evaluate_module(definitions);
        let main = module.borrow_mut().remove("main").expect("Error handling");
        let Value::Function(func) = main else {
            todo!("Error handling");
        };

        // Pass cli args if 1 arg provided
        Self::new().evaluate(&func.expr, &module)
    }
}
