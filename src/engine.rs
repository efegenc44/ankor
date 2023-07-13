use std::{rc::Rc, cell::RefCell};

use crate::{
    expr::{
        AccessExpr, ApplicationExpr, Expr, FunctionExpr, ImportExpr,
        LetExpr, ListExpr, MatchExpr, Pattern, SequenceExpr, StructureExpr, AssignmentExpr, ListPattern, StructurePattern, ReturnExpr, ModuleExpr,
    },
    lexer::Lexer, parser::Parser,
    prelude::get_prelude,
    value::{FunctionValue, Module, Value},
};

pub struct Engine {
    locals: Vec<(String, Value)>,

    return_exception: Option<Value>,
}

impl Engine {
    pub fn new() -> Self {
        Self { locals: vec![], return_exception: None }
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
        if let Some((_, value)) = self.locals.iter().rev().find(|bind| &bind.0 == ident) {
            return value.clone();
        }

        match module.borrow().get(ident) {
            Some(value) => value.clone(),
            None => todo!("Error handling"),
        } 
    }

    fn assign(&mut self, ident: &str, value: Value, module: &Module) {
        if let Some((_, target)) = self.locals.iter_mut().rev().find(|bind| &bind.0 == ident) {
            return *target = value;
        }

        match module.borrow_mut().get_mut(ident) {
            Some(target) => *target = value,
            None => todo!("Error handling"),
        } 
    }

    pub fn evaluate(&mut self, expr: &Expr, module: &Module) -> Value {
        use Expr::*;

        if self.return_exception.is_some() {
            return Value::Unit
        }
        
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
            Structure(structure_expr) => self.evaluate_structure_expr(structure_expr, module),
            Assignment(assignment_expr) => self.evaluate_assignment_expr(assignment_expr, module),
            Return(return_expr) => self.evaluate_return_expr(return_expr, module),
            Module(module_expr) => Self::evaluate_module_expr(module_expr),
        }
    }

    fn evaluate_let_expr(&mut self, let_expr: &LetExpr, module: &Module) -> Value {
        let LetExpr { patt, vexp, expr } = let_expr;

        let value = self.evaluate(vexp, module);
        let mut local_count = 0;
        if !self.fits_pattern(&value, patt, &mut local_count) {
            todo!("Error handling")
        };
        let result = self.evaluate(expr, module);
        self.remove_local(local_count);
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

                let mut local_count = 0;
                if !std::iter::zip(func.args.clone(), arg_values)
                    .all(|(arg, value)| self.fits_pattern(&value, &arg, &mut local_count)) {
                        todo!("Error handling")
                }

                let result = self.evaluate(&func.expr, &func.modl);
                self.remove_local(args.len() + clos_count);

                let result = self.return_exception.clone().unwrap_or(result);
                self.return_exception = None;
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
            let mut local_count = 0;
            if self.fits_pattern(&value, pattern, &mut local_count) {
                let result = self.evaluate(expr, module);
                self.remove_local(local_count);
                return result
            }
            self.remove_local(local_count);
        }

        todo!("Error handling")
    }

    // TODO: Maybe pass value as value rather than reference
    fn fits_pattern(&mut self, value: &Value, pattern: &Pattern, local_count: &mut usize) -> bool {
        match (value, pattern) {
            (Value::Integer(lint), Pattern::NonNegativeInteger(rint)) => lint == &rint.parse::<isize>().unwrap(),
            (Value::Integer(lint), Pattern::NegativeInteger(rint)) => lint == &-rint.parse::<isize>().unwrap(),
            (Value::Bool(lbool), Pattern::Bool(rbool)) => lbool == rbool,
            (Value::Unit, Pattern::Unit) => true,
            (Value::List(list), Pattern::List(ListPattern { before_rest, after_rest, rest })) => {
                match rest {
                    Some(name) => {
                        let result = before_rest.len() + after_rest.len() <= list.len() &&
                            std::iter::zip(list.iter(), before_rest)
                                .all(|(value, pattern)| self.fits_pattern(value, pattern, local_count)) &&
                            std::iter::zip(list.iter().rev(), after_rest.iter().rev())
                                .all(|(value, pattern)| self.fits_pattern(value, pattern, local_count));
                    
                        if let Some(name) = name {
                            let rest = &list[before_rest.len()..list.len() - after_rest.len()];
                            self.define_local(name.clone(), Value::List(Rc::new(rest.to_vec())));
                            *local_count += 1;
                        };

                        result
                    },
                    None => list.len() == before_rest.len() &&
                            std::iter::zip(list.iter(), before_rest)
                                .all(|(value, pattern)| self.fits_pattern(value, pattern, local_count))
                }
            }
            (Value::Structure(structure), Pattern::Structure(StructurePattern { fields, rest })) => {
                let structure = structure.borrow();

                (match rest {
                    Some(_) => fields.len() <= structure.len(),
                    _ => fields.len() == structure.len()
                }) && if let Some(Some(name)) = rest {
                    let mut structure = (*structure).clone();
                    let fits = fields.iter().all(|(field_name, pattern)| {
                        structure.remove(field_name).is_some_and(|value| {
                            if let Some(pattern) = pattern {
                                self.fits_pattern(&value, pattern, local_count)
                            } else {
                                self.define_local(field_name.clone(), value);
                                *local_count += 1;
                                true
                            }
                        })
                    });

                    if fits {
                        self.define_local(
                            name.clone(), 
                            Value::Structure(Rc::new(RefCell::new(structure)))
                        );
                        *local_count += 1;
                    }

                    fits
                } else {
                    fields.iter().all(|(field_name, pattern)| {
                        structure.get(field_name).is_some_and(|value| { 
                            if let Some(pattern) = pattern {
                                self.fits_pattern(value, pattern, local_count)
                            } else {
                                self.define_local(field_name.clone(), value.clone());
                                *local_count += 1;
                                true
                            }
                        })
                    })
                }
            }
            (_, Pattern::Identifier(ident)) => {
                self.define_local(ident.clone(), value.clone());
                *local_count += 1;
                true
            },
            _ => false
        }
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

        let (Value::Module(map) | Value::Structure(map)) = self.evaluate(expr, module) else {
            todo!("Error handling")
        };
        
        let map = map.borrow();
        match map.get(name) {
            Some(value) => value.clone(),
            None => todo!("Error handling"),
        }
    }

    fn evaluate_structure_expr(&mut self, structure_expr: &StructureExpr, module: &Module) -> Value {
        let StructureExpr { fields } = structure_expr;

        let structure = fields
            .iter()
            .map(|(field_name, expr)| (field_name.clone(), self.evaluate(expr, module))).collect();

        Value::Structure(Rc::new(RefCell::new(structure)))
    }


    fn evaluate_assignment_expr(&mut self, assignment_expr: &AssignmentExpr, module: &Module) -> Value {
        let AssignmentExpr { lhs, rhs } = assignment_expr;

        let rvalue = self.evaluate(rhs, module);


        match &**lhs {
            Expr::Identifier(ident) => self.assign(ident, rvalue, module),
            Expr::Access(AccessExpr { expr, name }) => {
                let (Value::Module(map) | Value::Structure(map)) = self.evaluate(expr, module) else {
                    todo!("Error handling")
                }; 

                let mut map = map.borrow_mut();
                match map.get_mut(name) {
                    Some(target) => *target = rvalue,
                    None => todo!("Error handling"),
                }
            }
            _ => todo!("Error handling")
        }
    
        Value::Unit
    }

    fn evaluate_return_expr(&mut self, return_expr: &ReturnExpr, module: &Module) -> Value {
        let ReturnExpr { expr } = return_expr;

        let value = self.evaluate(expr, module);
        self.return_exception = Some(value);

        Value::Unit
    }

    fn evaluate_module_expr(module_expr: &ModuleExpr) -> Value {
        let ModuleExpr { definitions } = module_expr;
        
        let module = Self::evaluate_module(definitions);

        Value::Module(module)
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
