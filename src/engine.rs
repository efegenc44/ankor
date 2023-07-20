use std::{rc::Rc, cell::RefCell, collections::HashMap};

use apnum::BigInt;

use crate::{
    expr::{
        AccessExpr, ApplicationExpr, Expr, FunctionExpr, ImportExpr,
        LetExpr, ListExpr, MatchExpr, Pattern, SequenceExpr, StructureExpr, 
        AssignmentExpr, ListPattern, StructurePattern, ReturnExpr, ModuleExpr, 
        WhileExpr, BreakExpr, ForExpr, IfExpr, OrPattern, RaiseExpr, TryHandleExpr,
    },
    lexer::Lexer, parser::Parser,
    prelude::get_prelude,
    value::{Value, self, Module, integer::Integer, function::{Function, FunctionValue}}, span::{Spanned, Span}, handle_error, reporter::Reporter, error::Error,
};

pub enum Exception {
    Return(Value),
    Break(Value),
    Continue,
    Exception((Error, String))
}

type EvaluationResult = Result<Value, Exception>;  

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

    fn error<S>(&mut self, msg: S, span: Span, source: String) -> EvaluationResult 
    where
        S: Into<String> 
    {
        Err(Exception::Exception((Error::new(msg, span, None), source)))
    }

    fn call_error<S>(&mut self, msg: S, span: Span, call_site: Span, call_site_source: String, source: String) -> EvaluationResult 
    where
        S: Into<String> 
    {
        Err(Exception::Exception((
            Error::new(msg, span, Some((Box::new(Error::new("Call Site", call_site, None)), call_site_source))), 
            source
        )))
    }

    fn resolve_identifier(&mut self, ident: &str, module: &Module, span: Span) -> EvaluationResult {
        if let Some((_, value)) = self.locals.iter().rev().find(|bind| bind.0 == ident) {
            return Ok(value.clone());
        }

        match module.map.borrow().get(ident) {
            Some(value) => Ok(value.clone()),
            None => self.error(format!("Undefined Symbol `{ident}`"), span, module.source.clone())
        }
    }

    fn assign(&mut self, ident: &str, value: Value, module: &Module, span: Span) -> EvaluationResult {
        if let Some((_, target)) = self.locals.iter_mut().rev().find(|bind| bind.0 == ident) {
            *target = value;
            return Ok(Value::Unit)
        }

        match module.map.borrow_mut().get_mut(ident) {
            Some(target) => {
                *target = value;
                return Ok(Value::Unit)
            },
            None => self.error(format!("Undefined Symbol `{ident}`"), span, module.source.clone())
        } 
    }

    fn parse_integer(number: &str) -> Integer {
        match number.parse() {
            Ok(number) => Integer::Small(number),
            Err(_) => Integer::Big(BigInt::try_from(number).unwrap()),
        }
    }

    pub fn evaluate(&mut self, expr: &Spanned<Expr>, module: &Module) -> EvaluationResult {
        use Expr::*;
        
        match &expr.data {
            Integer(int) => Ok(Value::Integer(Self::parse_integer(int))),
            Float(float) => Ok(Value::Float(float.parse().unwrap())),
            String(string) => Ok(Value::String(string.clone())),
            Bool(bool) => Ok(Value::Bool(*bool)),
            UnitValue => Ok(Value::Unit),
            Identifier(ident) => self.resolve_identifier(ident, module, expr.span),
            Let(let_expr) => self.evaluate_let_expr(let_expr, module),
            Function(function_expr) => self.evaluate_function_expr(function_expr, module),
            Application(application_expr) => self.evaluate_application_expr(application_expr, module, expr.span),
            Sequence(sequnce_expr) => self.evaluate_sequence_expr(sequnce_expr, module),
            Match(match_expr) => self.evaluate_match_expr(match_expr, module, expr.span),
            Import(import_expr) => self.evaluate_import_expr(import_expr, module, expr.span),
            Access(access_expr) => self.evaluate_access_expr(access_expr, module),
            List(list_expr) => self.evaluate_list_expr(list_expr, module),
            Structure(structure_expr) => self.evaluate_structure_expr(structure_expr, module),
            Assignment(assignment_expr) => self.evaluate_assignment_expr(assignment_expr, module),
            While(while_expr) => self.evaluate_while_expr(while_expr, module),
            For(for_expr) => self.evaluate_for_expr(for_expr, module),
            If(if_expr) => self.evaluate_if_expr(if_expr, module),
            Return(return_expr) => self.evaluate_return_expr(return_expr, module),
            Break(break_expr) => self.evaluate_break_expr(break_expr, module),
            Continue => self.evaluate_continue_expr(),
            Raise(raise_expr) => self.evaluate_raise_expr(raise_expr, module, expr.span),
            TryHandle(tryhandle_expr) => self.evaluate_tryhandle_expr(tryhandle_expr, module),
            Module(module_expr) => self.evaluate_module_expr(module_expr, module),
        }
    }

    fn evaluate_let_expr(&mut self, let_expr: &LetExpr, module: &Module) -> EvaluationResult {
        let LetExpr { patt, vexp, expr } = let_expr;

        let value = self.evaluate(vexp, module)?;
        let mut local_count = 0;
        if !self.fits_pattern(&value, patt, &mut local_count) {
            self.remove_local(local_count);
            return self.error("Pattern Couldn't Be Matched at Let Expression", patt.span, module.source.clone())
        }
        let result = self.evaluate(expr, module);
        self.remove_local(local_count);
        Ok(result?)
    }

    fn evaluate_function_expr(&mut self, function_expr: &FunctionExpr, module: &Module) -> EvaluationResult {
        let clos = if let Some(caps) = &function_expr.clos {
            let mut clos = vec![];
            for ident in caps {
                clos.push((ident.data.clone(), self.resolve_identifier(&ident.data, module, ident.span)?));
            }
            Some(clos)
        } else {
            None
        };
        

        let function_value = FunctionValue {
            args: function_expr.args.clone(),
            expr: function_expr.expr.clone(),
            modl: module.clone(),
            clos,
        };
        
        Ok(Value::Function(Function::Standart(Rc::new(function_value))))
    }

    fn call_function(&mut self, func_value: &Function, args: Vec<Value>, module: &Module, span: Span, func_span: Span) -> EvaluationResult {
        match func_value {
            Function::Native(func) => match func(&args) {
                Ok(value) => Ok(value),
                Err(err) => self.error(err, span, module.source.clone()),
            },
            Function::Composed(left, right) => {
                let first_pass = self.call_function(right, args, module, span, func_span).map_err(|mut exc| {
                    if let Exception::Exception((err, _)) = &mut exc {
                        err.msg = format!("At right function: {}", err.msg);
                    }
                    exc
                })?;
                self.call_function(left, vec![first_pass], module, span, func_span).map_err(|mut exc| {
                    if let Exception::Exception((err, _)) = &mut exc {
                        err.msg = format!("At left function: {}", err.msg);
                    }
                    exc
                })
            },
            Function::Partial(left, right) => {
                let args = {
                    let mut argsx = vec![*right.clone()];
                    argsx.extend(args);
                    argsx
                };
                self.call_function(left, args, module, span, func_span)
            },
            Function::Standart(func) => {
                if args.len() != func.args.len() {
                    return self.error(format!("Expected `{}` Arguments, Instead Found `{}`", func.args.len(), args.len()), span, module.source.clone())
                }
        
                let clos_count = func.clos
                    .as_ref()
                    .map(|clos| {
                        for (arg, value) in clos {
                            self.define_local(arg.clone(), value.clone());
                        }
                        clos.len()
                    })
                    .unwrap_or(0);
        
                let mut local_count = 0;
                for (arg, value) in std::iter::zip(&func.args, args) {
                    if !self.fits_pattern(&value, arg, &mut local_count) {
                        self.remove_local(clos_count + local_count);
                        return self.error("Function Argument Couldn't Be Matched", arg.span, module.source.clone());
                    }
                }
            
                let result = self.evaluate(&func.expr, &func.modl);
                self.remove_local(local_count + clos_count);
        
                match result {
                    Ok(value) => Ok(value),
                    Err(exc) => match exc {
                        Exception::Return(value) => Ok(value),
                        Exception::Exception((
                            Error { msg, span: inner_span, .. }, source
                        )) => self.call_error(msg.clone(), inner_span, span, module.source.clone(), source.clone()),
                        _ => Err(exc)
                    },
                }
            }
        }
    }

    fn evaluate_application_expr(&mut self, application_expr: &ApplicationExpr, module: &Module, span: Span) -> EvaluationResult {
        let ApplicationExpr { func, args } = application_expr;

        let arg_values: Result<Vec<_>, _> = args.iter().map(|expr| self.evaluate(expr, module)).collect();
        let arg_values = arg_values?; 

        let func_value = match self.evaluate(func, module)?.as_function() {
            Ok(func) => func,
            Err(msg) => return self.error(msg, span, module.source.clone()),
        };

        self.call_function(&func_value, arg_values, module, span, func.span)
    }

    fn evaluate_sequence_expr(&mut self, sequnce_expr: &SequenceExpr, module: &Module) -> EvaluationResult {
        let SequenceExpr { lhs, rhs } = sequnce_expr;

        self.evaluate(lhs, module)?;
        self.evaluate(rhs, module)
    }

    fn evaluate_match_expr(&mut self, match_expr: &MatchExpr, module: &Module, span: Span) -> EvaluationResult {
        let MatchExpr { expr, arms } = match_expr;

        let value = self.evaluate(expr, module)?;
        for (pattern, expr) in arms {
            let mut local_count = 0;
            if self.fits_pattern(&value, pattern, &mut local_count) {
                let result = self.evaluate(expr, module);
                self.remove_local(local_count);
                return result
            }
            self.remove_local(local_count);
        }

        self.error("Inexhaustive Match Expression, None of the Arms were Matched", span, module.source.clone())
    }

    fn fits_pattern(&mut self, value: &Value, pattern: &Spanned<Pattern>, local_count: &mut usize) -> bool {
        match (value, &pattern.data) {
            (Value::String(lstring), Pattern::String(rstring)) => lstring == rstring,
            (Value::Integer(int), Pattern::NonNegativeInteger(rint)) => int == &Self::parse_integer(rint),
            (Value::Integer(int), Pattern::NegativeInteger(rint)) => int == &(-&Self::parse_integer(rint)),
            (Value::Float(lfloat), Pattern::NonNegativeFloat(rfloat)) => lfloat == &rfloat.parse::<value::Float>().unwrap(),
            (Value::Float(lfloat), Pattern::NegativeFloat(rfloat)) => lfloat == &-rfloat.parse::<value::Float>().unwrap(),
            (Value::Bool(lbool), Pattern::Bool(rbool)) => lbool == rbool,
            (Value::Unit, Pattern::Unit) => true,
            (Value::List(list), Pattern::List(ListPattern { before_rest, after_rest, rest })) => {
                match rest {
                    Some(name) => {
                        let list = list.borrow();
                        let result = before_rest.len() + after_rest.len() <= list.len() &&
                            std::iter::zip(list.iter(), before_rest)
                                .all(|(value, pattern)| self.fits_pattern(value, pattern, local_count)) &&
                            std::iter::zip(list.iter().rev(), after_rest.iter().rev())
                                .all(|(value, pattern)| self.fits_pattern(value, pattern, local_count));
                    
                        if let Some(name) = name {
                            let rest = &list[before_rest.len()..list.len() - after_rest.len()];
                            self.define_local(name.clone(), Value::List(Rc::new(RefCell::new(rest.to_vec()))));
                            *local_count += 1;
                        };

                        result
                    },
                    None => {
                        let list = list.borrow();
                        list.len() == before_rest.len() &&
                        std::iter::zip(list.iter(), before_rest)
                            .all(|(value, pattern)| self.fits_pattern(value, pattern, local_count))
                    }
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
            (_, Pattern::Or(OrPattern { lhs, rhs })) => {
                let lc = *local_count;
                self.fits_pattern(value, lhs, local_count) || {
                    self.remove_local(*local_count);
                    *local_count = lc; 
                    self.fits_pattern(value, rhs, local_count)
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

    fn evaluate_import_expr(&mut self, import_expr: &ImportExpr, module: &Module, span: Span) -> EvaluationResult {
        let ImportExpr { parts } = import_expr;
    
        let file_path = parts.join("/") + ".ank";
        let file = match std::fs::read_to_string(&file_path) {
            Ok(value) => value,
            Err(_) => return self.error(format!("Couldn't Load the File: `{file_path}`"), span, module.source.clone()),
        };
        let mut reporter = Reporter::new();
        let tokens = handle_error!(Lexer::new(&file).collect(), "tokenizing", reporter, &file_path);
        let astree = handle_error!(Parser::new(tokens).parse_module(), "tokenizing", reporter, &file_path);
        Ok(Value::Module(self.evaluate_module(&file_path, &astree)?))
    }

    fn evaluate_list_expr(&mut self, list_expr: &ListExpr, module: &Module) -> EvaluationResult {
        let ListExpr { exprs } = list_expr;

        Ok(Value::List(Rc::new(RefCell::new(exprs.iter().map(|expr| self.evaluate(expr, module)).collect::<Result<Vec<_>, _>>()?))))
    }

    fn evaluate_access_expr(&mut self, access_expr: &AccessExpr, module: &Module) -> EvaluationResult {
        let AccessExpr { expr, name } = access_expr;

        let value = self.evaluate(expr, module)?;
        let (Value::Module(Module { map, .. }) | Value::Structure(map)) = value else {
            return self.error(format!("Field access is only available for `Module`s and `Structure`s, not for `{}`", value.type_name()), expr.span, module.source.clone())
        };
        
        let map = map.borrow();
        match map.get(&name.data) {
            Some(value) => Ok(value.clone()),
            None => self.error(format!("Value Has No Field Called `{}`", name.data), name.span, module.source.clone()),
        }
    }

    fn evaluate_structure_expr(&mut self, structure_expr: &StructureExpr, module: &Module) -> EvaluationResult {
        let StructureExpr { fields } = structure_expr;

        let mut structure = HashMap::new();
        for (field_name, expr) in fields {
            structure.insert(field_name.clone(), self.evaluate(expr, module)?);
        } 

        Ok(Value::Structure(Rc::new(RefCell::new(structure))))
    }


    fn evaluate_assignment_expr(&mut self, assignment_expr: &AssignmentExpr, module: &Module) -> EvaluationResult {
        let AssignmentExpr { lhs, rhs } = assignment_expr;

        let rvalue = self.evaluate(rhs, module)?;

        match &lhs.data {
            Expr::Identifier(ident) => self.assign(ident, rvalue, module, lhs.span),
            Expr::Access(AccessExpr { expr, name }) => {

                let value = self.evaluate(expr, module)?;
                let (Value::Module(Module { map, .. }) | Value::Structure(map)) = value else {
                    return self.error(format!("Field access is only available for `Module`s and `Structure`s, not for `{}`", value.type_name()), expr.span, module.source.clone())
                }; 

                let mut map = map.borrow_mut();
                match map.get_mut(&name.data) {
                    Some(target) => {
                        *target = rvalue;
                        Ok(Value::Unit)
                    },
                    None => self.error(format!("Value Has No Field Called `{}`", name.data), name.span, module.source.clone())
                }
            }
            _ => self.error("Invalid Assignment Target", lhs.span, module.source.clone())
        }
    }

    fn evaluate_while_expr(&mut self, while_expr: &WhileExpr, module: &Module) -> EvaluationResult {
        let WhileExpr { cond, body } = while_expr;

        loop {
            match self.evaluate(cond, module)?.as_bool() {
                Ok(value) => if value {
                    if let Err(exc) = self.evaluate(body, module) {
                        match exc {
                            Exception::Break(value) => break Ok(value),
                            Exception::Continue => continue,
                            _ => return Err(exc)
                        }
                    }
                } else {
                    break Ok(Value::Unit)
                },
                Err(err) => break self.error(err, cond.span, module.source.clone())
            }
        }

    }

    fn evaluate_for_expr(&mut self, for_expr: &ForExpr, module: &Module) -> EvaluationResult {
        let ForExpr { patt, expr, body } = for_expr;

        let mut iter: Box<dyn Iterator<Item = Value>> = match self.evaluate(expr, module)? {
            Value::Range(iter) => Box::new(iter),
            Value::List(list) => Box::new(list.borrow().clone().into_iter()),
            not_iter => return self.error(format!("`{}` is not an Iterator", not_iter.type_name()), expr.span, module.source.clone())
        };
    
        loop {
            let mut local_count = 0;
            match iter.next() {
                Some(value) => {
                    if !self.fits_pattern(&value, patt, &mut local_count) {
                        self.remove_local(local_count);
                        return self.error(format!("Loop variable didn't match the pattern"), patt.span, module.source.clone())
                    };
                    let result = self.evaluate(body, module);
                    self.remove_local(local_count);
                
                    if let Err(exc) = result {
                        match exc {
                            Exception::Break(value) => break Ok(value),
                            Exception::Continue => continue,
                            _ => return Err(exc)
                        }
                    }
                }
                None => break Ok(Value::Unit),
            }
        }
    }

    fn evaluate_if_expr(&mut self, if_expr: &IfExpr, module: &Module) -> EvaluationResult {
        let IfExpr { cond, truu, fals } = if_expr;
    
        match self.evaluate(cond, module)?.as_bool() {
            Ok(value) => if value {
                self.evaluate(truu, module)
            } else if let Some(fals) = fals {
                self.evaluate(fals, module)
            } else {
                Ok(Value::Unit)
            },
            Err(err) => self.error(err, cond.span, module.source.clone())
        }
    }

    fn evaluate_return_expr(&mut self, return_expr: &ReturnExpr, module: &Module) -> EvaluationResult {
        let ReturnExpr { expr } = return_expr;

        let value = self.evaluate(expr, module)?;
        Err(Exception::Return(value))
    }


    fn evaluate_break_expr(&mut self, break_expr: &BreakExpr, module: &Module) -> EvaluationResult {
        let BreakExpr { expr } = break_expr;

        let value = self.evaluate(expr, module)?;
        Err(Exception::Break(value))
    }

    fn evaluate_continue_expr(&mut self) -> EvaluationResult {
        Err(Exception::Continue)
    }

    fn evaluate_raise_expr(&mut self, raise_expr: &RaiseExpr, module: &Module, span: Span) -> EvaluationResult {
        let RaiseExpr { expr } = raise_expr;

        let value = self.evaluate(expr, module)?;
        self.error(value.to_string(), span, module.source.clone())
    }

    fn evaluate_tryhandle_expr(&mut self, tryhandle_expr: &TryHandleExpr, module: &Module) -> EvaluationResult {
        let TryHandleExpr { expr, hndl } = tryhandle_expr;

        match self.evaluate(expr, module) {
            Ok(value) => Ok(value),
            Err(exc) => match exc {
                Exception::Exception(_) => self.evaluate(hndl, module),
                _ => Err(exc)
            }
        }
    }

    fn evaluate_module_expr(&mut self, module_expr: &ModuleExpr, module: &Module) -> EvaluationResult {
        let ModuleExpr { definitions } = module_expr;
        
        let module = self.evaluate_module(&module.source, definitions)?;

        Ok(Value::Module(module))
    }

    pub fn evaluate_module(&mut self, source: &str, definitions: &Vec<(String, Spanned<Expr>)>) -> Result<Module, Exception> {
        let mut engine = Self::new(); 
        let module = get_prelude(source);
        for (name, expr) in definitions {
            let value = engine.evaluate(expr, &module)?;
            module.map.borrow_mut().insert(name.clone(), value);
        }

        Ok(module)
    }

    pub fn run_from_entry(source: &str, definitions: &Vec<(String, Spanned<Expr>)>, cli_args: &[String]) -> EvaluationResult {
        let mut engine = Self::new();
        let module = engine.evaluate_module(source, definitions)?;
        let main = match module.map.borrow_mut().remove("main") {
            Some(main) => main,
            // TODO: Better error reporting when main function is not provided
            None => {
                eprintln!("No Entry Point is Provided !!");
                std::process::exit(1);
            }
        };

        let main_func = match main.as_function() {
            Ok(func) => func,
            Err(_) => {
                let span = definitions.iter().find(|(name, _)| name == "main").unwrap().1.span;
                return engine.error("Symbol Main has to Be a Function", span, module.source.clone())
            },
        };

        if let Function::Standart(func) = main_func {
            if let Some(_) = func.clos {
                let span = definitions.iter().find(|(name, _)| name == "main").unwrap().1.span;
                return engine.error("Main Function cannot Capture Variables", span, module.source.clone())
            }

            let mut local_count = 0;
            match &func.args[..] {
                [] => (),
                [x] => {
                    let value = Value::List(Rc::new(RefCell::new(
                        cli_args
                            .iter()
                            .map(|arg| Value::String(arg.clone()))
                            .collect()
                    )));
                    if !engine.fits_pattern(&value, x, &mut local_count) {
                        return engine.error("CLI Arguments Don't Have the Expected Pattern", x.span, module.source.clone());
                    }
                }
                [_, x, ..] => {
                    return engine.error(format!("Main Function can have at most 1 Argument, instead provided {}", func.args.len()), x.span, module.source.clone());
                }
            }

            let result = engine.evaluate(&func.expr, &module);
            engine.remove_local(local_count);

            match result {
                Ok(value) => Ok(value),
                Err(exc) => match exc {
                    Exception::Return(value) => Ok(value),
                    _ => Err(exc)
                },
            }
        } else {
            let span = definitions.iter().find(|(name, _)| name == "main").unwrap().1.span;
            engine.error(format!("Symbol Main has to Be a Standart Function not `{main_func}`"), span, module.source.clone())
        }
    }
}
