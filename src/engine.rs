use std::{rc::Rc, cell::RefCell};

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
    value::{FunctionValue, Value, self, ModuleValue}, span::{Spanned, Span}, handle_error, reporter::Reporter, error::Error,
};

pub struct Engine {
    locals: Vec<(String, Value)>,

    return_exception: Option<Value>,
    break_exception: Option<Value>,
    continue_exception: bool,

    pub error: Option<(Error, String)>
}

impl Engine {
    pub fn new() -> Self {
        Self { 
            locals: vec![], 
            return_exception: None, 
            break_exception: None, 
            continue_exception: false,
            error: None, 
        }
    }

    fn define_local(&mut self, name: String, value: Value) {
        self.locals.push((name, value));
    }

    fn remove_local(&mut self, n: usize) {
        for _ in 0..n {
            self.locals.pop().unwrap();
        }
    }

    fn set_error<S>(&mut self, msg: S, span: Span, source: String) -> Value 
    where
        S: Into<String> 
    {
        self.error = Some((Error::new(msg, span, None), source));
        Value::Unit
    }

    fn set_call_error<S>(&mut self, msg: S, span: Span, call_site: Span, call_site_source: String, source: String) -> Value 
    where
        S: Into<String> 
    {
        self.error = Some((
            Error::new(msg, span, Some((Box::new(Error::new("Call Site", call_site, None)), call_site_source))), 
            source
        ));
        Value::Unit
    }

    fn resolve_identifier(&mut self, ident: &str, module: &ModuleValue, span: Span) -> Value {
        if let Some((_, value)) = self.locals.iter().rev().find(|bind| bind.0 == ident) {
            return value.clone();
        }

        match module.map.borrow().get(ident) {
            Some(value) => value.clone(),
            None => self.set_error(format!("Undefined Symbol `{ident}`"), span, module.source.clone())
        }
    }

    fn assign(&mut self, ident: &str, value: Value, module: &ModuleValue, span: Span) {
        if let Some((_, target)) = self.locals.iter_mut().rev().find(|bind| bind.0 == ident) {
            return *target = value;
        }

        match module.map.borrow_mut().get_mut(ident) {
            Some(target) => *target = value,
            None => {
                self.set_error(format!("Undefined Symbol `{ident}`"), span, module.source.clone());
            }
        } 
    }

    fn parse_integer(number: &str) -> Value {
        match number.parse() {
            Ok(number) => Value::Integer(number),
            Err(_) => Value::BigInteger(BigInt::try_from(number).unwrap()),
        }
    }

    fn is_exception_occured(&self) -> bool {
        self.return_exception.is_some() || 
        self.break_exception.is_some()  || 
        self.continue_exception         ||
        self.error.is_some()
    }

    pub fn evaluate(&mut self, expr: &Spanned<Expr>, module: &ModuleValue) -> Value {
        use Expr::*;

        if self.is_exception_occured() {
            return Value::Unit
        }
        
        match &expr.data {
            Integer(int) => Self::parse_integer(int),
            Float(float) => Value::Float(float.parse().unwrap()),
            String(string) => Value::String(string.clone()),
            Bool(bool) => Value::Bool(*bool),
            UnitValue => Value::Unit,
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

    fn evaluate_let_expr(&mut self, let_expr: &LetExpr, module: &ModuleValue) -> Value {
        let LetExpr { patt, vexp, expr } = let_expr;

        let value = self.evaluate(vexp, module);
        let mut local_count = 0;
        if !self.fits_pattern(&value, patt, &mut local_count) {
            self.set_error("Pattern Couldn't Be Matched at Let Expression", patt.span, module.source.clone());
        }
        let result = self.evaluate(expr, module);
        self.remove_local(local_count);
        result
    }

    fn evaluate_function_expr(&mut self, function_expr: &FunctionExpr, module: &ModuleValue) -> Value {
        let function_value = FunctionValue {
            args: function_expr.args.clone(),
            expr: function_expr.expr.clone(),
            clos: function_expr.clos
                .as_ref()
                .map(|clos| clos
                    .iter()
                    .map(|ident| (ident.data.clone(), self.resolve_identifier(&ident.data, module, ident.span))).collect()),
            modl: module.clone()
        };
        
        Value::Function(Rc::new(function_value))
    }

    fn evaluate_application_expr(&mut self, application_expr: &ApplicationExpr, module: &ModuleValue, span: Span) -> Value {
        let ApplicationExpr { func, args } = application_expr;

        let arg_values: Vec<_> = args.iter().map(|expr| self.evaluate(expr, module)).collect();

        let func_value = self.evaluate(func, module);

        if self.is_exception_occured() {
            return Value::Unit;
        } 

        match func_value {
            Value::Native(func) => match func(&arg_values) {
                Ok(value) => value,
                Err(err) => self.set_error(err, span, module.source.clone()),
            },
            Value::Function(func) => {
                if args.len() != func.args.len() {
                    return self.set_error(format!("Expected `{}` Arguments, Instead Found `{}`", func.args.len(), args.len()), span, module.source.clone())
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

                for (arg, value) in std::iter::zip(&func.args, arg_values) {
                    if !self.fits_pattern(&value, arg, &mut local_count) {
                        self.set_error("Function Argument Couldn't Be Matched", arg.span, module.source.clone());
                    }
                }

                let result = self.evaluate(&func.expr, &func.modl);
                self.remove_local(local_count + clos_count);

                if let Some((Error { msg, span: inner_span, .. }, source)) = &self.error {
                    self.set_call_error(msg.clone(), *inner_span, span, module.source.clone(), source.clone())
                } else {
                    let result = self.return_exception.clone().unwrap_or(result);
                    self.return_exception = None;
                    result
                }
            },
            // TODO: Report type here
            wrong_type => self.set_error(format!("`{wrong_type}` is not Function"), func.span, module.source.clone())
        }
    }

    fn evaluate_sequence_expr(&mut self, sequnce_expr: &SequenceExpr, module: &ModuleValue) -> Value {
        let SequenceExpr { lhs, rhs } = sequnce_expr;

        self.evaluate(lhs, module);
        self.evaluate(rhs, module)
    }

    fn evaluate_match_expr(&mut self, match_expr: &MatchExpr, module: &ModuleValue, span: Span) -> Value {
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

        self.set_error("Inexhaustive Match Expression, None of the Arms were Matched", span, module.source.clone())
    }

    // TODO: Maybe pass value as value rather than reference
    fn fits_pattern(&mut self, value: &Value, pattern: &Spanned<Pattern>, local_count: &mut usize) -> bool {
        // if self.is_exception_occured() {
        //     return true
        // }
        
        match (value, &pattern.data) {
            (Value::String(lstring), Pattern::String(rstring)) => lstring == rstring,
            (Value::Integer(_) | Value::BigInteger(_), Pattern::NonNegativeInteger(rint)) => value == &Self::parse_integer(rint),
            (Value::Integer(_) | Value::BigInteger(_), Pattern::NegativeInteger(rint)) => value == &(-&Self::parse_integer(rint)).unwrap(),
            (Value::Float(lfloat), Pattern::NonNegativeFloat(rfloat)) => lfloat == &rfloat.parse::<value::Float>().unwrap(),
            (Value::Float(lfloat), Pattern::NegativeFloat(rfloat)) => lfloat == &-rfloat.parse::<value::Float>().unwrap(),
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

    fn evaluate_import_expr(&mut self, import_expr: &ImportExpr, module: &ModuleValue, span: Span) -> Value {
        let ImportExpr { parts } = import_expr;
    
        let file_path = parts.join("/") + ".ank";
        let file = match std::fs::read_to_string(&file_path) {
            Ok(value) => value,
            Err(_) => return self.set_error(format!("Couldn't Load the File: `{file_path}`"), span, module.source.clone()),
        };
        let mut reporter = Reporter::new();
        let tokens = handle_error!(Lexer::new(&file).collect(), "tokenizing", reporter, &file_path);
        let astree = handle_error!(Parser::new(tokens).parse_module(), "tokenizing", reporter, &file_path);
        Value::Module(self.evaluate_module(&file_path, &astree))
    }

    fn evaluate_list_expr(&mut self, list_expr: &ListExpr, module: &ModuleValue) -> Value {
        let ListExpr { exprs } = list_expr;

        Value::List(Rc::new(exprs.iter().map(|expr| self.evaluate(expr, module)).collect()))
    }

    fn evaluate_access_expr(&mut self, access_expr: &AccessExpr, module: &ModuleValue) -> Value {
        let AccessExpr { expr, name } = access_expr;

        let value = self.evaluate(expr, module);

        if self.is_exception_occured() {
            return Value::Unit;
        }

        let (Value::Module(ModuleValue { map, .. }) | Value::Structure(map)) = value else {
            // TODO: Report type here
            return self.set_error("Field access is only available for `Module`s and `Structure`s", expr.span, module.source.clone())
        };
        
        let map = map.borrow();
        match map.get(&name.data) {
            Some(value) => value.clone(),
            None => self.set_error(format!("Value Has No Field Called `{}`", name.data), name.span, module.source.clone()),
        }
    }

    fn evaluate_structure_expr(&mut self, structure_expr: &StructureExpr, module: &ModuleValue) -> Value {
        let StructureExpr { fields } = structure_expr;

        let structure = fields
            .iter()
            .map(|(field_name, expr)| (field_name.clone(), self.evaluate(expr, module))).collect();

        Value::Structure(Rc::new(RefCell::new(structure)))
    }


    fn evaluate_assignment_expr(&mut self, assignment_expr: &AssignmentExpr, module: &ModuleValue) -> Value {
        let AssignmentExpr { lhs, rhs } = assignment_expr;

        let rvalue = self.evaluate(rhs, module);

        match &lhs.data {
            Expr::Identifier(ident) => self.assign(ident, rvalue, module, lhs.span),
            Expr::Access(AccessExpr { expr, name }) => {

                let value = self.evaluate(expr, module);

                if self.is_exception_occured() {
                    return Value::Unit;
                }
        
                let (Value::Module(ModuleValue { map, .. }) | Value::Structure(map)) = value else {
                    return self.set_error("Field access is only available for `Module`s and `Structure`s", expr.span, module.source.clone())
                }; 

                let mut map = map.borrow_mut();
                match map.get_mut(&name.data) {
                    Some(target) => *target = rvalue,
                    None => {
                        self.set_error(format!("Value Has No Field Called `{}`", name.data), name.span, module.source.clone());
                    }
                }
            }
            _ => {
                self.set_error("Invalid Assignment Target", lhs.span, module.source.clone());
            }
        }
    
        Value::Unit
    }

    fn evaluate_while_expr(&mut self, while_expr: &WhileExpr, module: &ModuleValue) -> Value {
        let WhileExpr { cond, body } = while_expr;

        let mut result = Value::Unit;
        loop {
            if self.continue_exception {
                self.continue_exception = false;
            }

            if let Some(value) = self.break_exception.clone() {
                self.break_exception = None;
                result = value;
                break; 
            }

            let value = self.evaluate(cond, module);

            if self.is_exception_occured() {
                break
            }

            match value.to_bool() {
                Ok(value) => if value {
                    self.evaluate(body, module);
                } else {
                    break
                },
                Err(err) => {
                    self.set_error(err, cond.span, module.source.clone());
                }
            }
        }

        result
    }

    fn evaluate_for_expr(&mut self, for_expr: &ForExpr, module: &ModuleValue) -> Value {
        let ForExpr { patt, expr, body } = for_expr;

        let mut result = Value::Unit;

        let value = self.evaluate(expr, module);

        if self.is_exception_occured() {
            return Value::Unit;
        }

        let mut iter: Box<dyn Iterator<Item = Value>> = match value {
            Value::Integer(int) => Box::new((0..int).map(Value::Integer)),
            Value::List(list) => {
                let len = list.len();
                Box::new((0..len).map(move |i| list[i].clone()))
            },
            // TODO: Report type here
            not_iter => return self.set_error(format!("`{not_iter}` is not an Iterator"), expr.span, module.source.clone())
        };
    
        loop {
            if self.continue_exception {
                self.continue_exception = false;
            }

            if let Some(value) = self.break_exception.clone() {
                self.break_exception = None;
                result = value;
                break; 
            }

            let mut local_count = 0;
            match iter.next() {
                Some(value) => {
                    self.fits_pattern(&value, patt, &mut local_count);
                    self.evaluate(body, module);
                    self.remove_local(local_count)
                }
                None => break,
            }
        }

        result
    }

    fn evaluate_if_expr(&mut self, if_expr: &IfExpr, module: &ModuleValue) -> Value {
        let IfExpr { cond, truu, fals } = if_expr;
    
        let value = self.evaluate(cond, module);

        if self.is_exception_occured() {
            return Value::Unit;
        }

        match value.to_bool() {
            Ok(value) => if value {
                self.evaluate(truu, module)
            } else if let Some(fals) = fals {
                self.evaluate(fals, module)
            } else {
                Value::Unit
            },
            Err(err) => self.set_error(err, cond.span, module.source.clone())
        }

        
    }

    fn evaluate_return_expr(&mut self, return_expr: &ReturnExpr, module: &ModuleValue) -> Value {
        let ReturnExpr { expr } = return_expr;

        let value = self.evaluate(expr, module);
        
        if self.is_exception_occured() {
            return Value::Unit;
        }

        self.return_exception = Some(value);

        Value::Unit
    }


    fn evaluate_break_expr(&mut self, break_expr: &BreakExpr, module: &ModuleValue) -> Value {
        let BreakExpr { expr } = break_expr;

        let value = self.evaluate(expr, module);

        if self.is_exception_occured() {
            return Value::Unit;
        }

        self.break_exception = Some(value);

        Value::Unit
    }

    fn evaluate_continue_expr(&mut self) -> Value {
        self.continue_exception = true; 
        
        Value::Unit
    }

    fn evaluate_raise_expr(&mut self, raise_expr: &RaiseExpr, module: &ModuleValue, span: Span) -> Value {
        let RaiseExpr { expr } = raise_expr;

        let value = self.evaluate(expr, module);

        if self.is_exception_occured() {
            if let Some((Error { msg, span: inner_span, .. }, source)) = &self.error {
                self.set_call_error(msg.clone(), inner_span.clone(), span, module.source.clone(), source.clone());
            }

            return Value::Unit;
        }

        self.set_error(value.to_string(), span, module.source.clone())
    }


    fn evaluate_tryhandle_expr(&mut self, tryhandle_expr: &TryHandleExpr, module: &ModuleValue) -> Value {
        let TryHandleExpr { expr, hndl } = tryhandle_expr;

        let value = self.evaluate(expr, module);

        if self.error.is_some() {
            self.error = None;

            return self.evaluate(hndl, module)            
        } else {
            value
        }
    }

    fn evaluate_module_expr(&mut self, module_expr: &ModuleExpr, module: &ModuleValue) -> Value {
        let ModuleExpr { definitions } = module_expr;
        
        let module = self.evaluate_module(&module.source, definitions);

        Value::Module(module)
    }

    pub fn evaluate_module(&mut self, source: &str, definitions: &Vec<(String, Spanned<Expr>)>) -> ModuleValue {
        let mut engine = Self::new(); 
        let module = get_prelude(source);
        for (name, expr) in definitions {
            let value = engine.evaluate(expr, &module);
            module.map.borrow_mut().insert(name.clone(), value);
        }

        self.error = engine.error;        

        module
    }

    pub fn run_from_entry(source: &str, definitions: &Vec<(String, Spanned<Expr>)>, cli_args: &[String]) -> Value {
        let mut engine = Self::new();
        let module = engine.evaluate_module(source, definitions);
        let main = match module.map.borrow_mut().remove("main") {
            Some(main) => main,
            // TODO: Better error reporting when main function is not provided
            None => {
                eprintln!("No Entry Point is Provided !!");
                std::process::exit(1);
            }
        };

        let result = match main {
            Value::Function(func) => {
                let clos_count = func.clos
                    .as_ref()
                    .map(|clos| {
                        for (arg, value) in clos {
                            engine.define_local(arg.clone(), value.clone());
                        }
                        clos.len()
                    })
                    .unwrap_or(0);
               
                let mut local_count = 0;
                match &func.args[..] {
                    [] => (),
                    [x] => {
                        let value = Value::List(Rc::new(
                            cli_args
                                .iter()
                                .map(|arg| Value::String(arg.clone()))
                                .collect()
                        ));
                        if !engine.fits_pattern(&value, x, &mut local_count) {
                            engine.set_error("CLI Arguments Don't Have the Expected Pattern", x.span, module.source.clone());
                        }
                    }
                    [_, x, ..] => {
                        engine.set_error(format!("Main Function can have at most 1 Argument, instead provided {}", func.args.len()), x.span, module.source.clone());
                    }
                }

                let result = engine.evaluate(&func.expr, &module);
                engine.remove_local(local_count + clos_count);
                
                // if let Some((Error { msg, span: inner_span, .. }, source)) = &engine.error {
                //     let span = definitions.iter().find(|(name, _)| name == "main").unwrap().1.span;
                //     engine.set_call_error(msg.clone(), *inner_span, span, module.source.clone(), source.clone())
                // } else {
                    let result = engine.return_exception.clone().unwrap_or(result);
                    engine.return_exception = None;
                    result
                // }
            },
    
            _ => {
                let span = definitions.iter().find(|(name, _)| name == "main").unwrap().1.span;
                engine.set_error("Symbol Main has to Be a Function", span, module.source.clone())
            }
        };

        if let Some((err, source)) = engine.error {
            let mut reporter = Reporter::new();
            reporter.report(&source, err, "runtime")
        };

        result
    }
}
