use std::{rc::Rc, collections::HashMap};

use crate::{
    expr::{ApplicationExpr, Expr, FunctionExpr, LetExpr, SequenceExpr, MatchExpr, Pattern},
    value::{Value, FunctionValue}, global::get_global,
};

pub struct Engine {
    global: HashMap<String, Value>,
    locals: Vec<(String, Value)>,
}

impl Engine {
    pub fn new() -> Self {
        Self { global: get_global(), locals: vec![] }
    }

    fn define_local(&mut self, name: String, value: Value) {
        self.locals.push((name, value));
    }

    fn remove_local(&mut self, n: usize) {
        for _ in 0..n {
            self.locals.pop().unwrap();
        }
    }

    fn resolve_identifier(&self, ident: &str) -> Value {
        for (name, value) in self.locals.iter().rev() {
            if name == ident {
                return value.clone();
            }
        }

        match self.global.get(ident) {
            Some(value) => value.clone(),
            None => todo!("Error handling"),
        } 
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Value {
        use Expr::*;
        match expr {
            Identifier(ident) => self.resolve_identifier(ident),
            Integer(int) => Value::Integer(int.parse().unwrap()),
            Bool(bool) => Value::Bool(*bool),
            Let(let_expr) => self.evaluate_let_expr(let_expr),
            Function(function_expr) => self.evaluate_function_expr(function_expr),
            Application(application_expr) => self.evaluate_application_expr(application_expr),
            Sequence(sequnce_expr) => self.evaluate_sequence_expr(sequnce_expr),
            Match(match_expr) => self.evaluate_match_expr(match_expr),
            UnitValue => Value::Unit,
        }
    }

    fn evaluate_let_expr(&mut self, let_expr: &LetExpr) -> Value {
        let LetExpr { name, vexp, expr } = let_expr;
        let value = self.evaluate(vexp);
        self.define_local(name.clone(), value);
        let result = self.evaluate(expr);
        self.remove_local(1);
        result
    }

    fn evaluate_function_expr(&mut self, function_expr: &FunctionExpr) -> Value {
        let function_value = FunctionValue {
            args: function_expr.args.clone(),
            expr: function_expr.expr.clone(),
            clos: function_expr.clos
                .as_ref()
                .map(|clos| clos
                    .iter()
                    .map(|ident| (ident.clone(), self.resolve_identifier(ident))).collect()),
        };
        
        Value::Function(Rc::new(function_value))
    }

    fn evaluate_application_expr(&mut self, application_expr: &ApplicationExpr) -> Value {
        let ApplicationExpr { func, args } = application_expr;

        let arg_values: Vec<_> = args.iter().map(|expr| self.evaluate(expr)).collect();

        match self.evaluate(func) {
            Value::Native(func) => func(&arg_values),
            Value::Function(func) => {
                if args.len() != func.args.len() {
                    todo!("Error handling")
                }

                let clos_count = if let Some(clos) = &func.clos {
                    for (arg, value) in clos {
                        self.define_local(arg.clone(), value.clone());
                    }
                    clos.len()
                } else {
                    0
                };
                
                for (arg, value) in std::iter::zip(func.args.clone(), arg_values) {
                    self.define_local(arg, value);
                }
                let result = self.evaluate(&func.expr);
                self.remove_local(args.len() + clos_count);
                result
            },
            _ => todo!("Error handling")
        }
    }

    fn evaluate_sequence_expr(&mut self, sequnce_expr: &SequenceExpr) -> Value {
        let SequenceExpr { lhs, rhs } = sequnce_expr;
        self.evaluate(lhs);
        self.evaluate(rhs)
    }

    fn evaluate_match_expr(&mut self, match_expr: &MatchExpr) -> Value {
        let MatchExpr { expr, arms } = match_expr;

        let value = self.evaluate(expr);
        for (pattern, expr) in arms {
            if let Some(local_count) = self.fits_pattern(&value, pattern) {
                let result = self.evaluate(expr);
                self.remove_local(local_count);
                return result
            }
        }

        todo!("Error handling")
    }

    fn fits_pattern(&mut self, value: &Value, pattern: &Pattern) -> Option<usize> {
        let mut local_count = 0;

        match (value, pattern) {
            (Value::Integer(lint), Pattern::NonNegativeInteger(rint)) => lint == &rint.parse::<isize>().unwrap(),
            (Value::Integer(lint), Pattern::NegativeInteger(rint)) => lint == &-rint.parse::<isize>().unwrap(),
            (Value::Bool(lbool), Pattern::Bool(rbool)) => lbool == rbool,
            (Value::Unit, Pattern::Unit) => true,
            (_, Pattern::Identifier(ident)) => {
                self.define_local(ident.clone(), value.clone());
                local_count += 1;
                true
            },
            _ => false
        }.then_some(local_count)
    } 
}
