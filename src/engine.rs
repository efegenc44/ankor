use std::{rc::Rc, collections::HashMap};

use crate::{
    expr::{ApplicationExpr, Expr, FunctionExpr, LetExpr, SequenceExpr},
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
            Integer(int) => Value::Integer(int.parse().unwrap()),
            Identifier(ident) => self.resolve_identifier(ident),
            Let(let_expr) => self.evaluate_let_expr(let_expr),
            Function(function_expr) => self.evaluate_function_expr(function_expr),
            Application(application_expr) => self.evaluate_application_expr(application_expr),
            Sequence(sequnce_expr) => self.evaluate_sequence_expr(sequnce_expr),
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

                if let Some(clos) = &func.clos {
                    for (arg, value) in clos {
                        self.define_local(arg.clone(), value.clone());
                    }
                }
                
                for (arg, value) in std::iter::zip(func.args.clone(), arg_values) {
                    self.define_local(arg, value);
                }
                let result = self.evaluate(&func.expr);
                self.remove_local(args.len());
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
}
