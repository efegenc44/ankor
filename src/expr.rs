use std::collections::HashMap;

use crate::span::{HasSpan, Spanned};

#[derive(Clone)]
pub enum Expr {
    Integer(String),
    Float(String),
    String(String),
    Bool(bool),
    Identifier(String),
    Let(LetExpr),
    Function(FunctionExpr),
    Application(ApplicationExpr),
    Sequence(SequenceExpr),
    Match(MatchExpr),
    Import(ImportExpr),
    Access(AccessExpr),
    List(ListExpr),
    Structure(StructureExpr),
    Assignment(AssignmentExpr),
    Module(ModuleExpr),
    While(WhileExpr),
    For(ForExpr),
    If(IfExpr),
    Return(ReturnExpr),
    Break(BreakExpr),
    Continue,
    UnitValue
}

impl HasSpan for Expr {}

#[derive(Clone)]
pub struct LetExpr {
    pub patt: Spanned<Pattern>,
    pub vexp: Box<Spanned<Expr>>,
    pub expr: Box<Spanned<Expr>>,
}

#[derive(Clone)]
pub struct FunctionExpr {
    pub args: Vec<Spanned<Pattern>>,
    pub expr: Box<Spanned<Expr>>,
    pub clos: Option<Vec<Spanned<String>>>
}

#[derive(Clone)]
pub struct ApplicationExpr {
    pub func: Box<Spanned<Expr>>,
    pub args: Vec<Spanned<Expr>>,
}

#[derive(Clone)]
pub struct SequenceExpr {
    pub lhs: Box<Spanned<Expr>>,
    pub rhs: Box<Spanned<Expr>>,
}

#[derive(Clone)]
pub struct MatchExpr {
    pub expr: Box<Spanned<Expr>>,
    pub arms: Vec<(Spanned<Pattern>, Spanned<Expr>)>
}

#[derive(Clone)]
pub enum Pattern {
    Identifier(String),
    String(String),
    NonNegativeInteger(String),
    NegativeInteger(String),
    NonNegativeFloat(String),
    NegativeFloat(String),
    Bool(bool),
    List(ListPattern),
    Structure(StructurePattern),
    Or(OrPattern),
    Unit
}

impl HasSpan for Pattern {}

pub type RestPattern = Option<String>; 

#[derive(Clone)]
pub struct ListPattern {
    pub before_rest: Vec<Spanned<Pattern>>,
    pub after_rest: Vec<Spanned<Pattern>>,
    pub rest: Option<RestPattern>
}

#[derive(Clone)]
pub struct StructurePattern {
    pub fields: HashMap<String, Option<Spanned<Pattern>>>,
    pub rest: Option<RestPattern>
}

#[derive(Clone)]
pub struct OrPattern {
    pub lhs: Box<Spanned<Pattern>>,
    pub rhs: Box<Spanned<Pattern>>,
}

#[derive(Clone)]
pub struct ImportExpr {
    pub parts: Vec<String>
}

#[derive(Clone)]
pub struct AccessExpr {
    pub expr: Box<Spanned<Expr>>,
    pub name: Spanned<String>,
}

#[derive(Clone)]
pub struct ListExpr {
    pub exprs: Vec<Spanned<Expr>>
}

#[derive(Clone)]
pub struct StructureExpr {
    pub fields: Vec<(String, Spanned<Expr>)>
}

#[derive(Clone)]
pub struct AssignmentExpr {
    pub lhs: Box<Spanned<Expr>>,
    pub rhs: Box<Spanned<Expr>>
}

#[derive(Clone)]
pub struct ReturnExpr {
    pub expr: Box<Spanned<Expr>>
}
#
[derive(Clone)]
pub struct BreakExpr {
    pub expr: Box<Spanned<Expr>>
}

#[derive(Clone)]
pub struct ModuleExpr {
    pub definitions: Vec<(String, Spanned<Expr>)>
}

#[derive(Clone)]
pub struct WhileExpr {
    pub cond: Box<Spanned<Expr>>,
    pub body: Box<Spanned<Expr>>
}

#[derive(Clone)]
pub struct ForExpr {
    pub patt: Spanned<Pattern>,
    pub expr: Box<Spanned<Expr>>,
    pub body: Box<Spanned<Expr>>
}

#[derive(Clone)]
pub struct IfExpr {
    pub cond: Box<Spanned<Expr>>,
    pub truu: Box<Spanned<Expr>>,
    pub fals: Option<Box<Spanned<Expr>>>
}