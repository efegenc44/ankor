use std::collections::HashMap;

#[derive(Clone)]
pub enum Expr {
    Integer(String),
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
    UnitValue
}

#[derive(Clone)]
pub struct LetExpr {
    pub patt: Pattern,
    pub vexp: Box<Expr>,
    pub expr: Box<Expr>,
}

#[derive(Clone)]
pub struct FunctionExpr {
    pub args: Vec<String>,
    pub expr: Box<Expr>,
    pub clos: Option<Vec<String>>
}

#[derive(Clone)]
pub struct ApplicationExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Clone)]
pub struct SequenceExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone)]
pub struct MatchExpr {
    pub expr: Box<Expr>,
    pub arms: Vec<(Pattern, Expr)>
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Identifier(String),
    NonNegativeInteger(String),
    NegativeInteger(String),
    Bool(bool),
    List(Vec<Pattern>),
    Rest(Option<String>),
    Structure(HashMap<String, Option<Pattern>>, Option<Option<String>>),
    Unit
}

#[derive(Clone)]
pub struct ImportExpr {
    pub parts: Vec<String>
}

#[derive(Clone)]
pub struct AccessExpr {
    pub expr: Box<Expr>,
    pub name: String,
}

#[derive(Clone)]
pub struct ListExpr {
    pub exprs: Vec<Expr>
}

#[derive(Clone)]
pub struct StructureExpr {
    pub fields: Vec<(String, Expr)>
}

#[derive(Clone)]
pub struct AssignmentExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>
}
