use std::collections::HashMap;

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

#[derive(Clone)]
pub struct LetExpr {
    pub patt: Pattern,
    pub vexp: Box<Expr>,
    pub expr: Box<Expr>,
}

#[derive(Clone)]
pub struct FunctionExpr {
    pub args: Vec<Pattern>,
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

pub type RestPattern = Option<String>; 

#[derive(Clone, Debug)]
pub struct ListPattern {
    pub before_rest: Vec<Pattern>,
    pub after_rest: Vec<Pattern>,
    pub rest: Option<RestPattern>
}

#[derive(Clone, Debug)]
pub struct StructurePattern {
    pub fields: HashMap<String, Option<Pattern>>,
    pub rest: Option<RestPattern>
}

#[derive(Clone, Debug)]
pub struct OrPattern {
    pub lhs: Box<Pattern>,
    pub rhs: Box<Pattern>,
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

#[derive(Clone)]
pub struct ReturnExpr {
    pub expr: Box<Expr>
}
#
[derive(Clone)]
pub struct BreakExpr {
    pub expr: Box<Expr>
}

#[derive(Clone)]
pub struct ModuleExpr {
    pub definitions: Vec<(String, Expr)>
}

#[derive(Clone)]
pub struct WhileExpr {
    pub cond: Box<Expr>,
    pub body: Box<Expr>
}

#[derive(Clone)]
pub struct ForExpr {
    pub patt: Pattern,
    pub expr: Box<Expr>,
    pub body: Box<Expr>
}

#[derive(Clone)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub truu: Box<Expr>,
    pub fals: Option<Box<Expr>>
}