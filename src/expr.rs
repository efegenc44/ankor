#[derive(Clone)]
pub enum Expr {
    Integer(String),
    Identifier(String),
    Let(LetExpr),
    Function(FunctionExpr),
    Application(ApplicationExpr),
    Sequence(SequenceExpr),
    UnitValue
}

#[derive(Clone)]
pub struct LetExpr {
    pub name: String,
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
