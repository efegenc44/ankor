use std::collections::HashMap;

use crate::{
    expr::{
        SequenceExpr, ApplicationExpr, Expr, FunctionExpr, LetExpr,
        MatchExpr, Pattern, ImportExpr, AccessExpr, ListExpr, StructureExpr, 
        AssignmentExpr, ListPattern, StructurePattern, ReturnExpr, 
        ModuleExpr, WhileExpr, BreakExpr, ForExpr, IfExpr, OrPattern, RaiseExpr, TryHandleExpr
    },
    token::Token, span::{Spanned, Span, HasSpan}, error::Error,
};

macro_rules! binary_expr_precedence_level {
    ( $name:ident, $inferior:ident, $operators:pat, LEFT_ASSOC ) => {
        binary_expr_precedence_level!($name, $inferior, $operators, while, $inferior);
    };

    ( $name:ident, $inferior:ident, $operators:pat, NO_ASSOC ) => {
        binary_expr_precedence_level!($name, $inferior, $operators, if, $inferior);
    };

    ( $name:ident, $inferior:ident, $operators:pat, RIGHT_ASSOC ) => {
        binary_expr_precedence_level!($name, $inferior, $operators, if, $name);
    };

    ( $name:ident, $inferior:ident, $operators:pat, $loop:tt, $rhs:ident ) => {
        fn $name(&mut self) -> ParseResult<Spanned<Expr>> {
            let mut left = self.$inferior()?;
            $loop let current_token @ $operators = self.current_token() {
                let op = Expr::Identifier(current_token.to_string()).with_span(self.get_span());
                self.advance();
                let right = self.$rhs()?;
                let left_span = left.span; 
                left = Expr::Application(ApplicationExpr {
                    func: Box::new(op),
                    args: vec![left, right]
                })
                .start_end(left_span, self.get_previous_span())
            }
            Ok(left)
        }
    };

    ( $name:ident, $inferior:ident, $operators:pat, Intrinsic($t:ident, $expr:ident, $exprexpr:ident) ) => {
        fn $name(&mut self) -> ParseResult<Spanned<$t>> {
            let mut left = self.$inferior()?;
            while let $operators = self.current_token() {
                self.advance();
                let right = self.$inferior()?;
                let left_span = left.span;
                left = $t::$expr($exprexpr {
                    lhs: Box::new(left),
                    rhs: Box::new(right)
                })
                .start_end(left_span, self.get_previous_span())
            }
            Ok(left)
        }
    };
}

type ParseResult<T> = Result<T, Error>;

pub struct Parser {
    tokens: Vec<Spanned<Token>>,
    index: usize,

    in_function: usize,
    in_loop: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Self { tokens, index: 0, in_function: 0, in_loop: 0 }
    }

    fn current_token(&self) -> &Token {
        &self.tokens.get(self.index).unwrap().data
    }

    fn get_span(&self) -> Span {
        self.tokens.get(self.index).unwrap().span
    }

    fn get_previous_span(&self) -> Span {
        self.tokens.get(self.index - 1).unwrap().span
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn expect(&mut self, expected: Token) -> ParseResult<Span> {
        if self.current_token() != &expected {
            return Error::make(
                format!("Expected `{expected}`, instead found `{}`", self.current_token()), 
                self.get_span()
            )
        }
        let span = self.get_span();
        self.advance();
        Ok(span)
    }

    fn expect_identifier(&mut self) -> ParseResult<Spanned<String>> {
        let Token::Identifier(identifier) = self.current_token() else {
            return Error::make(
                format!("Expected an `Identifier`, instead found `{}`", self.current_token()), 
                self.get_span()
            )
        };
        let result = Spanned { data: identifier.clone(), span: self.get_span() };
        self.advance();
        Ok(result)
    }

    fn optional(&mut self, expected: Token) -> bool {
        if self.current_token() == &expected {
            self.advance();
            true
        } else {
            false
        }
    }

    fn product(&mut self) -> ParseResult<Spanned<Expr>> {
        use Token::*;

        let current_span = self.get_span(); 
        let expr = match self.current_token() {
            Identifier(symbol) => Expr::Identifier(symbol.clone()),
            String(string) => Expr::String(string.clone()),
            Integer(int) => Expr::Integer(int.clone()),
            Float(float) => Expr::Float(float.clone()),
            Ktrue => Expr::Bool(true),
            Kfalse => Expr::Bool(false),

            non_literal => return Ok((match non_literal {
                Klet => Expr::Let(self.let_expr()?),
                Kdef => Expr::Function(self.function_expr()?),
                Kmatch => Expr::Match(self.match_expr()?),
                Kimport => Expr::Import(self.import_expr()?),
                Kmodule => Expr::Module(self.module_expr()?),
                Kwhile => Expr::While(self.while_expr()?),
                Kfor => Expr::For(self.for_expr()?),
                Kif => Expr::If(self.if_expr()?),
                Kreturn => Expr::Return(self.return_expr()?),
                Kbreak => Expr::Break(self.break_expr()?),
                Kcontinue => self.continue_expr()?,
                Kraise => Expr::Raise(self.raise_expr()?),
                Ktry => Expr::TryHandle(self.tryhandle_expr()?),
                LSquare => Expr::List(self.list_expr()?),
                LCurly => Expr::Structure(self.structure_expr()?),
                Bang | Minus => {
                    let op = Expr::Identifier(non_literal.to_string());
                    self.advance();
                    Expr::Application(ApplicationExpr {
                        func: Box::new(op.with_span(current_span)),
                        args: vec![self.product()?]
                    })
                }
                LParen => {
                    self.advance();
                    if self.optional(RParen) {
                        Expr::UnitValue
                    } else if let Plus | Minus | Star | Slash | Less |
                           LessEqual | Greater | GreaterEqual |
                           DoubleEqual | BangEqual | Kand | Kor = self.current_token()
                    {
                        let op = Expr::Identifier(self.current_token().to_string());
                        self.advance();
                        self.expect(RParen)?;
                        op
                    } else {
                        let expr = self.expr()?;
                        self.expect(RParen)?;
                        expr.data
                    }
                }
                unknown => return Error::make(
                    format!("Unknown Start of an Expression: `{unknown}`"), 
                    self.get_span()
                )
            })
            .start_end(current_span, self.get_previous_span()))
        };
        self.advance();

        Ok(expr.with_span(current_span))
    }

    fn call_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        use Token::*;

        let mut expr = self.product()?;
        loop {
            match self.current_token() {
                LParen => {
                    let span = expr.span;
                    expr = Expr::Application(ApplicationExpr {
                        func: Box::new(expr),
                        args: self.parse_comma_seperated(LParen, RParen, Self::expr)?,
                    })
                    .start_end(span, self.get_previous_span())
                }
                
                Dot => {
                    self.advance();
                    let span = expr.span;
                    expr = Expr::Access(AccessExpr {
                        expr: Box::new(expr),
                        name: self.expect_identifier()?,
                    })
                    .start_end(span, self.get_previous_span())
                }

                _ => break
            }
        }
        Ok(expr)
    }

    binary_expr_precedence_level!(compose,    call_expr,  Token::Ko,                                RIGHT_ASSOC);
    binary_expr_precedence_level!(term,       compose,    Token::Star        | Token::Slash,        LEFT_ASSOC);
    binary_expr_precedence_level!(arithmetic, term,       Token::Plus        | Token::Minus,        LEFT_ASSOC);
    binary_expr_precedence_level!(comparison, arithmetic, Token::Less        | Token::LessEqual |
                                                          Token::Greater     | Token::GreaterEqual, NO_ASSOC);
    binary_expr_precedence_level!(equality,   comparison, Token::DoubleEqual | Token::BangEqual,    NO_ASSOC);
    binary_expr_precedence_level!(bool_and,   equality,   Token::Kand,                              LEFT_ASSOC);
    binary_expr_precedence_level!(bool_or,    bool_and,   Token::Kor,                               LEFT_ASSOC);
    binary_expr_precedence_level!(assignment, bool_or,    Token::Equal,     Intrinsic(Expr, Assignment, AssignmentExpr));
    binary_expr_precedence_level!(sequence,   assignment, Token::Semicolon, Intrinsic(Expr, Sequence,   SequenceExpr));

    fn let_expr(&mut self) -> ParseResult<LetExpr> {
        use Token::*;

        self.expect(Klet)?;
        let patt = self.pattern()?;
        self.expect(Equal)?;
        let vexp = Box::new(self.expr()?);
        self.expect(Kin)?;
        let expr = Box::new(self.expr()?);

        Ok(LetExpr { patt, vexp, expr })
    }

    fn function_expr(&mut self) -> ParseResult<FunctionExpr> {
        use Token::*;

        self.in_function += 1;
        
        self.expect(Kdef)?;
        let args = self.parse_comma_seperated(LParen, RParen, Self::pattern)?;
        let clos = match matches!(self.current_token(), Pipe) {
            true => Some(self.parse_comma_seperated(Pipe, Pipe, Self::expect_identifier)?),
            false => None,
        };
        self.expect(FatArrow)?;
        let expr = Box::new(self.expr()?);
    
        self.in_function -= 1;
        
        Ok(FunctionExpr { args, expr, clos })
    }

    fn import_expr(&mut self) -> ParseResult<ImportExpr> {
        use Token::*;

        self.expect(Kimport)?;
        let mut parts = vec![self.expect_identifier()?.data];
        while self.optional(Dot) {
            parts.push(self.expect_identifier()?.data)
        }

        Ok(ImportExpr { parts })
    }

    fn list_expr(&mut self) -> ParseResult<ListExpr> {
        use Token::*;

        let exprs = self.parse_comma_seperated(LSquare, RSquare, Self::expr)?;

        Ok(ListExpr { exprs })
    }

    fn module_expr(&mut self) -> ParseResult<ModuleExpr> {
        use Token::*;

        self.expect(Kmodule)?;
        self.expect(LParen)?;
        let definitions = self.parse_module()?;
        self.expect(RParen)?;

        Ok(ModuleExpr { definitions })
    }

    fn while_expr(&mut self) -> ParseResult<WhileExpr> {
        use Token::*;

        self.in_loop += 1;
        
        self.expect(Kwhile)?;
        let cond = Box::new(self.expr()?);
        self.expect(Kdo)?;
        let body = Box::new(self.expr()?);
        
        self.in_loop -= 1;
        
        Ok(WhileExpr { cond, body })
    }

    fn for_expr(&mut self) -> ParseResult<ForExpr> {
        use Token::*;

        self.in_loop += 1;
        
        self.expect(Kfor)?;
        let patt = self.pattern()?;
        self.expect(Kin)?;
        let expr = Box::new(self.expr()?);
        self.expect(Kdo)?;
        let body = Box::new(self.expr()?);
        
        self.in_loop -= 1;
        
        Ok(ForExpr { patt, expr, body })
    }

    fn if_expr(&mut self) -> ParseResult<IfExpr> {
        use Token::*;
        
        self.expect(Kif)?;
        let cond = Box::new(self.expr()?);
        self.expect(Kthen)?;
        let truu = Box::new(self.expr()?);
        let fals = match self.optional(Kelse) {
            true => Some(Box::new(self.expr()?)),
            false => None,
        };
        
        Ok(IfExpr { cond, truu, fals })
    }

    fn return_expr(&mut self) -> ParseResult<ReturnExpr> {
        use Token::*;

        if self.in_function == 0 {
            return Error::make(
                "Return Expression Outside of a Function", 
                self.get_span()
            )
        }

        self.expect(Kreturn)?;
        let expr = Box::new(self.expr()?);

        Ok(ReturnExpr { expr })
    }

    fn break_expr(&mut self) -> ParseResult<BreakExpr> {
        use Token::*;

        if self.in_loop == 0 {
            return Error::make(
                "Break Expression Outside of a Loop", 
                self.get_span()
            )
        }

        self.expect(Kbreak)?;
        let expr = Box::new(self.expr()?);

        Ok(BreakExpr { expr })
    }

    fn continue_expr(&mut self) -> ParseResult<Expr> {
        use Token::*;

        if self.in_loop == 0 {
            return Error::make(
                "Continue Expression Outside of a Loop", 
                self.get_span()
            )
        }

        self.expect(Kcontinue)?;

        Ok(Expr::Continue)
    }

    fn raise_expr(&mut self) -> ParseResult<RaiseExpr> {
        use Token::*;

        self.expect(Kraise)?;
        let expr = Box::new(self.expr()?);

        Ok(RaiseExpr { expr })
    }
    
    fn tryhandle_expr(&mut self) -> ParseResult<TryHandleExpr> {
        use Token::*;

        self.expect(Ktry)?;
        let expr = Box::new(self.expr()?);
        self.expect(Khandle)?;
        let hndl = Box::new(self.expr()?);

        Ok(TryHandleExpr { expr, hndl })
    }

    fn structure_expr(&mut self) -> ParseResult<StructureExpr> {
        use Token::*;

        let fields = self.parse_comma_seperated(LCurly, RCurly, Self::field)?;
        
        Ok(StructureExpr { fields })
    }

    fn field(&mut self) -> ParseResult<(String, Spanned<Expr>)> {
        use Token::*;

        let field_name = self.expect_identifier()?.data;
        self.expect(Colon)?;
        let expr = self.expr()?;
    
        Ok((field_name, expr))
    }

    fn match_expr(&mut self) -> ParseResult<MatchExpr> {
        use Token::*;

        self.expect(Kmatch)?;
        let expr = Box::new(self.expr()?);

        let mut arms = vec![self.match_arm()?];
        while let Pipe = self.current_token() {
            arms.push(self.match_arm()?)
        } 

        Ok(MatchExpr { expr, arms })
    }

    fn match_arm(&mut self) -> ParseResult<(Spanned<Pattern>, Spanned<Expr>)> {
        use Token::*;

        self.expect(Pipe)?;
        let pattern = self.pattern()?;
        self.expect(FatArrow)?;
        let expr = self.expr()?;  

        Ok((pattern, expr))
    }

    fn single_pattern(&mut self) -> ParseResult<Spanned<Pattern>> {
        use Token::*;

        let current_span = self.get_span();
        let pattern = match self.current_token() {
            Identifier(ident) => Pattern::Identifier(ident.clone()),
            String(string) => Pattern::String(string.clone()),
            Integer(int) => Pattern::NonNegativeInteger(int.clone()),
            Float(float) => Pattern::NonNegativeFloat(float.clone()),
            Ktrue => Pattern::Bool(true),
            Kfalse => Pattern::Bool(false),
            Minus => {
                self.advance();
                match self.current_token() {
                    Integer(int) => Pattern::NegativeInteger(int.clone()),
                    Float(float) => Pattern::NegativeFloat(float.clone()),
                    invalid => return Error::make(
                        format!("`-` Can Only Be Followed by Number Literals in Patterns, found: `{invalid}`"), 
                        self.get_span()
                    )
                }
            },

            non_literal => return Ok((match non_literal {
                LParen => {
                    self.advance();
                    if self.optional(RParen) {
                        Pattern::Unit
                    } else {
                        let pattern = self.pattern()?;
                        self.expect(RParen)?;
                        pattern.data
                    }
                }
                LSquare => Pattern::List(self.list_pattern()?),
                LCurly => Pattern::Structure(self.structure_pattern()?),
                unknown => return Error::make(
                    format!("Unknown Start of a Pattern: `{unknown}`"), 
                    self.get_span()
                )
            })
            .start_end(current_span, self.get_previous_span()))
        };
        self.advance();

        Ok(pattern.with_span(current_span))
    }

    binary_expr_precedence_level!(or_pattern, single_pattern, Token::Kor, Intrinsic(Pattern, Or, OrPattern));

    fn pattern(&mut self) -> ParseResult<Spanned<Pattern>> {
        self.or_pattern()
    }

    fn rest_pattern(&mut self) -> ParseResult<Option<String>> {
        use Token::*;

        self.expect(Dot)?;
        self.expect(Dot)?;
        Ok(match self.current_token() {
            Identifier(ident) => {
                let ident = ident.clone();
                self.advance();
                Some(ident)
            }
            _ => None,
        })
    }

    fn list_pattern(&mut self) -> ParseResult<ListPattern> {
        use Token::*;

        let mut before_rest = vec![];
        let mut after_rest = vec![];
        
        self.expect(LSquare)?;
        if !self.optional(RSquare) {
            match self.current_token() {
                Dot => {
                    let rest = Some(self.rest_pattern()?);
                    while !self.optional(RSquare) {
                        self.expect(Comma)?;
                        after_rest.push(self.pattern()?)
                    }
                    return Ok(ListPattern { before_rest, after_rest, rest })
                }
                _ => before_rest.push(self.pattern()?)
            }

            while !self.optional(RSquare) {
                self.expect(Comma)?;
                match self.current_token() {
                    Dot => {
                        let rest = Some(self.rest_pattern()?);
                        while !self.optional(RSquare) {
                            self.expect(Comma)?;
                            after_rest.push(self.pattern()?)
                        }
                        return Ok(ListPattern { before_rest, after_rest, rest })
                    },
                    _ => before_rest.push(self.pattern()?)
                }
            }
        }

        Ok(ListPattern { before_rest, after_rest, rest: None })
    }

    fn structure_pattern(&mut self) -> ParseResult<StructurePattern> {
        use Token::*;

        let mut fields = HashMap::new();
        
        self.expect(LCurly)?;
        if !self.optional(RCurly) {
            match self.current_token() {
                Dot => {
                    let rest = Some(self.rest_pattern()?);
                    self.expect(RCurly)?;
                    return Ok(StructurePattern { fields, rest })
                }
                _ => {
                    let (field_name, pattern) = self.field_pattern()?;
                    fields.insert(field_name.data, pattern);
                }
            }

            while !self.optional(RCurly) {
                self.expect(Comma)?;
                match self.current_token() {
                    Dot => {
                        let rest = Some(self.rest_pattern()?);
                        self.expect(RCurly)?;
                        return Ok(StructurePattern { fields, rest })
                    },
                    _ => {
                        let (field_name, pattern) = self.field_pattern()?;
                        fields.insert(field_name.data, pattern);
                    }
                }
            }
        }

        Ok(StructurePattern { fields, rest: None })
    }

    fn field_pattern(&mut self) -> ParseResult<(Spanned<String>, Option<Spanned<Pattern>>)> {
        use Token::*;

        let field_name = self.expect_identifier()?;
        let pattern = match self.optional(Colon) {
            true => Some(self.pattern()?),
            false => None,
        };
    
        Ok((field_name, pattern))
    }

    fn expr(&mut self) -> ParseResult<Spanned<Expr>> {
        self.sequence()
    }
    
    pub fn parse(&mut self) -> ParseResult<Spanned<Expr>> {
        let expr = self.expr();

        if self.index != self.tokens.len() - 1 {
            return Error::make(
                "Some of the Tokens Couldn't Be Consumed by the Parser", 
                self.get_span()
            )
        }
        expr
    }

    pub fn parse_module(&mut self) -> ParseResult<Vec<(String, Spanned<Expr>)>> {
        let mut module = vec![];
        while let Token::Identifier(name) = self.current_token() {
            let name = name.clone();
            self.advance();
            self.expect(Token::Equal)?;
            let expr = self.expr()?;
            module.push((name, expr))
        } 
        Ok(module)
    }

    fn parse_comma_seperated<T>(
        &mut self,
        open: Token,
        close: Token,
        f: fn(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<Vec<T>> {
        let mut values = vec![];
        self.expect(open)?;
        if !self.optional(close.clone()) {
            values.push(f(self)?);
            while !self.optional(close.clone()) {
                self.expect(Token::Comma)?;
                values.push(f(self)?);
            }
        }
        Ok(values)
    }
}
