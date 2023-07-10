use crate::{
    expr::{
        SequenceExpr, ApplicationExpr, Expr, FunctionExpr, LetExpr,
        MatchExpr, Pattern, ImportExpr, AccessExpr, ListExpr
    },
    token::Token,
};

macro_rules! binary_expr_precedence_level {
    ( $name:ident, $inferior:ident, $operators:pat, LEFT_ASSOC ) => {
        fn $name(&mut self) -> Expr {
            let mut left = self.$inferior();
            while let current_token @ $operators = self.current_token() {
                let op = Expr::Identifier(current_token.to_string());
                self.advance();
                let right = self.$inferior();
                left = Expr::Application(ApplicationExpr {
                    func: Box::new(op),
                    args: vec![left, right]
                })
            }
            left
        }
    };
    
    ( $name:ident, $inferior:ident, $operators:pat, NO_ASSOC ) => {
        fn $name(&mut self) -> Expr {
            let mut left = self.$inferior();
            if let current_token @ $operators = self.current_token() {
                let op = Expr::Identifier(current_token.to_string());
                self.advance();
                let right = self.$inferior();
                left = Expr::Application(ApplicationExpr {
                    func: Box::new(op),
                    args: vec![left, right]
                })
            }
            left
        }
    };

    ( $name:ident, $inferior:ident, SEQUENCE ) => {
        fn $name(&mut self) -> Expr {
            let mut left = self.$inferior();
            while let Token::Semicolon = self.current_token() {
                self.advance();
                let right = self.$inferior();
                left = Expr::Sequence(SequenceExpr {
                    lhs: Box::new(left),
                    rhs: Box::new(right)
                })
            }
            left
        }
    };
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn current_token(&self) -> &Token {
        self.tokens.get(self.index).unwrap()
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn expect(&mut self, expected: Token) {
        if self.current_token() != &expected {
            todo!("Error handling")
        }
        self.advance();
    }

    fn expect_identifier(&mut self) -> String {
        let Token::Identifier(identifier) = self.current_token() else {
            todo!("Error handling")
        };
        let identifier = identifier.clone();
        self.advance();
        identifier
    }

    fn optional(&mut self, expected: Token) -> bool {
        if self.current_token() == &expected {
            self.advance();
            true
        } else {
            false
        }
    }

    fn product(&mut self) -> Expr {
        use Token::*;

        let expr = match self.current_token() {
            Identifier(symbol) => Expr::Identifier(symbol.clone()),
            Integer(int) => Expr::Integer(int.clone()),
            Ktrue => Expr::Bool(true),
            Kfalse => Expr::Bool(false),

            non_literal => return match non_literal {
                Klet => Expr::Let(self.let_expr()),
                Kdef => Expr::Function(self.function_expr()),
                Kmatch => Expr::Match(self.match_expr()),
                Kimport => Expr::Import(self.import_expr()),
                LSquare => Expr::List(self.list_expr()),
                Bang | Minus => {
                    let op = Expr::Identifier(non_literal.to_string());
                    self.advance();
                    Expr::Application(ApplicationExpr {
                        func: Box::new(op),
                        args: vec![self.product()]
                    })
                }
                LParen => {
                    self.advance();
                    if self.optional(RParen) {
                        Expr::UnitValue
                    } else {
                        let expr = self.expr();
                        self.expect(RParen);
                        expr
                    }
                }
                _ => todo!("Error handling"),
            }
        };
        self.advance();
        expr
    }

    fn call_expr(&mut self) -> Expr {
        use Token::*;

        let mut expr = self.product();
        loop {
            match self.current_token() {
                LParen => {
                    expr = Expr::Application(ApplicationExpr {
                        func: Box::new(expr),
                        args: self.parse_comma_seperated(LParen, RParen, Self::expr),
                    })
                }

                Dot => {
                    self.advance();
                    expr = Expr::Access(AccessExpr {
                        expr: Box::new(expr),
                        name: self.expect_identifier(),
                    })
                }

                _ => break
            }
        }
        expr
    }

    binary_expr_precedence_level!(term,       call_expr,  Token::Star,                               LEFT_ASSOC);
    binary_expr_precedence_level!(arithmetic, term,       Token::Plus        | Token::Minus,         LEFT_ASSOC);
    binary_expr_precedence_level!(comparison, arithmetic, Token::Less        | Token::LessEqual |
                                                          Token::Greater     | Token::GreaterEqual,  NO_ASSOC);
    binary_expr_precedence_level!(equality,   comparison, Token::DoubleEqual | Token::BangEqual,     NO_ASSOC);
    binary_expr_precedence_level!(sequence,   equality,                                              SEQUENCE);

    fn let_expr(&mut self) -> LetExpr {
        use Token::*;

        self.expect(Klet);
        let patt = self.pattern();
        self.expect(Equal);
        let vexp = Box::new(self.expr());
        self.expect(Kin);
        let expr = Box::new(self.expr());

        LetExpr { patt, vexp, expr }
    }

    fn function_expr(&mut self) -> FunctionExpr {
        use Token::*;

        self.expect(Kdef);
        let args = self.parse_comma_seperated(LParen, RParen, Self::expect_identifier);
        let clos = matches!(self.current_token(), Pipe)
            .then(|| self.parse_comma_seperated(Pipe, Pipe, Self::expect_identifier));
        self.expect(FatArrow);
        let expr = Box::new(self.expr());

        FunctionExpr { args, expr, clos }
    }

    fn import_expr(&mut self) -> ImportExpr {
        use Token::*;

        self.expect(Kimport);
        let mut parts = vec![self.expect_identifier()];
        while self.optional(Dot) {
            parts.push(self.expect_identifier())
        }

        ImportExpr { parts }
    }

    fn list_expr(&mut self) -> ListExpr {
        use Token::*;

        let exprs = self.parse_comma_seperated(LSquare, RSquare, Self::expr);

        ListExpr { exprs }
    }

    fn match_expr(&mut self) -> MatchExpr {
        use Token::*;

        self.expect(Kmatch);
        let expr = Box::new(self.expr());

        let mut arms = vec![self.match_arm()];
        while let Pipe = self.current_token() {
            arms.push(self.match_arm())
        } 

        MatchExpr { expr, arms }
    }

    fn match_arm(&mut self) -> (Pattern, Expr) {
        use Token::*;

        self.expect(Pipe);
        let pattern = self.pattern();
        self.expect(FatArrow);
        let expr = self.expr();  

        (pattern, expr)
    }

    fn pattern(&mut self) -> Pattern {
        use Token::*;

        let pattern = match self.current_token() {
            Identifier(ident) => Pattern::Identifier(ident.clone()),
            Integer(int) => Pattern::NonNegativeInteger(int.clone()),
            Ktrue => Pattern::Bool(true),
            Kfalse => Pattern::Bool(false),
            Minus => {
                self.advance();
                let Integer(int) = self.current_token() else {
                    todo!("Error handling")
                };
                Pattern::NegativeInteger(int.clone())
            },

            non_literal => return match non_literal {
                LSquare => Pattern::List(
                    self.parse_comma_seperated(LSquare, RSquare, Self::pattern)
                ),
                Dot => {
                    self.advance();
                    self.expect(Dot);
                    let ident = match self.current_token() {
                        Identifier(ident) => {
                            let ident = ident.clone();
                            self.advance();
                            Some(ident)
                        }
                        _ => None,
                    };
                    Pattern::Rest(ident)
                }
                LParen => {
                    self.advance();
                    if self.optional(RParen) {
                        Pattern::Unit
                    } else {
                        let pattern = self.pattern();
                        self.expect(RParen);
                        pattern
                    }
                }
                _ => todo!("Error handling")
            }
        };
        self.advance();
        pattern
    }

    fn expr(&mut self) -> Expr {
        self.sequence()
    }
    
    pub fn parse(&mut self) -> Expr {
        let expr = self.expr();

        if self.index != self.tokens.len() - 1 {
            todo!("Error handling")
        }
        expr
    }

    pub fn parse_module(&mut self) -> Vec<(String, Expr)> {
        let mut module = vec![];
        while let Token::Identifier(name) = self.current_token() {
            let name = name.clone();
            self.advance();
            self.expect(Token::Equal);
            let expr = self.expr();
            module.push((name, expr))
        } 
        module
    }

    fn parse_comma_seperated<T>(
        &mut self,
        open: Token,
        close: Token,
        f: fn(&mut Self) -> T,
    ) -> Vec<T> {
        let mut values = vec![];
        self.expect(open);
        if !self.optional(close.clone()) {
            values.push(f(self));
            while !self.optional(close.clone()) {
                self.expect(Token::Comma);
                values.push(f(self));
            }
        }
        values
    }
}
