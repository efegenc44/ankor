use crate::{
    expr::{SequenceExpr, ApplicationExpr, Expr, FunctionExpr, LetExpr},
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

        let mut expr = match self.current_token() {
            Identifier(symbol) => Expr::Identifier(symbol.clone()),
            Integer(int) => Expr::Integer(int.clone()),
            Klet => return Expr::Let(self.let_expr()),
            Kdef => return Expr::Function(self.function_expr()),
            LParen => {
                self.advance();
                return if self.optional(RParen) {
                    Expr::UnitValue
                } else {
                    let expr = self.expr();
                    self.expect(RParen);
                    expr
                }
            }
            _ => todo!("Error handling"),
        };
        self.advance();

        loop {
            match self.current_token() {
                LParen => {
                    expr = Expr::Application(ApplicationExpr {
                        func: Box::new(expr),
                        args: self.parse_comma_seperated(LParen, RParen, Self::expr),
                    })
                }
                _ => break,
            }
        }

        expr
    }

    binary_expr_precedence_level!(term,       product,    Token::Star,                LEFT_ASSOC);
    binary_expr_precedence_level!(arithmetic, term,       Token::Plus | Token::Minus, LEFT_ASSOC);
    binary_expr_precedence_level!(sequence,   arithmetic,                             SEQUENCE);

    fn let_expr(&mut self) -> LetExpr {
        use Token::*;

        self.expect(Klet);
        let name = self.expect_identifier();
        self.expect(Equal);
        let vexp = Box::new(self.expr());
        self.expect(Kin);
        let expr = Box::new(self.expr());

        LetExpr { name, vexp, expr }
    }

    fn function_expr(&mut self) -> FunctionExpr {
        use Token::*;

        self.expect(Kdef);
        let args = self.parse_comma_seperated(LParen, RParen, Self::expect_identifier);
        let clos = matches!(self.current_token(), Pipe)
            .then(|| self.parse_comma_seperated(Pipe, Pipe, Self::expect_identifier));
        self.expect(Equal);
        let expr = Box::new(self.expr());

        FunctionExpr { args, expr, clos }
    }

    pub fn expr(&mut self) -> Expr {
        self.sequence()
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
