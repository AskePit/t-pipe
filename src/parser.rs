#![allow(dead_code)]

mod ast;
mod lexer;

use crate::parser::ast::{
    ArithmeticExpressionNode, ArithmeticOperationNode, AstRootNode, CompareExpressionNode,
    CompareOperationNode, ExpressionNode, FunctionArgumentNode, FunctionCallNode, FunctionDataNode,
    FunctionsChainNode, LambdaNode, LiteralNode, LogicExpressionNode, LogicOperationNode,
    RightExpressionPart, TernaryOperatorNode,
};
use crate::parser::lexer::{LexerError, Token};
use ast::Ast;
use lexer::Lexer;

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    Unknown,
}

impl From<LexerError> for ParserError {
    fn from(value: LexerError) -> Self {
        ParserError::LexerError(value)
    }
}

pub struct Parser<'input> {
    lexer: Lexer<'input>,
}

impl<'input> Parser<'input> {
    pub fn new(code: &'input str) -> Parser {
        Parser {
            lexer: Lexer::new(code),
        }
    }

    pub fn parse(&mut self) -> Result<Ast, ParserError> {
        Ok(Ast {
            root: self.parse_program()?,
        })
    }

    fn parse_program(&mut self) -> Result<AstRootNode, ParserError> {
        Ok(AstRootNode {
            expression: self.parse_expression(None)?,
        })
    }

    fn parse_expression(
        &mut self,
        first_token: Option<Token>,
    ) -> Result<Box<ExpressionNode>, ParserError> {
        let left = self.parse_left_expression(first_token)?;
        let right = self.parse_right_expression()?;

        if let Some(r) = right {
            use RightExpressionPart::*;
            match r {
                Arithmetic((op, expr)) => Ok(Box::new(ExpressionNode::ArithmeticExpression(
                    ArithmeticExpressionNode {
                        l_expression: left,
                        operation: op,
                        r_expression: expr,
                    },
                ))),
                Logic((op, expr)) => Ok(Box::new(ExpressionNode::LogicExpression(
                    LogicExpressionNode {
                        l_expression: left,
                        operation: op,
                        r_expression: expr,
                    },
                ))),
                Compare((op, expr)) => Ok(Box::new(ExpressionNode::CompareExpression(
                    CompareExpressionNode {
                        l_expression: left,
                        operation: op,
                        r_expression: expr,
                    },
                ))),
                TernaryOperator(expr) => Ok(Box::new(ExpressionNode::TernaryOperator(
                    TernaryOperatorNode {
                        check_expression: left,
                        true_expression: expr.0,
                        false_expression: expr.1,
                    },
                ))),
                FunctionsChain(calls) => {
                    let data = match *left {
                        ExpressionNode::Literal(literal) => {
                            Ok(Box::new(FunctionDataNode::Literal(literal)))
                        }
                        ExpressionNode::XValue => Ok(Box::new(FunctionDataNode::XValue)),
                        _ => Err(ParserError::Unknown),
                    }?;

                    Ok(Box::new(ExpressionNode::FunctionsChain(
                        FunctionsChainNode {
                            data,
                            function_calls: calls,
                        },
                    )))
                }
            }
        } else {
            Ok(left)
        }
    }

    fn parse_left_expression(
        &mut self,
        first_token: Option<Token>,
    ) -> Result<Box<ExpressionNode>, ParserError> {
        let token = if let Some(t) = first_token {
            t
        } else {
            self.lexer.next()?
        };

        use Token::*;
        match &token {
            ArrayBracketBegin | StringLiteral(_) | CharLiteral(_) | IntLiteral(_)
            | BoolLiteral(_) => Ok(Box::new(ExpressionNode::Literal(
                self.parse_literal(token)?,
            ))),
            XValue => Ok(Box::new(ExpressionNode::XValue)),
            ParenthesisBegin => self.parse_parenthesis_expression(),
            _ => Err(ParserError::Unknown),
        }
    }

    fn parse_parenthesis_expression(&mut self) -> Result<Box<ExpressionNode>, ParserError> {
        let expr = self.parse_expression(None)?;

        let token = self.lexer.next()?;
        if token == Token::ParenthesisEnd {
            Ok(expr)
        } else {
            Err(ParserError::Unknown)
        }
    }

    fn parse_right_expression(&mut self) -> Result<Option<RightExpressionPart>, ParserError> {
        let token = self.lexer.next()?;

        use Token::*;
        match token {
            Plus | Minus => {
                let op = match token {
                    Plus => Ok(ArithmeticOperationNode::Plus),
                    Minus => Ok(ArithmeticOperationNode::Minus),
                    _ => Err(ParserError::Unknown),
                }?;
                Ok(Some(RightExpressionPart::Arithmetic((
                    op,
                    self.parse_expression(None)?,
                ))))
            }
            And | Or => {
                let op = match token {
                    And => Ok(LogicOperationNode::And),
                    Or => Ok(LogicOperationNode::Or),
                    _ => Err(ParserError::Unknown),
                }?;
                Ok(Some(RightExpressionPart::Logic((
                    op,
                    self.parse_expression(None)?,
                ))))
            }
            Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual => {
                let op = match token {
                    Equal => Ok(CompareOperationNode::Eq),
                    NotEqual => Ok(CompareOperationNode::Neq),
                    Less => Ok(CompareOperationNode::Lt),
                    LessEqual => Ok(CompareOperationNode::Le),
                    Greater => Ok(CompareOperationNode::Gt),
                    GreaterEqual => Ok(CompareOperationNode::Ge),
                    _ => Err(ParserError::Unknown),
                }?;
                Ok(Some(RightExpressionPart::Compare((
                    op,
                    self.parse_expression(None)?,
                ))))
            }
            Question => Ok(Some(RightExpressionPart::TernaryOperator(
                self.parse_ternary_operator()?,
            ))),
            Pipe => Ok(Some(RightExpressionPart::FunctionsChain(
                self.parse_functions_chain()?,
            ))),
            _ => Ok(None),
        }
    }

    fn parse_literal(&mut self, first_token: Token) -> Result<LiteralNode, ParserError> {
        match first_token {
            Token::StringLiteral(s) => Ok(LiteralNode::String(s)),
            Token::CharLiteral(c) => Ok(LiteralNode::Char(c)),
            Token::IntLiteral(i) => Ok(LiteralNode::Int(i)),
            Token::BoolLiteral(b) => Ok(LiteralNode::Bool(b)),
            Token::ArrayBracketBegin => Ok(LiteralNode::Array {
                array: self.parse_array()?,
            }),
            _ => Err(ParserError::Unknown),
        }
    }

    fn parse_array(&mut self) -> Result<Vec<Box<LiteralNode>>, ParserError> {
        let mut node = vec![];

        loop {
            let token = self.lexer.next()?;

            use Token::*;
            let literal = match &token {
                ArrayBracketBegin | StringLiteral(_) | CharLiteral(_) | IntLiteral(_)
                | BoolLiteral(_) => Ok(Box::new(self.parse_literal(token.clone())?)),
                ArrayBracketEnd => return Ok(node),
                _ => Err(ParserError::Unknown),
            };

            if let Ok(l) = literal {
                node.push(l);
            } else {
                break;
            }

            let token = self.lexer.next()?;

            if token == ArrayBracketEnd {
                return Ok(node);
            }

            if token != Comma {
                return Err(ParserError::Unknown);
            }
        }
        Ok(node)
    }

    fn parse_ternary_operator(
        &mut self,
    ) -> Result<(Box<ExpressionNode>, Box<ExpressionNode>), ParserError> {
        let true_expression = self.parse_expression(None)?;

        let token = self.lexer.next()?;
        if token != Token::Colon {
            return Err(ParserError::Unknown);
        }

        let false_expression = self.parse_expression(None)?;

        Ok((true_expression, false_expression))
    }

    fn parse_functions_chain(&mut self) -> Result<Vec<FunctionCallNode>, ParserError> {
        let mut res: Vec<FunctionCallNode> = vec![];

        loop {
            let mut node = FunctionCallNode {
                name: "".to_string(),
                arguments: vec![],
            };

            let token = self.lexer.next()?;
            match &token {
                Token::Identifier(name) => node.name = name.to_string(),
                _ => break,
            };

            loop {
                let token = self.lexer.next()?;

                if token == Token::Pipe {
                    break;
                }

                let argument = match token {
                    Token::LambdaBracketBegin => Box::new(FunctionArgumentNode::Lambda(
                        LambdaNode::AnonymousLambda(self.parse_anonymous_lambda()?),
                    )),
                    Token::Identifier(id) => Box::new(FunctionArgumentNode::Lambda(
                        LambdaNode::NamedLambda(id.to_string()),
                    )),
                    Token::Eof => break,
                    _ => Box::new(FunctionArgumentNode::Expression(
                        self.parse_expression(Some(token))?,
                    )),
                };

                node.arguments.push(argument);
            }

            res.push(node)
        }
        Ok(res)
    }

    fn parse_anonymous_lambda(&mut self) -> Result<Box<ExpressionNode>, ParserError> {
        let expr = self.parse_expression(None)?;

        let token = self.lexer.next()?;
        if token == Token::LambdaBracketEnd {
            Ok(expr)
        } else {
            Err(ParserError::Unknown)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::format_ast_short;
    use std::fs::File;
    use std::io::Read;

    fn parse(code: &str) -> Ast {
        let mut parser = Parser::new(code);

        let ast = parser.parse();
        assert!(ast.is_ok());

        let ast = ast.unwrap();
        ast
    }

    #[test]
    fn parse_the_simplest() {
        {
            let ast = parse("2");
            assert_eq!(format_ast_short(&ast), "Literal(2)");
        }
        {
            let ast = parse("\"1561\"");
            assert_eq!(format_ast_short(&ast), r#"Literal("1561")"#);
        }
        {
            let ast = parse(r#" x + "th" "#);
            assert_eq!(
                format_ast_short(&ast),
                r#"
ArithmeticExpression
    Expression
        XValue
    +
    Expression
        Literal("th")
                "#
                .trim()
                .replace("    ", "\t")
            );
        }
        {
            let ast = parse(r#""qwerty" | drop 2"#);
            assert_eq!(
                format_ast_short(&ast),
                r#"
FunctionsChain
    FunctionData
        Literal("qwerty")
    FunctionCall
        "drop"
        FunctionArgument
            Expression
                Literal(2)
                "#
                .trim()
                .replace("    ", "\t")
            );
        }
    }

    #[test]
    fn parse_from_file() {
        let mut file = File::open("src/parser/tests/ast/ast01.txt").unwrap();
        let mut contents = String::new();
        let _ = file.read_to_string(&mut contents);

        let splitted = contents
            .split("---")
            .map(str::trim)
            .filter(|x| x.len() > 0)
            .collect::<Vec<_>>();

        assert_eq!(splitted.len() % 2, 0);

        for i in (0..splitted.len() / 2).step_by(2) {
            let code = splitted[i];
            println!("{}", code);
            let ast = parse(code);

            let ast_str_expected = splitted[i + 1].replace("\r\n", "\n");
            let ast_str_real = format_ast_short(&ast);

            assert_eq!(ast_str_real, ast_str_expected);

            println!("ok");
        }
    }
}
