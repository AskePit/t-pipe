#![allow(dead_code)]

mod ast;
mod lexer;

use crate::parser::ast::{
    ArithmeticExpressionNode, ArrayNode, AstRootNode, CompareExpressionNode, ExpressionNode,
    LiteralNode, LogicExpressionNode, RightExpressionPart, TernaryOperatorNode,
};
use crate::parser::lexer::{LexerError, Token};
use ast::Ast;
use lexer::Lexer;

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
            expression: self.parse_expression()?,
        })
    }

    fn parse_expression(&mut self) -> Result<Box<ExpressionNode>, ParserError> {
        let left = self.parse_left_expression()?;
        let right = self.parse_right_expression()?;

        if let Some(r) = right {
            use RightExpressionPart::*;
            match *r {
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
                        true_expression: left,
                        false_expression: expr,
                    },
                ))),
            }
        } else {
            Ok(left)
        }
    }

    fn parse_left_expression(&mut self) -> Result<Box<ExpressionNode>, ParserError> {
        let token = self.lexer.next()?;

        use Token::*;
        match &token {
            ArrayBracketBegin | StringLiteral(_) | IntLiteral(_) | BoolLiteral(_) => Ok(Box::new(
                ExpressionNode::Literal(self.parse_literal(token)?),
            )),
            XValue => Ok(Box::new(ExpressionNode::XValue)),
            ParenthesisBegin => self.parse_parenthesis_expression(),
            _ => Err(ParserError::Unknown),
        }
    }

    fn parse_parenthesis_expression(&mut self) -> Result<Box<ExpressionNode>, ParserError> {
        let expr = self.parse_expression()?;

        let token = self.lexer.next()?;
        if token == Token::ParenthesisEnd {
            Ok(expr)
        } else {
            Err(ParserError::Unknown)
        }
    }

    fn parse_right_expression(&mut self) -> Result<Option<Box<RightExpressionPart>>, ParserError> {
        unimplemented!();
    }

    fn parse_literal(&mut self, first_token: Token) -> Result<LiteralNode, ParserError> {
        match first_token {
            Token::StringLiteral(s) => Ok(LiteralNode::String(s)),
            Token::CharLiteral(c) => Ok(LiteralNode::Char(c)),
            Token::IntLiteral(i) => Ok(LiteralNode::Int(i)),
            Token::BoolLiteral(b) => Ok(LiteralNode::Bool(b)),
            Token::ArrayBracketBegin => Ok(LiteralNode::Array(self.parse_array()?)),
            _ => Err(ParserError::Unknown),
        }
    }

    fn parse_array(&mut self) -> Result<ArrayNode, ParserError> {
        unimplemented!();
    }

    fn parse_literal_seq_tail(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_ternary_operator(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_compare_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_x_eq_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_compare_operation(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_logic_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_logic_operation(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_arithmetic_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_arithmetic_operation(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_functions_chain(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_functions_chain_start(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_functions_chain_rest(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_functions_chain_rest_tail(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_function_call(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_function_name(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_function_arguments(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_function_arguments_tail(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_function_argument(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_lambda(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_named_lambda(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_anonymous_lambda(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_lambda_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }

    fn parse_implicit_x_chain(&mut self) -> Result<(), ParserError> {
        unimplemented!();
    }
}
