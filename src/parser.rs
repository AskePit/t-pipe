#![allow(dead_code)]

pub mod ast;
mod lexer;

use crate::parser::ast::{AstNode, Literal};
use crate::parser::lexer::{LexerError, Token};
use crate::parser::ParserError::Unknown;
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

    pub fn parse(&mut self) -> Ast {
        Ast {
            root: Box::new(AstNode::Literal(Literal::String("root".to_string()))),
        };
        unimplemented!();
    }

    fn parse_program(&mut self) -> Result<(), ParserError> {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Result<(), ParserError> {
        self.parse_easy_to_parse_expression()?;
        self.parse_post_expression()?;
        Ok(())
    }

    fn parse_easy_to_parse_expression(&mut self) -> Result<(), ParserError> {
        let token = self.lexer.next()?;
        match token {
            Token::ArrayBracketBegin => self.parse_array(),
            Token::StringLiteral(s) => {
                unimplemented!();
                Ok(())
            }
            Token::CharLiteral(c) => {
                unimplemented!();
                Ok(())
            }
            Token::IntLiteral(i) => {
                unimplemented!();
                Ok(())
            }
            Token::BoolLiteral(b) => {
                unimplemented!();
                Ok(())
            }
            _ => Err(Unknown),
        }
    }

    fn parse_post_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_literal(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_array(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_literal_seq_tail(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_ternary_operator(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_compare_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_x_eq_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_compare_operation(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_logic_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_logic_operation(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_arithmetic_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_arithmetic_operation(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_functions_chain(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_functions_chain_start(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_functions_chain_rest(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_functions_chain_rest_tail(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_function_call(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_function_name(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_function_arguments(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_function_arguments_tail(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_function_argument(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_lambda(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_named_lambda(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_anonymous_lambda(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_lambda_expression(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }

    fn parse_implicit_x_chain(&mut self) -> Result<(), ParserError> {
        unimplemented!();
        Ok(())
    }
}
