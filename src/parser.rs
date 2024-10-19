#![allow(dead_code)]

use crate::ast::Ast;
use crate::ast::{
    format_ast_short, ArithmeticExpressionNode, ArithmeticOperationNode, AstRootNode,
    CompareExpressionNode, CompareOperationNode, ExpressionNode, FunctionArgumentNode,
    FunctionCallNode, FunctionDataNode, FunctionsChainNode, LambdaNode, LiteralNode,
    LogicExpressionNode, LogicOperationNode, TernaryOperatorNode,
};
use crate::lexer::Lexer;
use crate::lexer::{LexerError, Token};

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    Unexpected(String),
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

#[derive(Copy, Clone)]
enum Assoc {
    Left,
    Right,
}

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd)]
#[repr(u8)]
enum PrecedenceLevel {
    Lowest = 1,

    FunctionsChain = 10,
    Ternary = 20,
    Logic = 30,
    Compare = 40,
    Arithmetic = 50,
    Unary = 60,

    Strongest = 254,
}

impl PrecedenceLevel {
    fn to_int(self) -> u8 {
        self as u8
    }
    fn slightly_increased(&self) -> u8 {
        self.to_int() + 1
    }
    fn slightly_decreased(&self) -> u8 {
        self.to_int() - 1
    }
}

fn get_precedence(token: &Token) -> Option<PrecedenceLevel> {
    use Token::*;
    match token {
        Plus => Some(PrecedenceLevel::Arithmetic),
        Minus => Some(PrecedenceLevel::Arithmetic),
        Equal => Some(PrecedenceLevel::Compare),
        NotEqual => Some(PrecedenceLevel::Compare),
        Less => Some(PrecedenceLevel::Compare),
        LessEqual => Some(PrecedenceLevel::Compare),
        Greater => Some(PrecedenceLevel::Compare),
        GreaterEqual => Some(PrecedenceLevel::Compare),
        And => Some(PrecedenceLevel::Logic),
        Or => Some(PrecedenceLevel::Logic),
        Pipe => Some(PrecedenceLevel::FunctionsChain),
        Question => Some(PrecedenceLevel::Ternary),
        _ => None,
    }
}

fn get_precedence_level_assoc(operation: PrecedenceLevel) -> Assoc {
    use PrecedenceLevel::*;
    match operation {
        Ternary | Unary => Assoc::Right,
        _ => Assoc::Left,
    }
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
            expression: self.parse_expression(PrecedenceLevel::Lowest.to_int())?,
        })
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Box<ExpressionNode>, ParserError> {
        let mut left = self.parse_primary_expression()?;

        // Now, process infix/postfix operators like binary and ternary ones
        loop {
            let operation = self.lexer.next()?;

            let op_prec = get_precedence(&operation);
            if op_prec.is_none() {
                self.lexer.undo();
                break;
            }

            let op_prec = op_prec.unwrap();
            let assoc = get_precedence_level_assoc(op_prec);

            let l_prec = match assoc {
                Assoc::Left => op_prec.slightly_decreased(),
                Assoc::Right => op_prec.slightly_increased(),
            };

            // Check if we should stop based on precedence
            if l_prec < precedence {
                self.lexer.undo();
                break;
            }

            // Special case for the ternary operator `? :`
            if operation == Token::Question {
                let expr = self.parse_ternary_operator()?;
                left = Box::new(ExpressionNode::TernaryOperator(TernaryOperatorNode {
                    check_expression: left,
                    true_expression: expr.0,
                    false_expression: expr.1,
                }));
                continue; // move to the next expression after handling ternary
            }

            // Special case for pipe operator `|`
            if operation == Token::Pipe {
                let calls = self.parse_functions_chain()?;

                let data = match *left {
                    ExpressionNode::Literal(literal) => Ok(FunctionDataNode::Literal(literal)),
                    ExpressionNode::XValue => Ok(FunctionDataNode::XValue),
                    c => Err(ParserError::Unexpected(format!(
                        "Expected data for a functions chain, got {}",
                        format_ast_short(&c)
                    ))),
                }?;

                left = Box::new(ExpressionNode::FunctionsChain(FunctionsChainNode {
                    data,
                    function_calls: calls,
                }));
                continue; // move to the next expression after handling chain
            }

            let r_prec = match assoc {
                Assoc::Left => op_prec.slightly_increased(),
                Assoc::Right => op_prec.slightly_decreased(),
            };

            left = self.parse_infix(left, operation, r_prec)?;
        }

        Ok(left)
    }

    fn parse_infix(
        &mut self,
        left: Box<ExpressionNode>,
        operation: Token,
        precedence: u8,
    ) -> Result<Box<ExpressionNode>, ParserError> {
        use Token::*;
        match operation {
            Plus | Minus => {
                let op = match operation {
                    Plus => ArithmeticOperationNode::Plus,
                    Minus => ArithmeticOperationNode::Minus,
                    _ => unreachable!(),
                };
                Ok(Box::new(ExpressionNode::ArithmeticExpression(
                    ArithmeticExpressionNode {
                        l_expression: left,
                        operation: op,
                        r_expression: self.parse_expression(precedence)?,
                    },
                )))
            }
            And | Or => {
                let op = match operation {
                    And => LogicOperationNode::And,
                    Or => LogicOperationNode::Or,
                    _ => unreachable!(),
                };
                Ok(Box::new(ExpressionNode::LogicExpression(
                    LogicExpressionNode {
                        l_expression: left,
                        operation: op,
                        r_expression: self.parse_expression(precedence)?,
                    },
                )))
            }
            Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual => {
                let op = match operation {
                    Equal => CompareOperationNode::Eq,
                    NotEqual => CompareOperationNode::Neq,
                    Less => CompareOperationNode::Lt,
                    LessEqual => CompareOperationNode::Le,
                    Greater => CompareOperationNode::Gt,
                    GreaterEqual => CompareOperationNode::Ge,
                    _ => unreachable!(),
                };
                Ok(Box::new(ExpressionNode::CompareExpression(
                    CompareExpressionNode {
                        l_expression: left,
                        operation: op,
                        r_expression: self.parse_expression(precedence)?,
                    },
                )))
            }
            _ => Err(ParserError::Unexpected("Expected expression!".to_string())),
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Box<ExpressionNode>, ParserError> {
        let token = self.lexer.next()?;

        use Token::*;
        match &token {
            ArrayBracketBegin | StringLiteral(_) | CharLiteral(_) | IntLiteral(_)
            | BoolLiteral(_) => Ok(Box::new(ExpressionNode::Literal(
                self.parse_literal(token)?,
            ))),
            XValue => Ok(Box::new(ExpressionNode::XValue)),
            Identifier(id) => Ok(Box::new(ExpressionNode::ImplicitXValueOp(id.to_string()))),
            ParenthesisBegin => self.parse_parenthesis_expression(),
            Minus => Ok(Box::new(ExpressionNode::Negation(
                self.parse_expression(PrecedenceLevel::Unary.to_int())?,
            ))),
            t => Err(ParserError::Unexpected(format!(
                "Expected expression beginning, got {}",
                t
            ))),
        }
    }

    fn parse_parenthesis_expression(&mut self) -> Result<Box<ExpressionNode>, ParserError> {
        let expr = self.parse_expression(PrecedenceLevel::Lowest.to_int())?;

        let token = self.lexer.next()?;
        if token == Token::ParenthesisEnd {
            Ok(expr)
        } else {
            Err(ParserError::Unexpected(format!(
                "Expected ')', got {}",
                token
            )))
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
            _ => unreachable!(),
        }
    }

    fn parse_array(&mut self) -> Result<Vec<LiteralNode>, ParserError> {
        let mut node = vec![];

        loop {
            let token = self.lexer.next()?;

            use Token::*;
            let literal = match &token {
                ArrayBracketBegin | StringLiteral(_) | CharLiteral(_) | IntLiteral(_)
                | BoolLiteral(_) => Ok(self.parse_literal(token.clone())?),
                ArrayBracketEnd => return Ok(node),
                _ => Err(ParserError::Unexpected(format!(
                    "Expected array continuation, got {}",
                    token
                ))),
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
                return Err(ParserError::Unexpected(format!(
                    "Expected ',', got {}",
                    token
                )));
            }
        }
        Ok(node)
    }

    fn parse_ternary_operator(
        &mut self,
    ) -> Result<(Box<ExpressionNode>, Box<ExpressionNode>), ParserError> {
        let true_expression = self.parse_expression(PrecedenceLevel::Ternary.to_int())?;

        let token = self.lexer.next()?;
        if token != Token::Colon {
            return Err(ParserError::Unexpected(format!(
                "Expected ':', got {}",
                token
            )));
        }

        let false_expression = self.parse_expression(PrecedenceLevel::Ternary.to_int())?;

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
                _ => {
                    self.lexer.undo();
                    break;
                }
            };

            loop {
                let token = self.lexer.next()?;

                if token == Token::Pipe {
                    break;
                }

                use Token::*;
                let argument = match token {
                    LambdaBracketBegin => FunctionArgumentNode::Lambda(
                        LambdaNode::AnonymousLambda(self.parse_anonymous_lambda()?),
                    ),
                    Identifier(id) => {
                        FunctionArgumentNode::Lambda(LambdaNode::NamedLambda(id.to_string()))
                    }
                    ArrayBracketBegin | StringLiteral(_) | CharLiteral(_) | IntLiteral(_)
                    | BoolLiteral(_) | XValue | ParenthesisBegin | Minus => {
                        self.lexer.undo();
                        FunctionArgumentNode::Expression(
                            self.parse_expression(PrecedenceLevel::FunctionsChain.to_int())?,
                        )
                    }
                    Eof => break,
                    _ => {
                        self.lexer.undo();
                        break;
                    }
                };

                node.arguments.push(argument);
            }

            res.push(node)
        }
        Ok(res)
    }

    fn parse_anonymous_lambda(&mut self) -> Result<Box<ExpressionNode>, ParserError> {
        let expr = self.parse_expression(PrecedenceLevel::Lowest.to_int())?;

        let token = self.lexer.next()?;
        if token == Token::LambdaBracketEnd {
            Ok(expr)
        } else {
            Err(ParserError::Unexpected(format!(
                "Expected '}}', got {}",
                token
            )))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::format_ast_short;
    use std::fs::File;
    use std::io::Read;

    fn parse(code: &str) -> Ast {
        let mut parser = Parser::new(code);

        let ast = parser.parse();

        if let Err(e) = &ast {
            println!("Parser error is: {:?}", e);
        }

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
        let mut file = File::open("src/tests/ast/ast01.txt").unwrap();
        let mut contents = String::new();
        let _ = file.read_to_string(&mut contents);

        let splitted = contents
            .split("---")
            .map(str::trim)
            .filter(|x| x.len() > 0)
            .collect::<Vec<_>>();

        assert_eq!(splitted.len() % 2, 0);

        for i in (0..splitted.len()).step_by(2) {
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
