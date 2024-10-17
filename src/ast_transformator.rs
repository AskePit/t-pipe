#![allow(dead_code)]

use crate::parser::ast::{
    ArithmeticOperationNode, CompareOperationNode, ExpressionNode, LiteralNode,
};

fn merge_negation(expr: Box<ExpressionNode>) -> Box<ExpressionNode> {
    if let ExpressionNode::Negation(expr) = expr.as_ref() {
        if let ExpressionNode::Literal(LiteralNode::Int(val)) = expr.as_ref() {
            return Box::new(ExpressionNode::Literal(LiteralNode::Int(-val)));
        }
    }
    expr
}

fn compile_time_int_calc(expr: Box<ExpressionNode>) -> Box<ExpressionNode> {
    if let ExpressionNode::ArithmeticExpression(expr) = expr.as_ref() {
        if let ExpressionNode::Literal(LiteralNode::Int(l_val)) = expr.l_expression.as_ref() {
            if let ExpressionNode::Literal(LiteralNode::Int(r_val)) = expr.r_expression.as_ref() {
                return Box::new(ExpressionNode::Literal(LiteralNode::Int(
                    match expr.operation {
                        ArithmeticOperationNode::Plus => l_val + r_val,
                        ArithmeticOperationNode::Minus => l_val - r_val,
                    },
                )));
            }
        }
    }
    expr
}

fn compile_time_int_compare(expr: Box<ExpressionNode>) -> Box<ExpressionNode> {
    if let ExpressionNode::CompareExpression(expr) = expr.as_ref() {
        if let ExpressionNode::Literal(LiteralNode::Int(l_val)) = expr.l_expression.as_ref() {
            if let ExpressionNode::Literal(LiteralNode::Int(r_val)) = expr.r_expression.as_ref() {
                return Box::new(ExpressionNode::Literal(LiteralNode::Bool(
                    match expr.operation {
                        CompareOperationNode::Eq => l_val == r_val,
                        CompareOperationNode::Neq => l_val != r_val,
                        CompareOperationNode::Lt => l_val < r_val,
                        CompareOperationNode::Le => l_val <= r_val,
                        CompareOperationNode::Gt => l_val > r_val,
                        CompareOperationNode::Ge => l_val >= r_val,
                    },
                )));
            }
        }
    }
    expr
}

fn compile_time_string_equality(src_expr: Box<ExpressionNode>) -> Box<ExpressionNode> {
    if let ExpressionNode::CompareExpression(expr) = src_expr.as_ref() {
        if let ExpressionNode::Literal(LiteralNode::String(l_val)) = expr.l_expression.as_ref() {
            if let ExpressionNode::Literal(LiteralNode::String(r_val)) = expr.r_expression.as_ref()
            {
                return match expr.operation {
                    CompareOperationNode::Eq => {
                        Box::new(ExpressionNode::Literal(LiteralNode::Bool(l_val == r_val)))
                    }
                    CompareOperationNode::Neq => {
                        Box::new(ExpressionNode::Literal(LiteralNode::Bool(l_val != r_val)))
                    }
                    _ => src_expr,
                };
            }
        }
    }
    src_expr
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{format_ast_short, Ast, AstRootNode};
    use crate::parser::Parser;

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
    fn test_transformations() {
        {
            let mut ast = parse("-15");

            assert_eq!(
                format_ast_short(&ast),
                r#"
Negation
    Expression
        Literal(15)
            "#
                .trim()
                .replace("    ", "\t")
            );

            ast.root.expression = merge_negation(ast.root.expression);

            assert_eq!(format_ast_short(&ast), "Literal(-15)");
        }

        {
            let mut ast = parse("-15 - 15");

            assert_eq!(
                format_ast_short(&ast),
                r#"
ArithmeticExpression
    Expression
        Negation
            Expression
                Literal(15)
    -
    Expression
        Literal(15)
            "#
                .trim()
                .replace("    ", "\t")
            );

            let mut arithm_node = match ast.root.expression.as_mut() {
                ExpressionNode::ArithmeticExpression(ref mut expr) => expr,
                _ => unreachable!(),
            };

            arithm_node.l_expression = merge_negation(arithm_node.l_expression);

            assert_eq!(
                format_ast_short(&ast),
                r#"
ArithmeticExpression
    Expression
        Literal(-15)
    -
    Expression
        Literal(15)
            "#
                .trim()
                .replace("    ", "\t")
            );
        }
    }
}
