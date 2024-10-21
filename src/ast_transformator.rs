#![allow(dead_code)]

use crate::ast::{ArithmeticOperationNode, Ast, CompareOperationNode, ExpressionNode, LiteralNode};

fn merge_negation(src_expr: &mut Box<ExpressionNode>) {
    if let ExpressionNode::Negation(expr) = src_expr.as_mut() {
        if let ExpressionNode::Literal(LiteralNode::Int(val)) = expr.as_mut() {
            *src_expr = Box::new(ExpressionNode::Literal(LiteralNode::Int(-*val)));
        }
    }
}

fn compile_time_int_calc(src_expr: &mut Box<ExpressionNode>) {
    if let ExpressionNode::ArithmeticExpression(expr) = src_expr.as_ref() {
        if let ExpressionNode::Literal(LiteralNode::Int(l_val)) = expr.l_expression.as_ref() {
            if let ExpressionNode::Literal(LiteralNode::Int(r_val)) = expr.r_expression.as_ref() {
                *src_expr = Box::new(ExpressionNode::Literal(LiteralNode::Int(
                    match expr.operation {
                        ArithmeticOperationNode::Plus => l_val + r_val,
                        ArithmeticOperationNode::Minus => l_val - r_val,
                    },
                )));
            }
        }
    }
}

fn compile_time_int_compare(src_expr: &mut Box<ExpressionNode>) {
    if let ExpressionNode::CompareExpression(expr) = src_expr.as_ref() {
        if let ExpressionNode::Literal(LiteralNode::Int(l_val)) = expr.l_expression.as_ref() {
            if let ExpressionNode::Literal(LiteralNode::Int(r_val)) = expr.r_expression.as_ref() {
                *src_expr = Box::new(ExpressionNode::Literal(LiteralNode::Bool(
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
}

fn compile_time_string_equality(src_expr: &mut Box<ExpressionNode>) {
    if let ExpressionNode::CompareExpression(expr) = src_expr.as_ref() {
        if let ExpressionNode::Literal(LiteralNode::String(l_val)) = expr.l_expression.as_ref() {
            if let ExpressionNode::Literal(LiteralNode::String(r_val)) = expr.r_expression.as_ref()
            {
                match expr.operation {
                    CompareOperationNode::Eq => {
                        *src_expr =
                            Box::new(ExpressionNode::Literal(LiteralNode::Bool(l_val == r_val)))
                    }
                    CompareOperationNode::Neq => {
                        *src_expr =
                            Box::new(ExpressionNode::Literal(LiteralNode::Bool(l_val != r_val)))
                    }
                    _ => {}
                };
            }
        }
    }
}

pub fn transform_ast(ast: &mut Ast) {
    todo!();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::format_ast_short;
    use crate::test_utils::tests::parse;

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

            merge_negation(&mut ast.root.expression);

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

            let arithm_node = match ast.root.expression.as_mut() {
                ExpressionNode::ArithmeticExpression(ref mut expr) => expr,
                _ => unreachable!(),
            };

            merge_negation(&mut arithm_node.l_expression);

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
