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
