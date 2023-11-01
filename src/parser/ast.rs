pub struct Ast {
    pub root: AstRootNode,
}

pub struct AstRootNode {
    pub expression: Box<ExpressionNode>,
}

#[derive(PartialEq, Debug)]
pub enum ExpressionNode {
    Literal(LiteralNode),
    XValue,
    FunctionsChain(FunctionsChainNode),
    ArithmeticExpression(ArithmeticExpressionNode),
    LogicExpression(LogicExpressionNode),
    CompareExpression(CompareExpressionNode),
    TernaryOperator(TernaryOperatorNode),
}

#[derive(PartialEq, Debug)]
pub enum RightExpressionPart {
    Arithmetic((ArithmeticOperationNode, Box<ExpressionNode>)),
    Logic((LogicOperationNode, Box<ExpressionNode>)),
    Compare((CompareOperationNode, Box<ExpressionNode>)),
    TernaryOperator((Box<ExpressionNode>, Box<ExpressionNode>)),
    FunctionsChain(Vec<FunctionCallNode>),
}

#[derive(PartialEq, Debug)]
pub enum LiteralNode {
    String(String),
    Char(char),
    Int(i32),
    Bool(bool),
    Array{array: Vec<Box<LiteralNode>>},
}

impl AstDisplay for LiteralNode {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            LiteralNode::String(s) => (vec![format!("Literal(\"{}\")", s)], vec![0]),
            LiteralNode::Char(c) => (vec![format!("Literal(\'{}\')", c)], vec![0]),
            LiteralNode::Int(i) => (vec![format!("Literal({})", i)], vec![0]),
            LiteralNode::Bool(b) => (vec![format!("Literal({})", b)], vec![0]),
            LiteralNode::Array{array} => {
                let info_iter = array.iter().map(|x| x.get_display_info());

                let mut content_lines: Vec<_> = info_iter.clone().flat_map(|x| x.0).collect();

                let mut content_levels: Vec<_> =
                    info_iter.flat_map(|x| x.1).map(|x| x + 1).collect();

                content_lines.insert(0, "Array".to_string());
                content_levels.insert(0, 0);

                (content_lines, content_levels)
            }
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct FunctionsChainNode {
    pub data: Box<FunctionDataNode>,
    pub function_calls: Vec<FunctionCallNode>,
}

#[derive(PartialEq, Debug)]
pub enum FunctionDataNode {
    Literal(LiteralNode),
    XValue,
}

impl AstDisplay for FunctionDataNode {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            FunctionDataNode::Literal(l) => l.get_display_info(),
            FunctionDataNode::XValue => (vec!["XValue".to_string()], vec![0])
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct FunctionCallNode {
    pub name: String,
    pub arguments: Vec<Box<FunctionArgumentNode>>,
}

#[derive(PartialEq, Debug)]
pub enum FunctionArgumentNode {
    Expression(Box<ExpressionNode>),
    Lambda(LambdaNode),
}

#[derive(PartialEq, Debug)]
pub enum LambdaNode {
    AnonymousLambda(Box<ExpressionNode>),
    NamedLambda(String),
}

#[derive(PartialEq, Debug)]
pub struct ArithmeticExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: ArithmeticOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

#[derive(PartialEq, Debug)]
pub enum ArithmeticOperationNode {
    Plus,
    Minus,
}

#[derive(PartialEq, Debug)]
pub struct LogicExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: LogicOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

#[derive(PartialEq, Debug)]
pub enum LogicOperationNode {
    And,
    Or,
}

#[derive(PartialEq, Debug)]
pub struct CompareExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: CompareOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

#[derive(PartialEq, Debug)]
pub enum CompareOperationNode {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(PartialEq, Debug)]
pub struct TernaryOperatorNode {
    pub check_expression: Box<ExpressionNode>,
    pub true_expression: Box<ExpressionNode>,
    pub false_expression: Box<ExpressionNode>,
}

pub type AstDisplayLevel = u32;

pub trait AstDisplay {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_display() {
        let node = LiteralNode::Array {
            array: vec![
                Box::new(LiteralNode::String("qwerty".to_string())),
                Box::new(LiteralNode::Char('4')),
                Box::new(LiteralNode::Array{
                    array: vec![Box::new(LiteralNode::Int(4))],
                }),
            ],
        };
        let (lines, levels) = node.get_display_info();

        println!("{:?}", lines);
        println!("{:?}", levels);
    }

    #[test]
    fn xvalue_display() {
        let node = FunctionDataNode::XValue;
        let (lines, levels) = node.get_display_info();

        println!("{:?}", lines);
        println!("{:?}", levels);
    }
}
