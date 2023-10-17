use serde::Serialize;

#[derive(Serialize)]
pub struct Ast {
    pub root: AstRootNode,
}

#[derive(Serialize)]
pub struct AstRootNode {
    pub expression: Box<ExpressionNode>,
}

#[derive(PartialEq, Debug, Serialize)]
pub enum ExpressionNode {
    Literal(LiteralNode),
    XValue,
    FunctionsChain(FunctionsChainNode),
    ArithmeticExpression(ArithmeticExpressionNode),
    LogicExpression(LogicExpressionNode),
    CompareExpression(CompareExpressionNode),
    TernaryOperator(TernaryOperatorNode),
}

#[derive(PartialEq, Debug, Serialize)]
pub enum RightExpressionPart {
    Arithmetic((ArithmeticOperationNode, Box<ExpressionNode>)),
    Logic((LogicOperationNode, Box<ExpressionNode>)),
    Compare((CompareOperationNode, Box<ExpressionNode>)),
    TernaryOperator((Box<ExpressionNode>, Box<ExpressionNode>)),
    FunctionsChain(Vec<FunctionCallNode>),
}

#[derive(PartialEq, Debug, Serialize)]
pub enum LiteralNode {
    String(String),
    Char(char),
    Int(i32),
    Bool(bool),
    Array(ArrayNode),
}

#[derive(PartialEq, Debug, Serialize)]
pub struct ArrayNode {
    pub array: Vec<Box<LiteralNode>>,
}

#[derive(PartialEq, Debug, Serialize)]
pub struct FunctionsChainNode {
    pub data: Box<FunctionDataNode>,
    pub function_calls: Vec<FunctionCallNode>,
}

#[derive(PartialEq, Debug, Serialize)]
pub enum FunctionDataNode {
    Literal(LiteralNode),
    XValue,
}

#[derive(PartialEq, Debug, Serialize)]
pub struct FunctionCallNode {
    pub name: String,
    pub arguments: Vec<Box<FunctionArgumentNode>>,
}

#[derive(PartialEq, Debug, Serialize)]
pub enum FunctionArgumentNode {
    Expression(Box<ExpressionNode>),
    Lambda(LambdaNode),
}

#[derive(PartialEq, Debug, Serialize)]
pub enum LambdaNode {
    AnonymousLambda(Box<ExpressionNode>),
    NamedLambda(String),
}

#[derive(PartialEq, Debug, Serialize)]
pub struct ArithmeticExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: ArithmeticOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

#[derive(PartialEq, Debug, Serialize)]
pub enum ArithmeticOperationNode {
    Plus,
    Minus,
}

#[derive(PartialEq, Debug, Serialize)]
pub struct LogicExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: LogicOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

#[derive(PartialEq, Debug, Serialize)]
pub enum LogicOperationNode {
    And,
    Or,
}

#[derive(PartialEq, Debug, Serialize)]
pub struct CompareExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: CompareOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

#[derive(PartialEq, Debug, Serialize)]
pub enum CompareOperationNode {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(PartialEq, Debug, Serialize)]
pub struct TernaryOperatorNode {
    pub check_expression: Box<ExpressionNode>,
    pub true_expression: Box<ExpressionNode>,
    pub false_expression: Box<ExpressionNode>,
}
