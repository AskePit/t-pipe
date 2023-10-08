pub struct Ast {
    pub root: AstRootNode,
}

pub struct AstRootNode {
    pub expression: Box<ExpressionNode>,
}

pub enum ExpressionNode {
    Literal(LiteralNode),
    XValue,
    FunctionsChain(FunctionsChainNode),
    ArithmeticExpression(ArithmeticExpressionNode),
    LogicExpression(LogicExpressionNode),
    CompareExpression(CompareExpressionNode),
    TernaryOperator(TernaryOperatorNode),
}

pub enum LiteralNode {
    String(String),
    Char(char),
    Int(i32),
    Bool(bool),
    Array(ArrayNode),
}

pub struct ArrayNode {
    array: Vec<Box<LiteralNode>>,
}

pub struct FunctionsChainNode {
    data: Box<FunctionDataNode>,
    function_calls: Vec<Box<FunctionCallNode>>,
}

pub enum FunctionDataNode {
    Literal(LiteralNode),
    XValue,
}

pub struct FunctionCallNode {
    name: String,
    arguments: Vec<Box<FunctionArgumentNode>>,
}

pub enum FunctionArgumentNode {
    Expression(ExpressionNode),
    Lambda(LambdaNode),
}

pub enum LambdaNode {
    AnonymousLambda(ExpressionNode),
    NamedLambda(String),
}

pub struct ArithmeticExpressionNode {
    l_expression: Box<ExpressionNode>,
    operation: ArithmeticOperationNode,
    r_expression: Box<ExpressionNode>,
}

pub enum ArithmeticOperationNode {
    Plus,
    Minus,
}

pub struct LogicExpressionNode {
    l_expression: Box<ExpressionNode>,
    operation: LogicOperationNode,
    r_expression: Box<ExpressionNode>,
}

pub enum LogicOperationNode {
    And,
    Or,
}

pub struct CompareExpressionNode {
    l_expression: Box<ExpressionNode>,
    operation: Box<CompareOperationNode>,
    r_expression: Box<ExpressionNode>,
}

pub enum CompareOperationNode {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
}

pub struct TernaryOperatorNode {
    true_expression: Box<ExpressionNode>,
    false_expression: Box<ExpressionNode>,
}
