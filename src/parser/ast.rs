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

pub enum RightExpressionPart {
    Arithmetic((ArithmeticOperationNode, Box<ExpressionNode>)),
    Logic((LogicOperationNode, Box<ExpressionNode>)),
    Compare((CompareOperationNode, Box<ExpressionNode>)),
    TernaryOperator(Box<ExpressionNode>),
    FunctionChain(Box<ExpressionNode>),
}

pub enum LiteralNode {
    String(String),
    Char(char),
    Int(i32),
    Bool(bool),
    Array(ArrayNode),
}

pub struct ArrayNode {
    pub array: Vec<Box<LiteralNode>>,
}

pub struct FunctionsChainNode {
    pub data: Box<FunctionDataNode>,
    pub function_calls: Vec<Box<FunctionCallNode>>,
}

pub enum FunctionDataNode {
    Literal(LiteralNode),
    XValue,
}

pub struct FunctionCallNode {
    pub name: String,
    pub arguments: Vec<Box<FunctionArgumentNode>>,
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
    pub l_expression: Box<ExpressionNode>,
    pub operation: ArithmeticOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

pub enum ArithmeticOperationNode {
    Plus,
    Minus,
}

pub struct LogicExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: LogicOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

pub enum LogicOperationNode {
    And,
    Or,
}

pub struct CompareExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: CompareOperationNode,
    pub r_expression: Box<ExpressionNode>,
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
    pub true_expression: Box<ExpressionNode>,
    pub false_expression: Box<ExpressionNode>,
}
