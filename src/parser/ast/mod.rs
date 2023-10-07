pub enum AstNode {
    Program(Expression),
    Expression(Expression),
    EasyToParseExpression(EasyToParseExpression),
    PostExpression,
    Literal(Literal),
    Array(LiteralSeq),
    LiteralSeqTail(LiteralSeqTail),
    TernaryOperator(TernaryOperator),
    CompareExpression(CompareExpression),
    XEqExpression(XEqExpression),
    CompareOperation(CompareOperation),
    LogicExpression(LogicExpression),
    LogicOperation(LogicOperation),
    ArithmeticExpression(ArithmeticExpression),
    ArithmeticOperation(ArithmeticOperation),
    FunctionsChain(FunctionsChain),
    FunctionsChainStart(FunctionsChainStart),
    FunctionsChainRest(FunctionsChainRest),
    FunctionsChainRestTail(FunctionsChainRestTail),
    FunctionCall(FunctionCall),
    FunctionName(String),
    FunctionArguments(FunctionArguments),
    FunctionArgumentsTail(FunctionArgumentsTail),
    FunctionArgument(FunctionArgument),
    Lambda(Lambda),
    NamedLambda(String),
    AnonymousLambda(LambdaExpression),
    LambdaExpression(LambdaExpression),
    ImplicitXChain(ImplicitXChain),
}

pub struct Expression {
    pre: Box<EasyToParseExpression>,
    post: Box<PostExpression>,
}

pub enum EasyToParseExpression {
    Literal(Literal),
    XValue,
    FunctionsChain(FunctionsChain),
    Expression(Expression),
}

pub enum PostExpression {
    ArithmeticExpression(ArithmeticExpression),
    LogicExpression(LogicExpression),
    CompareExpression(CompareExpression),
    TernaryOperator(TernaryOperator),
    Empty,
}

pub enum Literal {
    String(String),
    Char(char),
    Int(i32),
    Bool(bool),
    Array(LiteralSeq),
}

pub struct FunctionsChain {
    start: Box<FunctionsChainStart>,
    rest: Box<FunctionsChainRest>,
}

pub struct LiteralSeq {
    head: Box<Literal>,
    tail: Box<LiteralSeqTail>,
}

pub enum LiteralSeqTail {
    LiteralSeq(LiteralSeq),
    Empty,
}

pub enum FunctionsChainStart {
    Literal(Literal),
    XValue,
}

pub struct FunctionsChainRest {
    function_call: Box<FunctionCall>,
    chain_tail: Box<FunctionsChainRestTail>,
}

pub enum FunctionsChainRestTail {
    FunctionsChainRest(FunctionsChainRest),
    Empty,
}

pub struct FunctionCall {
    name: String,
    arguments: Box<FunctionArguments>,
}

pub struct FunctionArguments {
    head: Box<FunctionArgument>,
    tail: Box<FunctionArgumentsTail>,
}

pub enum FunctionArgument {
    Expression(Expression),
    Lambda(Lambda),
}

pub enum FunctionArgumentsTail {
    FunctionsChainRest(FunctionsChainRest),
    Empty,
}

pub enum Lambda {
    AnonymousLambda(LambdaExpression),
    NamedLambda(String),
}

pub enum LambdaExpression {
    Expression(Expression),
    ImplicitXChain(ImplicitXChain),
}

pub struct ImplicitXChain {
    function_call: Box<FunctionCall>,
    chain_tail: Box<FunctionsChainRestTail>,
}

pub struct ArithmeticExpression {
    operation: ArithmeticOperation,
    expression: Box<Expression>,
}

pub enum ArithmeticOperation {
    Plus,
    Minus,
}

pub struct LogicExpression {
    operation: LogicOperation,
    expression: Box<Expression>,
}

pub enum LogicOperation {
    And,
    Or,
}

pub enum CompareExpression {
    NormalCompareExpression(NormalCompareExpression),
    XEqExpression(XEqExpression),
}

pub struct NormalCompareExpression {
    operation: Box<CompareOperation>,
    expression: Box<Expression>,
}

pub enum XEqExpression {
    Literal(Literal),
    ArithmeticExpression(ArithmeticExpression),
}

pub enum CompareOperation {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
}

pub struct TernaryOperator {
    true_expression: Box<Expression>,
    false_expression: Box<Expression>,
}

pub struct Ast {
    pub root: Box<AstNode>,
}

impl Ast {}
