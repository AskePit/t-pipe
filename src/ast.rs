#![allow(dead_code)]

pub struct Ast {
    pub root: AstRootNode,
}

impl AstDisplay for Ast {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        self.root.get_display_info()
    }
}

pub struct AstRootNode {
    pub expression: Box<ExpressionNode>,
}

impl AstDisplay for AstRootNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        self.expression.get_display_info()
    }
}

#[derive(PartialEq, Debug, Default)]
pub enum ExpressionNode {
    Literal(LiteralNode),
    #[default]
    XValue,
    ImplicitXValueOp(String),
    FunctionsChain(FunctionsChainNode),
    Negation(Box<ExpressionNode>),
    ArithmeticExpression(ArithmeticExpressionNode),
    LogicExpression(LogicExpressionNode),
    CompareExpression(CompareExpressionNode),
    TernaryOperator(TernaryOperatorNode),
}

impl AstDisplay for ExpressionNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            ExpressionNode::Literal(l) => l.get_display_info(),
            ExpressionNode::XValue => (vec!["XValue".into()], vec![0]),
            ExpressionNode::ImplicitXValueOp(id) => (vec![format!("Id(\"{}\")", &id)], vec![0]),
            ExpressionNode::FunctionsChain(ch) => ch.get_display_info(),
            ExpressionNode::Negation(e) => {
                let mut info = e.get_display_info();

                for l in &mut info.1 {
                    *l += 1;
                }

                info.0.insert(0, "Negation".to_string());
                info.1.insert(0, 0);

                info
            }
            ExpressionNode::ArithmeticExpression(e) => e.get_display_info(),
            ExpressionNode::LogicExpression(e) => e.get_display_info(),
            ExpressionNode::CompareExpression(e) => e.get_display_info(),
            ExpressionNode::TernaryOperator(op) => op.get_display_info(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum LiteralNode {
    String(String),
    Char(char),
    Int(i32),
    Bool(bool),
    Array { array: Vec<LiteralNode> },
}

impl AstDisplay for LiteralNode {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            LiteralNode::String(s) => (vec![format!("Literal(\"{}\")", s)], vec![0]),
            LiteralNode::Char(c) => (vec![format!("Literal(\'{}\')", c)], vec![0]),
            LiteralNode::Int(i) => (vec![format!("Literal({})", i)], vec![0]),
            LiteralNode::Bool(b) => (vec![format!("Literal({})", b)], vec![0]),
            LiteralNode::Array { array } => {
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
    pub data: FunctionDataNode,
    pub function_calls: Vec<FunctionCallNode>,
}

impl AstDisplay for FunctionsChainNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        let mut res = self.data.get_display_info();

        for call in &self.function_calls {
            let mut call_res = call.get_display_info();
            res.0.append(&mut call_res.0);
            res.1.append(&mut call_res.1);
        }

        res
    }
}

#[derive(PartialEq, Debug)]
pub enum FunctionDataNode {
    Literal(LiteralNode),
    XValue,
}

impl AstDisplay for FunctionDataNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            FunctionDataNode::Literal(l) => l.get_display_info(),
            FunctionDataNode::XValue => (vec!["XValue".into()], vec![0]),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct FunctionCallNode {
    pub name: String,
    pub arguments: Vec<FunctionArgumentNode>,
}

impl AstDisplay for FunctionCallNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        let mut res = (vec![format!("\"{}\"", &self.name)], vec![0]);

        for arg in &self.arguments {
            let mut arg_res = arg.get_display_info();
            res.0.append(&mut arg_res.0);
            res.1.append(&mut arg_res.1);
        }

        res
    }
}

#[derive(PartialEq, Debug)]
pub enum FunctionArgumentNode {
    Expression(Box<ExpressionNode>),
    Lambda(LambdaNode),
}

impl AstDisplay for FunctionArgumentNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            FunctionArgumentNode::Expression(e) => e.get_display_info(),
            FunctionArgumentNode::Lambda(l) => l.get_display_info(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum LambdaNode {
    AnonymousLambda(Box<ExpressionNode>),
    NamedLambda(String),
}

impl AstDisplay for LambdaNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            LambdaNode::AnonymousLambda(e) => e.get_display_info(),
            LambdaNode::NamedLambda(name) => (vec![format!("\"{}\"", name)], vec![0]),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct ArithmeticExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: ArithmeticOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

impl AstDisplay for ArithmeticExpressionNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        let l_info = self.l_expression.get_display_info();
        let mut op_info = self.operation.get_display_info();
        let mut r_info = self.r_expression.get_display_info();

        let mut res = l_info;
        res.0.append(&mut op_info.0);
        res.1.append(&mut op_info.1);

        res.0.append(&mut r_info.0);
        res.1.append(&mut r_info.1);

        res
    }
}

#[derive(PartialEq, Debug)]
pub enum ArithmeticOperationNode {
    Plus,
    Minus,
}

impl AstDisplay for ArithmeticOperationNode {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            ArithmeticOperationNode::Plus => (vec!["+".into()], vec![0]),
            ArithmeticOperationNode::Minus => (vec!["-".into()], vec![0]),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct LogicExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: LogicOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

impl AstDisplay for LogicExpressionNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        let l_info = self.l_expression.get_display_info();
        let mut op_info = self.operation.get_display_info();
        let mut r_info = self.r_expression.get_display_info();

        let mut res = l_info;
        res.0.append(&mut op_info.0);
        res.1.append(&mut op_info.1);

        res.0.append(&mut r_info.0);
        res.1.append(&mut r_info.1);

        res
    }
}

#[derive(PartialEq, Debug)]
pub enum LogicOperationNode {
    And,
    Or,
}

impl AstDisplay for LogicOperationNode {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            LogicOperationNode::And => (vec!["and".into()], vec![0]),
            LogicOperationNode::Or => (vec!["or".into()], vec![0]),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct CompareExpressionNode {
    pub l_expression: Box<ExpressionNode>,
    pub operation: CompareOperationNode,
    pub r_expression: Box<ExpressionNode>,
}

impl AstDisplay for CompareExpressionNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        let l_info = self.l_expression.get_display_info();
        let mut op_info = self.operation.get_display_info();
        let mut r_info = self.r_expression.get_display_info();

        let mut res = l_info;
        res.0.append(&mut op_info.0);
        res.1.append(&mut op_info.1);

        res.0.append(&mut r_info.0);
        res.1.append(&mut r_info.1);

        res
    }
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

impl AstDisplay for CompareOperationNode {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        match &self {
            CompareOperationNode::Eq => (vec!["=".into()], vec![0]),
            CompareOperationNode::Neq => (vec!["!=".into()], vec![0]),
            CompareOperationNode::Lt => (vec!["<".into()], vec![0]),
            CompareOperationNode::Le => (vec!["<=".into()], vec![0]),
            CompareOperationNode::Gt => (vec![">".into()], vec![0]),
            CompareOperationNode::Ge => (vec![">=".into()], vec![0]),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct TernaryOperatorNode {
    pub check_expression: Box<ExpressionNode>,
    pub true_expression: Box<ExpressionNode>,
    pub false_expression: Box<ExpressionNode>,
}

impl AstDisplay for TernaryOperatorNode {
    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        let l_info = self.check_expression.get_display_info();
        let mut op_info = self.true_expression.get_display_info();
        let mut r_info = self.false_expression.get_display_info();

        let mut res = l_info;
        res.0.append(&mut op_info.0);
        res.1.append(&mut op_info.1);

        res.0.append(&mut r_info.0);
        res.1.append(&mut r_info.1);

        res
    }
}

pub type AstDisplayLevel = u32;

pub trait AstDisplay {
    fn get_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        let mut lines = vec![self.get_node_name().into()];
        let mut levels = vec![0];

        let mut children_data = self.get_children_display_info();

        if !children_data.0.is_empty() {
            children_data.1.iter_mut().for_each(|x| *x += 1);
            lines.append(&mut children_data.0);
            levels.append(&mut children_data.1);
        }

        (lines, levels)
    }

    fn get_children_display_info(&self) -> (Vec<String>, Vec<AstDisplayLevel>) {
        (vec![], vec![])
    }

    fn get_node_name(&self) -> &str {
        let class_name = std::any::type_name::<Self>();
        let opt_i = class_name.rfind("::");

        let class_name = if let Some(i) = opt_i {
            &class_name[i + 2..]
        } else {
            class_name
        };

        class_name.strip_suffix("Node").unwrap_or(class_name)
    }
}

pub fn format_ast(node: &impl AstDisplay) -> String {
    let (lines, levels) = node.get_display_info();
    let mut res = String::new();

    for (i, line) in lines.iter().enumerate() {
        res += &"\t".repeat(levels[i] as usize);
        res += line;
        if i < lines.len() - 1 {
            res += "\n";
        }
    }

    res
}

pub fn format_ast_short(node: &impl AstDisplay) -> String {
    let (mut lines, mut levels) = node.get_display_info();

    if !lines.is_empty() && lines[0] == "AstRoot" {
        lines = lines[2..].to_vec();
        levels = levels[2..].to_vec();
        levels.iter_mut().for_each(|x| *x -= 2);
    }

    let mut res = String::new();

    for (i, line) in lines.iter().enumerate() {
        res += &"\t".repeat(levels[i] as usize);
        res += line;
        if i < lines.len() - 1 {
            res += "\n";
        }
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_display() {
        let node = LiteralNode::Array {
            array: vec![
                LiteralNode::String("qwerty".to_string()),
                LiteralNode::Char('4'),
                LiteralNode::Array {
                    array: vec![LiteralNode::Int(4)],
                },
            ],
        };
        let (lines, levels) = node.get_display_info();

        println!("{:?}", lines);
        println!("{:?}", levels);
    }

    #[test]
    fn xvalue_display() {
        let node = FunctionDataNode::Literal(LiteralNode::Bool(false));
        let (lines, levels) = node.get_display_info();

        println!("{:?}", lines);
        println!("{:?}", levels);
    }
}
