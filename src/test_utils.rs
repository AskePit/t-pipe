#[cfg(test)]
pub(crate) mod tests {
    use crate::ast::Ast;
    use crate::parser::Parser;

    pub fn parse(code: &str) -> Ast {
        let mut parser = Parser::new(code);

        let ast = parser.parse();

        if let Err(e) = &ast {
            println!("Parser error is: {:?}", e);
        }

        assert!(ast.is_ok());

        let ast = ast.unwrap();
        ast
    }
}
