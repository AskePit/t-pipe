use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token {
    Identifier(Rc<str>),
    StringLiteral(Rc<str>),
    CharLiteral(char),
    IntLiteral(i32),
    BoolLiteral(bool),
    ArrayBracketBegin,  // [
    ArrayBracketEnd,    // ]
    LambdaBracketBegin, // {
    LambdaBracketEnd,   // }
    Plus,               // +
    Minus,              // -
    Equal,              // =
    NotEqual,           // !=
    Less,               // <
    LessEqual,          // <=
    Greater,            // >
    GreaterEqual,       // >=
    And,                // and
    Or,                 // or
    Pipe,               // |
    Question,           // ?
    Colon,              // :
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum LexerError {
    Eof,
    Unknown,
    Expected(char),
}

struct Lexer<'input> {
    orig: &'input str,
    rest: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(code: &'input str) -> Lexer {
        Lexer {
            orig: code,
            rest: code,
        }
    }

    pub fn next(&mut self) -> Result<Token, LexerError> {
        self.eat_space();

        let c = self.eat()?;

        match c {
            '[' => Ok(Token::ArrayBracketBegin),
            ']' => Ok(Token::ArrayBracketEnd),
            '{' => Ok(Token::LambdaBracketBegin),
            '}' => Ok(Token::LambdaBracketEnd),
            '+' => Ok(Token::Plus),
            '-' => Ok(Token::Minus),
            '=' => Ok(Token::Equal),
            '|' => Ok(Token::Pipe),
            '?' => Ok(Token::Question),
            ':' => Ok(Token::Colon),
            '!' => {
                let c2 = self.eat()?;
                if c2 == '=' {
                    Ok(Token::NotEqual)
                } else {
                    Err(LexerError::Expected('='))
                }
            }
            '<' => match self.get_curr() {
                Ok(c2) => {
                    if c2 == '=' {
                        self.eat()?;
                        Ok(Token::LessEqual)
                    } else {
                        Ok(Token::Less)
                    }
                }
                Err(_) => Ok(Token::Less),
            },
            '>' => match self.get_curr() {
                Ok(c2) => {
                    if c2 == '=' {
                        self.eat()?;
                        Ok(Token::GreaterEqual)
                    } else {
                        Ok(Token::Greater)
                    }
                }
                Err(_) => Ok(Token::Greater),
            },
            _ => Err(LexerError::Unknown),
        }
    }

    fn get_curr(&self) -> Result<char, LexerError> {
        self.rest.chars().next().ok_or(LexerError::Eof)
    }

    fn get_curr2(&self) -> Result<char, LexerError> {
        let mut chars = self.rest.chars();
        chars.next().ok_or(LexerError::Eof)?;
        chars.next().ok_or(LexerError::Eof)
    }

    fn eat(&mut self) -> Result<char, LexerError> {
        let mut indices = self.rest.char_indices();
        let (_curr_i, curr_c) = indices.next().ok_or(LexerError::Eof)?;

        match indices.next() {
            None => {
                self.rest = &self.rest[self.rest.len()..];
            }
            Some((next_i, _next_c)) => {
                self.rest = &self.rest[next_i..];
            }
        }

        Ok(curr_c)
    }

    fn eat_space(&mut self) {
        loop {
            let c = self.get_curr();
            if let Ok(c) = c {
                if c.is_whitespace() {
                    let _ = self.eat();
                } else {
                    return;
                }
            } else {
                return;
            }
        }
    }

    pub fn is_eof(&self) -> bool {
        self.rest.len() <= 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_primitives() {
        // let code = "\"qwerty\" | map {'y' ? 'z' : x}";
        let code = " }[| >=<<=!=?   ";
        let mut lexer = Lexer::new(code);

        assert_eq!(lexer.next(), Ok(Token::LambdaBracketEnd));
        assert_eq!(lexer.next(), Ok(Token::ArrayBracketBegin));
        assert_eq!(lexer.next(), Ok(Token::Pipe));
        assert_eq!(lexer.next(), Ok(Token::GreaterEqual));
        assert_eq!(lexer.next(), Ok(Token::Less));
        assert_eq!(lexer.next(), Ok(Token::LessEqual));
        assert_eq!(lexer.next(), Ok(Token::NotEqual));
        assert_eq!(lexer.next(), Ok(Token::Question));
        assert!(!lexer.is_eof());
        let last = lexer.next();
        assert!(lexer.is_eof());
        assert_eq!(last, Err(LexerError::Eof));
        assert!(lexer.is_eof());
        // assert_eq!(token, Lexem::StringLiteral("qwerty"));
    }

    #[test]
    fn parse_basic_lambda() {
        let code = "\"qwerty\" | map {'y' ? 'z' : x}";
        let mut lexer = Lexer::new(code);

        let token = lexer.next().unwrap();
        assert_eq!(token, Token::StringLiteral(Rc::from("qwerty")));
    }
}
