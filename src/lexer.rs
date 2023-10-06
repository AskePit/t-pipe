#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Lexem<'input> {
    Identifier(&'input str),
    StringLiteral(&'input str),
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

#[derive(Debug, PartialEq)]
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

    pub fn next(&mut self) -> Result<Lexem, LexerError> {
        self.eat_space();

        let c = self.eat()?;

        match c {
            '[' => Ok(Lexem::ArrayBracketBegin),
            ']' => Ok(Lexem::ArrayBracketEnd),
            '{' => Ok(Lexem::LambdaBracketBegin),
            '}' => Ok(Lexem::LambdaBracketEnd),
            '+' => Ok(Lexem::Plus),
            '-' => Ok(Lexem::Minus),
            '=' => Ok(Lexem::Equal),
            '|' => Ok(Lexem::Pipe),
            '?' => Ok(Lexem::Question),
            ':' => Ok(Lexem::Colon),
            '!' => {
                let c2 = self.eat()?;
                if c2 == '=' {
                    Ok(Lexem::NotEqual)
                } else {
                    Err(LexerError::Expected('='))
                }
            }
            '<' => match self.get_curr() {
                Ok(c2) => {
                    if c2 == '=' {
                        self.eat()?;
                        Ok(Lexem::LessEqual)
                    } else {
                        Ok(Lexem::Less)
                    }
                }
                Err(_) => Ok(Lexem::Less),
            },
            '>' => match self.get_curr() {
                Ok(c2) => {
                    if c2 == '=' {
                        self.eat()?;
                        Ok(Lexem::GreaterEqual)
                    } else {
                        Ok(Lexem::Greater)
                    }
                }
                Err(_) => Ok(Lexem::Greater),
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_primitives() {
        // let code = "\"qwerty\" | map {'y' ? 'z' : x}";
        let code = " }[| >=<<=!=?   ";
        let mut lexer = Lexer::new(code);

        assert_eq!(lexer.next(), Ok(Lexem::LambdaBracketEnd));
        assert_eq!(lexer.next(), Ok(Lexem::ArrayBracketBegin));
        assert_eq!(lexer.next(), Ok(Lexem::Pipe));
        assert_eq!(lexer.next(), Ok(Lexem::GreaterEqual));
        assert_eq!(lexer.next(), Ok(Lexem::Less));
        assert_eq!(lexer.next(), Ok(Lexem::LessEqual));
        assert_eq!(lexer.next(), Ok(Lexem::NotEqual));
        assert_eq!(lexer.next(), Ok(Lexem::Question));
        assert_eq!(lexer.next(), Err(LexerError::Eof));
        // assert_eq!(token, Lexem::StringLiteral("qwerty"));
    }

    #[test]
    fn parse_basic_lambda() {
        let code = "\"qwerty\" | map {'y' ? 'z' : x}";
        let mut lexer = Lexer::new(code);

        let token = lexer.next().unwrap();
        assert_eq!(token, Lexem::StringLiteral("qwerty"));
    }
}
