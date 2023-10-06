use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token {
    Identifier(Rc<str>),
    StringLiteral(Rc<str>),
    CharLiteral(char),
    IntLiteral(i32),
    BoolLiteral(bool),
    XValue,             // x
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
pub enum LexerError {
    Eof,
    ImpossibleChar,
    Unknown,
    Expected(char),
}

pub struct Lexer<'input> {
    orig: &'input str,
    stash: &'input str,
    rest: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(code: &'input str) -> Lexer {
        Lexer {
            orig: code,
            stash: code,
            rest: code,
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

    fn save(&mut self) {
        self.stash = self.rest;
    }

    fn load(&mut self) {
        self.rest = self.stash;
    }

    pub fn is_eof(&self) -> bool {
        self.rest.len() <= 0
    }

    pub fn next(&mut self) -> Result<Token, LexerError> {
        self.eat_space();
        self.save();

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
            '\"' => self.parse_string_literal().map(|x| Token::StringLiteral(x)),
            '\'' => self.parse_char_literal().map(|x| Token::CharLiteral(x)),
            c => {
                if c.is_alphabetic() || c == '_' {
                    let id = self.parse_identifier()?;
                    match id.as_ref() {
                        "x" => Ok(Token::XValue),
                        "true" => Ok(Token::BoolLiteral(true)),
                        "false" => Ok(Token::BoolLiteral(false)),
                        "or" => Ok(Token::Or),
                        "and" => Ok(Token::And),
                        _ => Ok(Token::Identifier(id)),
                    }
                } else if c.is_numeric() {
                    self.parse_int_literal().map(|x| Token::IntLiteral(x))
                } else {
                    Err(LexerError::ImpossibleChar)
                }
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<Rc<str>, LexerError> {
        // parsing starts from the second char. first char is remembered in `self.stash`
        loop {
            let c = self.get_curr();
            if let Ok(c) = c {
                if c.is_ascii_alphanumeric() || c == '_' {
                    let _ = self.eat()?;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let begin = self.stash.as_ptr();
        let end = self.rest.as_ptr();
        let length = unsafe { end.offset_from(begin) } as usize;

        return Ok(Rc::from(&self.stash[..length]));
    }

    fn parse_char_literal(&mut self) -> Result<char, LexerError> {
        !unimplemented!();
    }

    fn parse_string_literal(&mut self) -> Result<Rc<str>, LexerError> {
        !unimplemented!();
    }

    fn parse_int_literal(&mut self) -> Result<i32, LexerError> {
        self.load();
        !unimplemented!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_primitives() {
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
    }

    #[test]
    fn parse_identifiers() {
        let code = "map { x | drop }";
        let mut lexer = Lexer::new(code);

        assert_eq!(lexer.next(), Ok(Token::Identifier(Rc::from("map"))));
        assert_eq!(lexer.next(), Ok(Token::LambdaBracketBegin));
        assert_eq!(lexer.next(), Ok(Token::XValue));
        assert_eq!(lexer.next(), Ok(Token::Pipe));
        assert_eq!(lexer.next(), Ok(Token::Identifier(Rc::from("drop"))));
        assert_eq!(lexer.next(), Ok(Token::LambdaBracketEnd));

        let last = lexer.next();
        assert_eq!(last, Err(LexerError::Eof));
        assert!(lexer.is_eof());
    }
}
