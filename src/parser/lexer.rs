#![allow(dead_code)]

use std::rc::Rc;
use unescaper::Unescaper;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token {
    Identifier(Rc<str>),
    StringLiteral(String),
    CharLiteral(char),
    IntLiteral(i32),
    BoolLiteral(bool),
    XValue,             // x
    ArrayBracketBegin,  // [
    ArrayBracketEnd,    // ]
    LambdaBracketBegin, // {
    LambdaBracketEnd,   // }
    ParenthesisBegin,   // (
    ParenthesisEnd,     // (
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
    Comma,              // ,
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexerError {
    Eof,
    Expected(char),
    Unexpected(char),
    ExpectedXButGotY(String, char),
    BadEscaping,
    InvalidCharacterLiteral,
    Unknown,
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

    pub fn next(&mut self) -> Result<Token, LexerError> {
        let res = self.next_impl();

        if res.is_err() && *res.as_ref().err().unwrap_or(&LexerError::Unknown) == LexerError::Eof {
            Ok(Token::Eof)
        } else {
            res
        }
    }

    pub fn next_impl(&mut self) -> Result<Token, LexerError> {
        self.eat_space();
        self.save();

        let c = self.eat()?;

        match c {
            '[' => Ok(Token::ArrayBracketBegin),
            ']' => Ok(Token::ArrayBracketEnd),
            '{' => Ok(Token::LambdaBracketBegin),
            '}' => Ok(Token::LambdaBracketEnd),
            '(' => Ok(Token::ParenthesisBegin),
            ')' => Ok(Token::ParenthesisEnd),
            '+' => Ok(Token::Plus),
            '-' => Ok(Token::Minus),
            '=' => Ok(Token::Equal),
            '|' => Ok(Token::Pipe),
            '?' => Ok(Token::Question),
            ':' => Ok(Token::Colon),
            ',' => Ok(Token::Comma),
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
                    Err(LexerError::Unexpected(c))
                }
            }
        }
    }

    pub fn is_eof(&self) -> bool {
        self.rest.len() <= 0
    }

    fn get_curr(&self) -> Result<char, LexerError> {
        self.rest.chars().next().ok_or(LexerError::Eof)
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

    fn eat_all(&mut self, f: impl Fn(char) -> bool) {
        loop {
            let c = self.get_curr();
            if let Ok(c) = c {
                if f(c) {
                    let _ = self.eat();
                } else {
                    return;
                }
            } else {
                return;
            }
        }
    }

    fn eat_space(&mut self) {
        self.eat_all(|c| c.is_whitespace());
    }

    fn save(&mut self) {
        self.stash = self.rest;
    }

    fn load(&mut self) {
        self.rest = self.stash;
    }

    fn get_stashed_string(&self) -> &str {
        let begin = self.stash.as_ptr();
        let end = self.rest.as_ptr();
        let length = unsafe { end.offset_from(begin) } as usize;

        &self.stash[..length]
    }

    fn parse_identifier(&mut self) -> Result<Rc<str>, LexerError> {
        // parsing starts from the second char. first char is remembered in `self.stash`s
        self.eat_all(|c| c.is_ascii_alphanumeric() || c == '_');
        return Ok(Rc::from(self.get_stashed_string()));
    }

    fn parse_char_literal(&mut self) -> Result<char, LexerError> {
        let c = self.parse_quoted('\'')?;

        if c.len() > 1 {
            Err(LexerError::InvalidCharacterLiteral)
        } else {
            c.chars().next().ok_or(LexerError::InvalidCharacterLiteral)
        }
    }

    fn parse_string_literal(&mut self) -> Result<String, LexerError> {
        self.parse_quoted('\"')
    }

    fn parse_quoted(&mut self, quote: char) -> Result<String, LexerError> {
        // parsing starts from the char after `'`.
        loop {
            let c = self.eat()?;

            if c == '\\' {
                let _ = self.eat()?;
                continue;
            }

            if c == quote {
                break;
            }
        }

        let quoted_escaped = self.get_stashed_string();
        let escaped = &quoted_escaped[1..quoted_escaped.len() - 1];
        let unescaped = Unescaper::new(escaped)
            .unescape()
            .map_err(|_| LexerError::BadEscaping)?;

        Ok(unescaped)
    }

    fn parse_int_literal(&mut self) -> Result<i32, LexerError> {
        // parsing starts from the char after `'`.
        self.eat_all(|c| c.is_numeric());

        let str_int = self.get_stashed_string();
        return Ok(str_int.parse::<i32>().unwrap());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_primitives() {
        let code = " }[| >=<<=!=?,   ";
        let mut lexer = Lexer::new(code);

        assert_eq!(lexer.next(), Ok(Token::LambdaBracketEnd));
        assert_eq!(lexer.next(), Ok(Token::ArrayBracketBegin));
        assert_eq!(lexer.next(), Ok(Token::Pipe));
        assert_eq!(lexer.next(), Ok(Token::GreaterEqual));
        assert_eq!(lexer.next(), Ok(Token::Less));
        assert_eq!(lexer.next(), Ok(Token::LessEqual));
        assert_eq!(lexer.next(), Ok(Token::NotEqual));
        assert_eq!(lexer.next(), Ok(Token::Question));
        assert_eq!(lexer.next(), Ok(Token::Comma));
        assert!(!lexer.is_eof());
        let last = lexer.next();
        assert!(lexer.is_eof());
        assert_eq!(last, Ok(Token::Eof));
        assert!(lexer.is_eof());
    }

    #[test]
    fn parse_identifiers() {
        let code = "map { (x) | drop }";
        let mut lexer = Lexer::new(code);

        assert_eq!(lexer.next(), Ok(Token::Identifier(Rc::from("map"))));
        assert_eq!(lexer.next(), Ok(Token::LambdaBracketBegin));
        assert_eq!(lexer.next(), Ok(Token::ParenthesisBegin));
        assert_eq!(lexer.next(), Ok(Token::XValue));
        assert_eq!(lexer.next(), Ok(Token::ParenthesisEnd));
        assert_eq!(lexer.next(), Ok(Token::Pipe));
        assert_eq!(lexer.next(), Ok(Token::Identifier(Rc::from("drop"))));
        assert_eq!(lexer.next(), Ok(Token::LambdaBracketEnd));

        let last = lexer.next();
        assert_eq!(last, Ok(Token::Eof));
        assert!(lexer.is_eof());
    }

    #[test]
    fn parse_char_string_literal() {
        let code = r#"
            'a' '\n' '\'' 'fg' '' '\\' '\q'
            "" "azaz" "Q\werty" "14\n" "\\" "\/" "\r/" "\";s;q\""
            "\\\"
        "#;
        let mut lexer = Lexer::new(code);

        assert_eq!(lexer.next(), Ok(Token::CharLiteral('a')));
        assert_eq!(lexer.next(), Ok(Token::CharLiteral('\n')));
        assert_eq!(lexer.next(), Ok(Token::CharLiteral('\'')));
        assert_eq!(lexer.next(), Err(LexerError::InvalidCharacterLiteral));
        assert_eq!(lexer.next(), Err(LexerError::InvalidCharacterLiteral));
        assert_eq!(lexer.next(), Ok(Token::CharLiteral('\\')));
        assert_eq!(lexer.next(), Err(LexerError::BadEscaping));
        assert_eq!(lexer.next(), Ok(Token::StringLiteral("".to_string())));
        assert_eq!(lexer.next(), Ok(Token::StringLiteral("azaz".to_string())));
        assert_eq!(lexer.next(), Err(LexerError::BadEscaping));
        assert_eq!(lexer.next(), Ok(Token::StringLiteral("14\n".to_string())));
        assert_eq!(lexer.next(), Ok(Token::StringLiteral("\\".to_string())));

        // it's a Unescaper crate specific!
        // Motivation:
        // https://www.ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf
        // On page 4 it says: "\/ represents the solidus character (U+002F)."
        assert_eq!(lexer.next(), Ok(Token::StringLiteral("/".to_string())));

        assert_eq!(lexer.next(), Ok(Token::StringLiteral("\r/".to_string())));
        assert_eq!(
            lexer.next(),
            Ok(Token::StringLiteral("\";s;q\"".to_string()))
        );
        assert_eq!(lexer.next(), Ok(Token::Eof));
    }

    #[test]
    fn parse_int() {
        let code = "12 - 14 + 3";
        let mut lexer = Lexer::new(code);

        assert_eq!(lexer.next(), Ok(Token::IntLiteral(12)));
        assert_eq!(lexer.next(), Ok(Token::Minus));
        assert_eq!(lexer.next(), Ok(Token::IntLiteral(14)));
        assert_eq!(lexer.next(), Ok(Token::Plus));
        assert_eq!(lexer.next(), Ok(Token::IntLiteral(3)));
    }

    #[test]
    fn parse_real_programs() {
        let code = r#"
            "15 dogs"
                | split ' '
                | nth_map 0 { x + "th" }
                | nth_map 1 { drop -1 }
                | reverse
                | join " the "
        "#;

        let mut lexer = Lexer::new(code);

        macro_rules! assert_token {
            ($token:expr) => {
                assert_eq!(lexer.next(), Ok($token));
            };
        }

        assert_token!(Token::StringLiteral("15 dogs".to_string()));
        assert_token!(Token::Pipe);
        assert_token!(Token::Identifier(Rc::from("split")));
        assert_token!(Token::CharLiteral(' '));
        assert_token!(Token::Pipe);
        assert_token!(Token::Identifier(Rc::from("nth_map")));
        assert_token!(Token::IntLiteral(0));
        assert_token!(Token::LambdaBracketBegin);
        assert_token!(Token::XValue);
        assert_token!(Token::Plus);
        assert_token!(Token::StringLiteral("th".to_string()));
        assert_token!(Token::LambdaBracketEnd);
        assert_token!(Token::Pipe);
        assert_token!(Token::Identifier(Rc::from("nth_map")));
        assert_token!(Token::IntLiteral(1));
        assert_token!(Token::LambdaBracketBegin);
        assert_token!(Token::Identifier(Rc::from("drop")));
        assert_token!(Token::Minus);
        assert_token!(Token::IntLiteral(1));
        assert_token!(Token::LambdaBracketEnd);
        assert_token!(Token::Pipe);
        assert_token!(Token::Identifier(Rc::from("reverse")));
        assert_token!(Token::Pipe);
        assert_token!(Token::Identifier(Rc::from("join")));
        assert_token!(Token::StringLiteral(" the ".to_string()));
    }
}
