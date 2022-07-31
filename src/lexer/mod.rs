use crate::token::TokenType;

struct Lexer {
    input: String,
    position: i32,
    read_position: i32,
    pub ch: char,
}

impl Lexer {
    fn new(s: String) -> Self {
        let mut l = Lexer {
            input: s,
            position: 0,
            read_position: 1,
            ch: '\0',
        };

        l.read_char();
        l
    }

    /// Read a character and move reading position forward.
    fn read_char(&mut self) {
        let chars: Vec<char> = self.input.chars().collect();
        if self.read_position > chars.len() as i32 {
            self.ch = '\0';
        } else {
            self.ch = *chars.get(self.position as usize).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    /// Get next token and moves the lexer forward one step.
    pub fn next_token(&mut self) -> TokenType {
        let token = match self.ch {
            '=' => TokenType::Assign,
            ';' => TokenType::Semicolon,
            '(' => TokenType::Lparen,
            ')' => TokenType::Rparen,
            '{' => TokenType::Lbrace,
            '}' => TokenType::Rbrace,
            ',' => TokenType::Comma,
            '+' => TokenType::Plus,
            '\0' => TokenType::EOF,
            _ => TokenType::Illegal,
        };

        self.read_char();
        token
    }
}

#[cfg(test)]
mod token_tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let expected = vec![
            TokenType::Assign,
            TokenType::Plus,
            TokenType::Lparen,
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Rbrace,
            TokenType::Comma,
            TokenType::Semicolon,
        ];

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        for (ex_token, ex_literal) in expected.into_iter().zip(input.chars()) {
            assert_eq!(
                ex_token, token,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                ex_token, token
            );
            assert_eq!(
                ex_literal.to_string(),
                token.string(),
                "Expected literal {:?}, but got literal {:?}",
                ex_token,
                token
            );
            token = l.next_token();
        }
    }
}
