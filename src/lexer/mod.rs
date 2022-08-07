use crate::token::*;

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
    pub fn next_token(&mut self) -> Token {
        let token = match self.ch {
            '=' => Token::new(TokenType::Assign, self.ch.to_string()),
            ';' => Token::new(TokenType::Semicolon, self.ch.to_string()),
            '(' => Token::new(TokenType::Lparen, self.ch.to_string()),
            ')' => Token::new(TokenType::Rparen, self.ch.to_string()),
            '{' => Token::new(TokenType::Lbrace, self.ch.to_string()),
            '}' => Token::new(TokenType::Rbrace, self.ch.to_string()),
            ',' => Token::new(TokenType::Comma, self.ch.to_string()),
            '+' => Token::new(TokenType::Plus, self.ch.to_string()),
            '\0' => Token::new(TokenType::EOF, "EOF".to_string()),
            _ => Token::new(TokenType::Assign, "ILLEGAL".to_string()),
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
                ex_token, token.token_type,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                ex_token, token.token_type
            );
            assert_eq!(
                ex_literal.to_string(),
                token.literal,
                "Expected literal {:?}, but got literal {:?}",
                ex_token,
                token.literal
            );
            token = l.next_token();
        }
    }

    #[test]
    fn test_next_token2() {
        let input = "let five = 5
let ten = 10

let add = fn(x, y) {
    x + y;
}

let result = add(five, ten);";

        let expected = vec![
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "five".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "ten".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "10".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "add".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Function,
                literal: "fn".to_string(),
            },
            Token {
                token_type: TokenType::Lparen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "x".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "y".to_string(),
            },
            Token {
                token_type: TokenType::Rparen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::Lbrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "x".to_string(),
            },
            Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "y".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Lbrace,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "result".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "add".to_string(),
            },
            Token {
                token_type: TokenType::Lparen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "five".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "ten".to_string(),
            },
            Token {
                token_type: TokenType::Rparen,
                literal: ")".to_string(),
            },
        ];

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        for (ex_token, ex_literal) in expected.into_iter().zip(input.chars()) {
            assert_eq!(
                ex_token, token.token_type,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                ex_token, token.token_type
            );
            assert_eq!(
                ex_literal.to_string(),
                token.literal,
                "Expected literal {:?}, but got literal {:?}",
                ex_token,
                token.literal
            );
            token = l.next_token();
        }
    }
}
