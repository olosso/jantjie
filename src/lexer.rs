use crate::tokens::TokenType;

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    /// Read a character and move reading position forward.
    fn read_char(&mut self) {
        let chars: Vec<char> = self.input.chars().collect();
        if self.read_position >= self.input.len() {
            self.ch = '_';
        } else {
            self.ch = *chars.get(self.read_position).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
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

        let l = Lexer::new(input);

        for (token_type, literal) in expected.iter().zip(input.chars()) {
            let token = l.next();

            assert_eq!(token.tokentype, token_type);
            assert_eq!(token.literal, literal);
        }
    }
}
