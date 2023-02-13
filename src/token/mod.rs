#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    Multiplication,
    Division,
    Not,
    Equal,
    NotEqual,
    GT,
    LT,
    GTOE,
    LTOE,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,

    If,
    Else,
    Return,

    True,
    False,
}

impl TokenType {
    pub fn string(&self) -> String {
        let s = match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::EOF => "EOF",

            TokenType::Ident => "IDENT",
            TokenType::Int => "INT",

            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Multiplication => "*",
            TokenType::Division => "/",
            TokenType::Not => "!",
            TokenType::Equal => "==",
            TokenType::NotEqual => "!=",
            TokenType::GT => ">",
            TokenType::LT => "<",
            TokenType::GTOE => ">=",
            TokenType::LTOE => "<=",

            TokenType::Comma => ",",
            TokenType::Semicolon => ";",

            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",

            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",

            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Return => "RETURN",

            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
        };

        String::from(s)
    }

    pub fn keywords() -> Vec<String> {
        let keywords = ["let", "func", "if", "else", "return", "true", "false"];

        keywords.map(|x| x.to_string()).to_vec()
    }

    pub fn is_statement(&self) -> bool {
        matches!(&self, TokenType::Let | TokenType::Return)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Token {
            token_type,
            literal,
        }
    }

    pub fn from_keyword(literal: String) -> Self {
        let token_type = match literal.as_str() {
            "let" => TokenType::Let,
            "func" => TokenType::Function,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            _ => TokenType::Illegal,
        };

        Token {
            token_type,
            literal,
        }
    }
}

#[cfg(test)]
mod token_tests {

    #[test]
    fn hello() {}
}
