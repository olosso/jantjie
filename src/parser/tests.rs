#[cfg(test)]
mod parser_tests {
    use crate::parser::*;

    #[test]
    fn test_parse_let_statement() {
        let input = String::from("let x = 5;");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program: Program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            1,
            "Expected program to have 1 statement, but received {:?}",
            program.statements.len()
        );

        let expected_identifiers = vec!["x"];

        for (actual, expected) in program.statements.iter().zip(expected_identifiers) {
            assert_eq!(actual.token_literal(), expected)
        }
    }

    #[test]
    #[should_panic(expected = "let statement not followed by an identifier")]
    fn test_parse_let_statement_without_identifier_should_panic() {
        let malformed_input = "let = 1;".to_string();
        let lexer = Lexer::new(malformed_input);
        let mut parser = Parser::new(lexer);

        let program: Program = parser.parse_program();
    }

    #[test]
    #[should_panic(expected = "Identifier not followed by an assignment in let statement")]
    fn test_parse_let_statement_without_assignment_should_panic() {
        let malformed_input = "let x 5;".to_string();

        let lexer = Lexer::new(malformed_input);
        let mut parser = Parser::new(lexer);

        let program: Program = parser.parse_program();
    }

    #[test]
    fn test_parse_let_statements() {
        let input = String::from(
            "
let x = 5;
let y = 10;
let foobar = 838383;
",
        );

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program: Program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            3,
            "Expected program to have 3 statements, but received {:?}",
            program.statements.len()
        );

        let expected_identifiers = vec!["x", "y", "foobar"];

        dbg!(&program);

        for (actual, expected) in program.statements.iter().zip(expected_identifiers) {
            assert_eq!(actual.token_literal(), expected);
            assert!(matches!(actual, Statement::Let(_, _)))
        }
    }

    #[test]
    fn test_parse_return_statements() {
        let input = String::from(
            "
return 5;
return 10;
",
        );

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program: Program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            2,
            "Expected program to have 2 statements, but received {:?}",
            program.statements.len()
        );

        let expected_identifiers = vec!["", ""];

        dbg!(&program);

        for (actual, expected) in program.statements.iter().zip(expected_identifiers) {
            assert!(matches!(actual, Statement::Return(_)))
        }
    }
}
