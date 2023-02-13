#[cfg(test)]
mod parser_tests {
    use crate::parser::*;

    fn init(input: &str) -> Program {
        let input = String::from(input);

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program()
    }

    #[test]
    fn test_parse_let_statement() {
        let program = init("let x = 5;");

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
        let program = init(
            "
let x = 5;
let y = 10;
let foobar = 838383;
",
        );

        assert_eq!(
            program.statements.len(),
            3,
            "Expected program to have 3 statements, but received {:?}",
            program.statements.len()
        );

        let expected_identifiers = vec!["x", "y", "foobar"];

        for (actual, expected) in program.statements.iter().zip(expected_identifiers) {
            assert_eq!(actual.token_literal(), expected);
            assert!(matches!(actual, Statement::Let(_, _)))
        }
    }

    #[test]
    fn test_parse_return_statements() {
        let program = init(
            "
return 5;
return 10;
return;
",
        );

        dbg!(&program);
        assert_eq!(
            program.statements.len(),
            3,
            "Expected program to have 2 statements, but received {:?}",
            program.statements.len()
        );

        let expected_identifiers = vec!["", "", ""];

        for (actual, expected) in program.statements.iter().zip(expected_identifiers) {
            assert!(matches!(actual, Statement::Return(_)))
        }
    }

    #[test]
    fn test_precedence() {
        assert!(Precedence::EMPTY <= Precedence::LOWEST);
        assert!(Precedence::EMPTY < Precedence::LOWEST);
        assert!(Precedence::PREFIX < Precedence::CALL);
    }

    #[test]
    fn test_parse_single_expressions() {
        case_parse_identifier_expression(
            "foobar;",
            Statement::Expr(
                Token {
                    token_type: TokenType::Ident,
                    literal: String::from("foobar"),
                },
                Some(Expression::Placeholder),
            ),
            "Expected IdentifierToken. Got None.",
        );

        case_parse_prefix_int_expression(
            "42;",
            Statement::Expr(
                Token {
                    token_type: TokenType::Ident,
                    literal: String::from("42"),
                },
                Some(Expression::IntegerLiteral(42)),
            ),
            "Expected Some(LiteralExpression). Got None.",
        );

        case_parse_prefix_bang_expression(
            "!foo;",
            Statement::Expr(
                Token {
                    token_type: TokenType::Ident,
                    literal: String::from("-foo"),
                },
                Some(Expression::IntegerLiteral(-42)),
            ),
            "Expected Some(LiteralExpression). Got None.",
        )
    }

    fn parse_single_expression(source_code: &str, statement: &Statement) -> Statement {
        let program = init(source_code);
        let n_parsed_statements = program.statements.len();

        assert_eq!(
            n_parsed_statements, 1,
            "Expected program to have a single statement, but received {}",
            n_parsed_statements
        );

        let parsed = program.statements[0].clone();

        assert_eq!(statement.literal(), parsed.literal());

        parsed
    }

    fn case_parse_identifier_expression(source_code: &str, statement: Statement, error_msg: &str) {
        let parsed = parse_single_expression(source_code, &statement);

        assert!(matches!(parsed, statement))
    }

    fn case_parse_prefix_int_expression(source_code: &str, statement: Statement, error_msg: &str) {
        let parsed = parse_single_expression(source_code, &statement);

        let val = match statement.expr() {
            Some(Expression::IntegerLiteral(x)) => x,
            _ => panic!(),
        };

        match parsed.expr() {
            Some(Expression::IntegerLiteral(i)) => {
                assert_eq!(i, val);
            }
            _ => panic!("{}", error_msg),
        }
    }

    fn case_parse_prefix_bang_expression(source_code: &str, statement: Statement, error_msg: &str) {
        let parsed = parse_single_expression(source_code, &statement);

        // TODO
        //let val = match statement.expr() {
        //    Some(Expression::IntegerLiteral(x)) => x,
        //    _ => panic!(),
        //};

        //match parsed.expr() {
        //    Some(Expression::IntegerLiteral(i)) => {
        //        assert_eq!(i, val);
        //    }
        //    _ => panic!("{}", error_msg),
        //}
    }
}
