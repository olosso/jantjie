#[cfg(test)]
mod parser_tests {
    use crate::parser::*;

    fn init(input: &str) -> Program {
        let input = String::from(input);

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.print_tokens();
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

        let parsed = &program.statements[0];

        assert!(matches!(parsed, Statement::Let(_, _, _)));
        assert_eq!(parsed.token().unwrap().literal, "let");
        assert_eq!(parsed.token().unwrap().token_type, TokenType::Let);
        assert_eq!(parsed.name().unwrap(), "x");

        // TODO Evaluate expressions
        //assert_eq!(parsed.expr().to_string(), "5");
    }

    #[test]
    #[should_panic(expected = "Let statement not followed by an identifier")]
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
    fn test_parse_return_statements() {
        let program = init(
            "
     return 5;
     return 10;
     ",
        );

        assert_eq!(
            program.statements.len(),
            2,
            "Expected program to have 2 statements, but received {:?}",
            program.statements.len()
        );

        let parsed = &program.statements[1];
        dbg!(parsed);

        assert!(matches!(parsed, Statement::Return(_, _)));
        assert_eq!(parsed.token().unwrap().literal, "return");
        assert_eq!(parsed.token().unwrap().token_type, TokenType::Return);
        assert_eq!(parsed.expr(), &Expression::Placeholder);
    }

    #[test]
    fn test_parse_single_token_expressions() {
        case_parse_identifier_expression(
            "foobar;",
            Statement::Expr(
                Token {
                    token_type: TokenType::Ident,
                    literal: String::from("foobar"),
                },
                Expression::Identifier(
                    Token {
                        token_type: TokenType::Ident,
                        literal: String::from("foobar"),
                    },
                    "foobar".to_string(),
                ),
            ),
            "Expected IdentifierToken. Got None.",
        );

        case_parse_integer_literal_expression(
            "42;",
            Statement::Expr(
                Token {
                    token_type: TokenType::Int,
                    literal: String::from("42"),
                },
                Expression::IntegerLiteral(
                    Token {
                        token_type: TokenType::Int,
                        literal: String::from("42"),
                    },
                    42,
                ),
            ),
            "Expected IntegerLiteral. Got None.",
        );

        case_parse_prefix_expression(
            "-42;",
            Statement::Expr(
                Token {
                    token_type: TokenType::Minus,
                    literal: String::from("-"),
                },
                Expression::Prefix(
                    Token {
                        token_type: TokenType::Minus,
                        literal: String::from("-"),
                    },
                    String::from("-"),
                    Box::new(Expression::IntegerLiteral(
                        Token {
                            token_type: TokenType::Int,
                            literal: String::from("42"),
                        },
                        -42,
                    )),
                ),
            ),
            "Expected IntegerLiteral. Got None.",
        );
    }

    fn parse_single_expression(source_code: &str, statement: &Statement) -> Statement {
        let program = init(source_code);
        let n_parsed_statements = program.statements.len();

        assert_eq!(
            n_parsed_statements, 1,
            "Expected program to have a single statement, but received {n_parsed_statements}\n Source code: {source_code}",
        );

        let parsed = program.statements[0].clone();

        assert_eq!(statement.literal(), parsed.literal());
        assert_eq!(
            parsed.token().unwrap().token_type,
            statement.token().unwrap().token_type
        );

        parsed
    }

    fn case_parse_identifier_expression(source_code: &str, statement: Statement, error_msg: &str) {
        let parsed = parse_single_expression(source_code, &statement);

        let parsed_expr = parsed.expr();
        let input_expr = statement.expr();
        assert!(matches!(parsed_expr, Expression::Identifier(_, _)))
    }

    fn case_parse_integer_literal_expression(
        source_code: &str,
        statement: Statement,
        error_msg: &str,
    ) {
        let parsed = parse_single_expression(source_code, &statement);

        let parsed_expr = parsed.expr();
        let input_expr = statement.expr();
        assert!(matches!(parsed_expr, Expression::IntegerLiteral(_, _)))
    }

    fn case_parse_prefix_expression(source_code: &str, statement: Statement, error_msg: &str) {
        let parsed = parse_single_expression(source_code, &statement);

        let parsed_expr = parsed.expr();
        let input_expr = statement.expr();
        assert!(matches!(parsed_expr, Expression::Prefix(_, _, _)))
    }

    struct InfixTest {
        input: String,
        left_value: i32,
        operator: String,
        right_value: i32,
    }

    impl InfixTest {
        fn new(input: String, left_value: i32, operator: String, right_value: i32) -> Self {
            Self {
                input,
                left_value,
                operator,
                right_value,
            }
        }
    }

    #[test]
    fn test_infix_operations() {
        let infix_tests = vec![
            InfixTest::new("5+5".to_string(), 5, "+".to_string(), 5),
            InfixTest::new("5-5".to_string(), 5, "-".to_string(), 5),
            InfixTest::new("5*5".to_string(), 5, "*".to_string(), 5),
            InfixTest::new("5/5".to_string(), 5, "/".to_string(), 5),
            InfixTest::new("5>5".to_string(), 5, ">".to_string(), 5),
            InfixTest::new("5<5".to_string(), 5, "<".to_string(), 5),
            InfixTest::new("5==5".to_string(), 5, "==".to_string(), 5),
            InfixTest::new("5!=5".to_string(), 5, "!=".to_string(), 5),
        ];

        for test in infix_tests {
            let program = init(&test.input);

            // Does the program only contain 1 statement?
            assert_eq!(program.statements.len(), 1);

            // Has the program correctly parsed it as a ExpressionStatement?
            assert!(matches!(program.statements[0], Statement::Expr(_, _)));

            // Has the ExpressionStatement Token been parsed correctly?
            assert_eq!(
                program.statements[0].token_literal(),
                test.left_value.to_string()
            );

            // Has the program correctly parsed it as a InfixExpression?
            assert!(matches!(
                program.statements[0].expr(),
                Expression::Infix(_, _, _, _)
            ));

            // Has the Infix operator been parsed correctly?
            assert_eq!(program.statements[0].expr().op().unwrap(), test.operator);

            // Has the program correctly the Left node as 5?
            assert_eq!(
                program.statements[0].expr().left().unwrap().int().unwrap(),
                5
            );

            // Has the program correctly the Right node as 5?
            assert_eq!(
                program.statements[0].expr().right().unwrap().int().unwrap(),
                5
            );
        }
    }
}
