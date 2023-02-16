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

        let parsed = &program.statements[0];

        assert!(matches!(parsed, Statement::Let(_, _, _)));
        assert_eq!(parsed.token().unwrap().literal, "let");
        assert_eq!(parsed.token().unwrap().token_type, TokenType::Let);
        assert_eq!(parsed.name().unwrap(), "x");
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
        assert_eq!(
            *parsed.expr(),
            Expression::IntegerLiteral(Token::new(TokenType::Int, "10".to_string()), 10)
        );
    }

    /*
     * Testing Expressions with a single LiteralExpression.
     */
    struct LiteralExpressionTest<'a, T> {
        exp: &'a Expression,
        expected: T,
    }

    impl<'a, T> LiteralExpressionTest<'a, T> {
        fn new(exp: &'a Expression, expected: T) -> Self {
            LiteralExpressionTest { exp, expected }
        }
    }

    impl<'a> LiteralExpressionTest<'a, i32> {
        fn test(&self) {
            test_integer_literal(self.exp, self.expected)
        }
    }

    impl<'a> LiteralExpressionTest<'a, String> {
        fn test(&self) {
            test_identifier(self.exp, &self.expected)
        }
    }

    impl<'a> LiteralExpressionTest<'a, bool> {
        fn test(&self) {
            test_bool(self.exp, self.expected)
        }
    }

    #[test]
    fn test_literal_expression() {
        LiteralExpressionTest::new(
            &Expression::IntegerLiteral(Token::new(TokenType::Int, "10".to_string()), 10),
            10,
        )
        .test();

        LiteralExpressionTest::new(
            &Expression::Identifier(
                Token::new(TokenType::Ident, "foo".to_string()),
                "foo".to_string(),
            ),
            "foo".to_string(),
        )
        .test();

        LiteralExpressionTest::new(
            &Expression::Bool(Token::new(TokenType::True, "true".to_string()), true),
            true,
        )
        .test();

        LiteralExpressionTest::new(
            &Expression::Bool(Token::new(TokenType::True, "false".to_string()), false),
            false,
        )
        .test();
    }

    fn test_integer_literal(exp: &Expression, value: i32) {
        assert!(
            matches!(exp, Expression::IntegerLiteral(..)),
            "Expression not IntegerLiteral, got {exp:?}",
        );

        assert_eq!(
            exp.int().unwrap(),
            value,
            "IntegerLiteral doesn't have value {value}, got {:?}",
            exp.int().unwrap()
        );

        assert_eq!(
            exp.token_literal(),
            value.to_string(),
            "IntegerLiteral doesn't have literal {value}, got {:?}",
            exp.token_literal()
        );
    }

    fn test_identifier(exp: &Expression, value: &String) {
        assert!(
            matches!(exp, Expression::Identifier(..)),
            "Expression not Identifier, got {exp:?}",
        );

        assert_eq!(
            &exp.to_string(),
            value,
            "Identifier doesn't have value {value}, got {:?}",
            &exp.to_string()
        );

        assert_eq!(
            &exp.token_literal(),
            value,
            "Identifier doesn't have literal {value}, got {:?}",
            &exp.token_literal()
        );
    }

    fn test_bool(exp: &Expression, value: bool) {
        assert!(
            matches!(exp, Expression::Bool(..)),
            "Expression not Bool, got {exp:?}",
        );

        assert_eq!(
            &exp.buul().unwrap(),
            &value,
            "Bool doesn't have value {value}, got {:?}",
            &exp.buul().unwrap()
        );

        assert_eq!(
            &exp.token_literal(),
            &value.to_string(),
            "Identifier doesn't have literal {value}, got {:?}",
            &exp.token_literal()
        );
    }

    /*
     * Testing Expressions a single InfixExpression.
     */
    struct InfixTest<L, R> {
        input: String,
        left: L,
        operator: String,
        right: R,
    }

    impl<L, R> InfixTest<L, R> {
        fn new(input: &str, left: L, operator: &str, right: R) -> Self {
            Self {
                input: input.to_string(),
                left,
                operator: operator.to_string(),
                right,
            }
        }
    }

    impl InfixTest<i32, i32> {
        fn test(&self) {
            let program = init(&self.input);

            // Has the program correctly parsed it as a InfixExpression?
            assert!(matches!(
                program.statements[0].expr(),
                Expression::Infix(_, _, _, _)
            ));

            // Does the program only contain 1 statement?
            assert_eq!(program.statements.len(), 1);

            LiteralExpressionTest::new(program.statements[0].expr().left().unwrap(), self.left)
                .test();
            // Has the Infix operator been parsed correctly?
            assert_eq!(program.statements[0].expr().op().unwrap(), self.operator);

            LiteralExpressionTest::new(program.statements[0].expr().right().unwrap(), self.right)
                .test();
        }
    }

    impl InfixTest<&str, &str> {
        fn test(&self) {
            let program = init(&self.input);

            // Has the program correctly parsed it as a InfixExpression?
            assert!(matches!(
                program.statements[0].expr(),
                Expression::Infix(_, _, _, _)
            ));

            // Does the program only contain 1 statement?
            assert_eq!(program.statements.len(), 1);

            LiteralExpressionTest::new(
                program.statements[0].expr().left().unwrap(),
                self.left.to_owned(),
            )
            .test();

            // Has the Infix operator been parsed correctly?
            assert_eq!(program.statements[0].expr().op().unwrap(), self.operator);

            LiteralExpressionTest::new(
                program.statements[0].expr().right().unwrap(),
                self.right.to_owned(),
            )
            .test();
        }
    }

    impl InfixTest<bool, bool> {
        fn test(&self) {
            let program = init(&self.input);

            // Has the program correctly parsed it as a InfixExpression?
            assert!(matches!(
                program.statements[0].expr(),
                Expression::Infix(_, _, _, _)
            ));

            // Does the program only contain 1 statement?
            assert_eq!(program.statements.len(), 1);

            LiteralExpressionTest::new(
                program.statements[0].expr().left().unwrap(),
                self.left.to_owned(),
            )
            .test();

            // Has the Infix operator been parsed correctly?
            assert_eq!(program.statements[0].expr().op().unwrap(), self.operator);

            LiteralExpressionTest::new(
                program.statements[0].expr().right().unwrap(),
                self.right.to_owned(),
            )
            .test();
        }
    }

    #[test]
    fn test_infix_operations() {
        let infix_tests = vec![
            InfixTest::new("5+5", 5, "+", 5),
            InfixTest::new("5-5", 5, "-", 5),
            InfixTest::new("5*5", 5, "*", 5),
            InfixTest::new("5/5", 5, "/", 5),
            InfixTest::new("5>5", 5, ">", 5),
            InfixTest::new("5<5", 5, "<", 5),
            InfixTest::new("5==5", 5, "==", 5),
            InfixTest::new("5!=5", 5, "!=", 5),
        ];

        for test in infix_tests.into_iter() {
            test.test()
        }

        let infix_tests = vec![
            InfixTest::new("foo+bar", "foo", "+", "bar"),
            InfixTest::new("foo-bar", "foo", "-", "bar"),
            InfixTest::new("foo*bar", "foo", "*", "bar"),
            InfixTest::new("foo/bar", "foo", "/", "bar"),
            InfixTest::new("foo>bar", "foo", ">", "bar"),
            InfixTest::new("foo<bar", "foo", "<", "bar"),
            InfixTest::new("foo==bar", "foo", "==", "bar"),
            InfixTest::new("foo!=bar", "foo", "!=", "bar"),
        ];

        for test in infix_tests.into_iter() {
            test.test()
        }

        let infix_tests = vec![
            InfixTest::new("true==false", true, "==", false),
            InfixTest::new("true!=false", true, "!=", false),
        ];

        for test in infix_tests.into_iter() {
            test.test()
        }
    }

    #[test]
    fn test_longer_infix_operations() {
        let program = init("5+5+5");

        // Does the program only contain 1 statement?
        assert_eq!(program.statements.len(), 1);

        // Has the program correctly parsed it as a ExpressionStatement?
        assert!(matches!(program.statements[0], Statement::Expr(_, _)));
        assert!(matches!(
            program.statements[0].expr(),
            Expression::Infix(..)
        ));
        assert!(matches!(
            **program.statements[0].expr().left().unwrap(),
            Expression::Infix(..)
        ));
        assert!(matches!(
            **program.statements[0].expr().left().unwrap().left().unwrap(),
            Expression::IntegerLiteral(..)
        ));
        assert!(matches!(
            **program.statements[0]
                .expr()
                .left()
                .unwrap()
                .right()
                .unwrap(),
            Expression::IntegerLiteral(..)
        ));
        assert!(matches!(
            **program.statements[0].expr().right().unwrap(),
            Expression::IntegerLiteral(..)
        ));
    }

    /*
     * Testing Precedence parsing.
     */
    struct PrecedenceTest<'a> {
        input: &'a str,
        output: &'a str,
    }

    impl<'a> PrecedenceTest<'a> {
        fn new(input: &'a str, output: &'a str) -> Self {
            PrecedenceTest { input, output }
        }

        fn test(&self) {
            let mut program = init(self.input);
            assert_eq!(program.to_string(), self.output);
        }
    }

    #[test]
    fn test_precedence_parsing() {
        let tests = vec![
            PrecedenceTest::new("1+1+1", "((1 + 1) + 1)"),
            PrecedenceTest::new("1+1*1", "(1 + (1 * 1))"),
            PrecedenceTest::new("1+1*1/1", "(1 + ((1 * 1) / 1))"),
            PrecedenceTest::new("1+1*-1/1", "(1 + ((1 * (-1)) / 1))"),
            PrecedenceTest::new("1+-1*-1/1", "(1 + (((-1) * (-1)) / 1))"),
            PrecedenceTest::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            PrecedenceTest::new("1*1<10", "((1 * 1) < 10)"),
            PrecedenceTest::new("1*1<10+1", "((1 * 1) < (10 + 1))"),
            PrecedenceTest::new("true == false == true", "((true == false) == true)"),
            PrecedenceTest::new("3 <    5 == true", "((3 < 5) == true)"),
        ];

        for test in tests.into_iter() {
            test.test();
        }
    }
}
