#[cfg(test)]
mod evaluator_tests {
    use crate::ast::*;
    use crate::evaluator::*;
    use crate::lexer::*;
    use crate::object::*;
    use crate::parser::*;

    fn init(source_code: &str) -> Program {
        let lexer = Lexer::new(source_code.to_string());
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap()
    }

    /*
     * Integer
     */
    struct IntegerEvalTest {
        input: String,
        expected: i32,
        program: Program,
    }

    impl IntegerEvalTest {
        fn new(input: &str, expected: i32) -> Self {
            IntegerEvalTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![
            IntegerEvalTest::new("5", 5),
            IntegerEvalTest::new("10", 10),
            IntegerEvalTest::new("-5", -5),
            IntegerEvalTest::new("-10", -10),
        ];

        for case in cases {
            let value = eval(&case.program).unwrap();
            test_integer_object(value, case.expected);
        }
    }

    fn test_integer_object(obj: Object, expected: i32) {
        assert_eq!(obj.obtype(), Type::INTEGER);
        assert_eq!(obj.as_int().unwrap(), expected);
    }

    /*
     * Boolean
     */
    struct BoolEvalTest {
        input: String,
        expected: bool,
        program: Program,
    }

    impl BoolEvalTest {
        fn new(input: &str, expected: bool) -> Self {
            BoolEvalTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_bool_expression() {
        let cases = vec![
            BoolEvalTest::new("true", true),
            BoolEvalTest::new("false", false),
        ];

        for case in cases {
            let value = eval(&case.program).unwrap();
            test_bool_object(value, case.expected);
        }
    }

    fn test_bool_object(obj: Object, expected: bool) {
        assert_eq!(obj.obtype(), Type::BOOLEAN);
        assert_eq!(obj.as_bool().unwrap(), expected);
    }

    /*
     * Bang
     */
    struct BangEvalTest {
        input: String,
        expected: bool,
        program: Program,
    }

    impl BangEvalTest {
        fn new(input: &str, expected: bool) -> Self {
            BangEvalTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_bang_expression() {
        let cases = vec![
            BangEvalTest::new("!true", false),
            BangEvalTest::new("!false", true),
            BangEvalTest::new("!!true", true),
            BangEvalTest::new("!5", false),
            BangEvalTest::new("!!5", true),
        ];

        for case in cases {
            let value = eval(&case.program).unwrap();
            test_bool_object(value, case.expected);
        }
    }
}
