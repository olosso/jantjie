#[cfg(test)]
mod evaluator_tests {
    use crate::ast::*;
    use crate::evaluator::*;
    use crate::lexer::*;
    use crate::object::*;
    use crate::parser::*;

    struct IntegerEvalTest {
        input: String,
        expected: i32,
        program: Program,
    }

    impl IntegerEvalTest {
        fn new(input: &str, expected: i32) -> Self {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();

            IntegerEvalTest {
                input: input.to_string(),
                expected,
                program,
            }
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![IntegerEvalTest::new("5", 5), IntegerEvalTest::new("10", 10)];

        for case in cases {
            let value = eval(&case.program);
            test_integer_object(value, case.expected);
        }
    }

    fn test_integer_object(obj: Object, expected: i32) {
        assert_eq!(obj.obtype(), Type::INTEGER);
        assert_eq!(obj.as_int().unwrap(), expected);
    }
}
