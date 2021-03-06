use crate::{
    eval::{environment::Environment, object::Object},
    parser::ast::{Block, Expr, Infix, Literal, Prefix, Program, Statement},
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

pub mod environment;
mod object;

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            environment: Self::add_builtins(Rc::new(RefCell::new(Environment::new()))),
        }
    }

    pub fn new_with_custom_environment(environment: Rc<RefCell<Environment>>) -> Self {
        Self {
            environment: Self::add_builtins(environment),
        }
    }

    fn add_builtins(environment: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let mut environment_mut = environment.borrow_mut();
        environment_mut.set(
            "len",
            Object::Builtin("len".to_string(), |args| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::String(s) => Object::Integer(s.len() as i64),
                    Object::Array(array) => Object::Integer(array.len() as i64),
                    _ => {
                        Object::Error(format!("argument to `len` not supported for {:?}", args[0]))
                    }
                }
            }),
        );

        environment_mut.set(
            "first",
            Object::Builtin("first".to_string(), |args| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                match &args[0] {
                    Object::Array(elems) => elems[0].clone(),
                    _ => Object::Error(format!(
                        "argument to `first` must be Object::Array(..), got={:?}",
                        &args[0]
                    )),
                }
            }),
        );

        environment_mut.set(
            "last",
            Object::Builtin("last".to_string(), |args| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                match &args[0] {
                    Object::Array(elems) if !elems.is_empty() => {
                        let last = elems.len() - 1;
                        elems[last].clone()
                    }
                    _ => Object::Error(format!(
                        "argument to `last` must be ARRAY, got={:?}",
                        &args[0]
                    )),
                }
            }),
        );

        environment_mut.set(
            "rest",
            Object::Builtin("rest".to_string(), |args| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                match &args[0] {
                    Object::Array(elems) if !elems.is_empty() => Object::Array(elems[1..].into()),
                    _ => Object::Error(format!(
                        "argument to `rest` must be ARRAY, got={:?}",
                        &args[0]
                    )),
                }
            }),
        );

        environment_mut.set(
            "push",
            Object::Builtin("push".to_string(), |args| {
                if args.len() != 2 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=2",
                        args.len()
                    ));
                }
                match args[0].clone() {
                    Object::Array(mut elems) => {
                        elems.push(args[1].clone());
                        Object::Array(elems)
                    }
                    _ => Object::Error(format!(
                        "argument to `push` must be ARRAY, got={:?}",
                        &args[0]
                    )),
                }
            }),
        );

        environment_mut.set(
            "puts",
            Object::Builtin("puts".to_string(), |args| {
                args.iter().for_each(|arg| {
                    println!("{}", arg);
                });
                Object::Null
            }),
        );

        drop(environment_mut);
        environment
    }

    pub fn eval(&mut self, program: Program) -> Object {
        let returned = self.eval_block(&program);
        self.returned(returned)
    }

    fn eval_block(&mut self, program: &[Statement]) -> Object {
        match program.len() {
            0 => Object::Null,
            1 => self.eval_statement(&program[0]),
            _ => program.iter().fold(Object::Null, |carried_object, stmt| {
                if matches!(carried_object, Object::Return(_))
                    || matches!(carried_object, Object::Error(_))
                {
                    return carried_object;
                }
                self.eval_statement(stmt)
            }),
        }
    }

    fn returned(&self, maybe_returned: Object) -> Object {
        match maybe_returned {
            Object::Return(object) => *object,
            _ => maybe_returned,
        }
    }

    fn eval_statement(&mut self, statement: &Statement) -> Object {
        match statement {
            Statement::Let(ident, expr) => {
                let val = self.eval_expr(expr);

                self.environment.borrow_mut().set(ident, val);

                Object::Null
            }
            Statement::Return(expr) => {
                let val = self.eval_expr(expr);
                if matches!(val, Object::Error(_)) {
                    return val;
                }
                Object::Return(Box::new(self.eval_expr(expr)))
            }
            Statement::Expression(expr) => self.eval_expr(expr),
            _ => Object::Null,
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Object {
        match expr {
            Expr::Ident(ident) => {
                if let Some(object) = self.environment.borrow().get(ident) {
                    object
                } else {
                    Object::Error(format!("identifier not found: {}", ident))
                }
            }
            Expr::Literal(lit) => self.eval_literal(lit),
            Expr::Prefix(prefix_op, right) => {
                let right = self.eval_expr(right);
                if Self::is_error(&right) {
                    return right;
                }
                self.eval_prefix_expr(prefix_op, right)
            }
            Expr::Infix(infix_op, left, right) => {
                let left = self.eval_expr(left);
                if Self::is_error(&left) {
                    return left;
                }
                let right = self.eval_expr(right);
                if Self::is_error(&right) {
                    return right;
                }
                self.eval_infix_expr(infix_op, left, right)
            }
            Expr::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expr(condition, consequence, alternative),
            Expr::Function { params, body } => {
                Object::Function(params.clone(), body.clone(), self.environment.clone())
            }
            Expr::Call {
                function,
                arguments,
            } => {
                let func_object = self.eval_expr(function);

                let args = arguments
                    .iter()
                    .map(|arg| self.eval_expr(arg))
                    .collect::<Vec<Object>>();

                if Self::is_error(&args[0]) {
                    return args[0].clone();
                }

                match &func_object {
                    Object::Function(..) => self.eval_func_application(args, func_object),
                    Object::Builtin(..) => self.eval_builtin_func(args, func_object),
                    Object::Error(_) => func_object,
                    _ => Object::Error("something went wrong".to_string()),
                }
            }
            Expr::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(|elem| self.eval_expr(elem))
                    .collect::<Vec<Object>>();
                if Self::is_error(&elements[0]) {
                    return elements[0].clone();
                }

                Object::Array(elements)
            }
            Expr::Index(left, index) => {
                let left = self.eval_expr(left);
                if Self::is_error(&left) {
                    return left;
                }
                let index = self.eval_expr(index);
                if Self::is_error(&index) {
                    return index;
                }
                self.eval_index_expr(left, index)
            }
            Expr::HashLiteral(hash) => self.eval_hash_literal(hash),
            _ => Object::Null,
        }
    }

    fn eval_literal(&self, lit: &Literal) -> Object {
        match lit {
            Literal::Int(i) => Object::Integer(*i),
            Literal::Bool(b) => Object::Boolean(*b),
            Literal::String(s) => Object::String(s.clone()),
        }
    }

    fn eval_prefix_expr(&self, prefix: &Prefix, right: Object) -> Object {
        match prefix {
            Prefix::Bang => self.eval_bang_op_expression(right),
            Prefix::Minus => self.eval_minus_op_expression(right),
        }
    }

    fn eval_infix_expr(&self, operator: &Infix, left: Object, right: Object) -> Object {
        match (operator, &left, &right) {
            (_, Object::Integer(_), Object::Integer(_)) => {
                self.eval_integer_infix_expr(operator, &left, &right)
            }
            (_, Object::String(_), Object::String(_)) => {
                self.eval_string_infix_expr(operator, &left, &right)
            }
            (Infix::Equal, Object::Boolean(left), Object::Boolean(right)) => {
                Object::Boolean(left == right)
            }
            (Infix::NotEqual, Object::Boolean(left), Object::Boolean(right)) => {
                Object::Boolean(left != right)
            }
            (_, Object::Boolean(_), Object::Boolean(_)) => Object::Error(format!(
                "unknown operator: {:?} {} {:?}",
                left, operator, right
            )),
            (_, Object::Boolean(_), Object::Integer(_))
            | (_, Object::Integer(_), Object::Boolean(_)) => Object::Error(format!(
                "type mismatch: {:?} {} {:?}",
                left, operator, right
            )),
            _ => Object::Null,
        }
    }

    fn eval_if_expr(
        &mut self,
        condition: &Expr,
        consequence: &[Statement],
        alternative: &Option<Block>,
    ) -> Object {
        let condition = self.eval_expr(condition);

        if matches!(condition, Object::Error(_)) {
            return condition;
        }

        if Self::is_truthy(condition) {
            self.eval_block(consequence)
        } else if let Some(alternative) = alternative {
            self.eval_block(&alternative)
        } else {
            Object::Null
        }
    }

    fn eval_bang_op_expression(&self, object: Object) -> Object {
        match object {
            Object::Boolean(true) => Object::Boolean(false),
            Object::Boolean(false) => Object::Boolean(true),
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false),
        }
    }

    fn eval_minus_op_expression(&self, object: Object) -> Object {
        if let Object::Integer(i) = object {
            Object::Integer(-i)
        } else {
            Object::Error(format!("unknown operator: -{:?}", object))
        }
    }

    fn eval_integer_infix_expr(&self, operator: &Infix, left: &Object, right: &Object) -> Object {
        match (operator, left, right) {
            (Infix::Plus, Object::Integer(left), Object::Integer(right)) => {
                Object::Integer(left + right)
            }
            (Infix::Minus, Object::Integer(left), Object::Integer(right)) => {
                Object::Integer(left - right)
            }
            (Infix::Multiply, Object::Integer(left), Object::Integer(right)) => {
                Object::Integer(left * right)
            }
            (Infix::Divide, Object::Integer(left), Object::Integer(right)) => {
                Object::Integer(left / right)
            }
            (Infix::LessThan, Object::Integer(left), Object::Integer(right)) => {
                Object::Boolean(left < right)
            }
            (Infix::GreaterThan, Object::Integer(left), Object::Integer(right)) => {
                Object::Boolean(left > right)
            }
            (Infix::Equal, Object::Integer(left), Object::Integer(right)) => {
                Object::Boolean(left == right)
            }
            (Infix::NotEqual, Object::Integer(left), Object::Integer(right)) => {
                Object::Boolean(left != right)
            }
            _ => Object::Null,
        }
    }

    fn eval_string_infix_expr(&self, operator: &Infix, left: &Object, right: &Object) -> Object {
        match (operator, left, right) {
            (Infix::Plus, Object::String(s1), Object::String(s2)) => {
                Object::String(s1.to_owned() + s2)
            }
            (..) => Object::Error(format!(
                "unknown operator: {:?} {} {:?}",
                left, operator, right
            )),
        }
    }

    fn eval_func_application(&mut self, arguments: Vec<Object>, function: Object) -> Object {
        let (params, stmt, func_env) = match function {
            Object::Function(params, stmt, func_env) => (params, stmt, func_env),
            _ => return Object::Error(format!("not a function: {:?}", function)),
        };

        let original_env = self.environment.clone();
        let execution_env = Rc::new(RefCell::new(Environment::new_with_outer(func_env)));
        params.iter().zip(arguments).for_each(|(param, arg)| {
            execution_env.borrow_mut().set(param, arg);
        });
        self.environment = execution_env;
        let eval_result = self.eval_block(&stmt);

        self.environment = original_env;
        self.returned(eval_result)
    }

    fn eval_builtin_func(&self, arguments: Vec<Object>, builtin: Object) -> Object {
        match builtin {
            Object::Builtin(_, builtin_fn) => builtin_fn(arguments),
            _ => Object::Error(format!("not a builtin: {:?}", builtin)),
        }
    }

    fn eval_index_expr(&self, left: Object, index: Object) -> Object {
        match (&left, &index) {
            (Object::Array(..), Object::Integer(..)) => self.eval_array_index_expr(left, index),
            (Object::Hash(..), ..) => self.eval_hash_index_expr(left, index),
            _ => Object::Error(format!("index operator not supported: {:?}", left)),
        }
    }

    fn eval_array_index_expr(&self, left: Object, index: Object) -> Object {
        if let (Object::Array(array), Object::Integer(index)) = (left, index) {
            if index < 0 || index > (array.len() - 1) as i64 {
                return Object::Null;
            }

            return array[index as usize].clone();
        }

        Object::Null
    }

    fn eval_hash_index_expr(&self, left: Object, index: Object) -> Object {
        if let Object::Hash(hash) = left {
            return match hash.get(&index) {
                Some(o) => o.clone(),
                None => Object::Null,
            };
        }

        Object::Null
    }

    fn eval_hash_literal(&mut self, hash: &BTreeMap<Expr, Expr>) -> Object {
        Object::Hash(
            hash.iter()
                .map(|(key, value)| (self.eval_expr(key), self.eval_expr(value)))
                .collect::<HashMap<Object, Object>>(),
        )
    }

    fn is_truthy(object: Object) -> bool {
        match object {
            Object::Boolean(true) => true,
            Object::Boolean(false) => false,
            Object::Null => false,
            _ => true,
        }
    }

    fn is_error(object: &Object) -> bool {
        matches!(object, Object::Error(_))
    }
}

#[cfg(test)]
mod test {
    use crate::{
        eval::{environment::Environment, object::Object, Evaluator},
        lexer::{token::TokenSlice, Lexer},
        parser::{
            ast::{Expr, Infix, Literal, Statement},
            Parser,
        },
    };
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    #[derive(Debug)]
    struct TestDataSimple<T> {
        input: &'static str,
        expected: T,
    }

    #[test]
    fn eval_integer_expression() {
        let test_data = vec![
            TestDataSimple {
                input: "5",
                expected: 5,
            },
            TestDataSimple {
                input: "10",
                expected: 10,
            },
            TestDataSimple {
                input: "-5",
                expected: -5,
            },
            TestDataSimple {
                input: "-10",
                expected: -10,
            },
            TestDataSimple {
                input: "5 + 5 + 5 + 5 - 10",
                expected: 10,
            },
            TestDataSimple {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            TestDataSimple {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            TestDataSimple {
                input: "5 * 2 + 10",
                expected: 20,
            },
            TestDataSimple {
                input: "5 + 2 * 10",
                expected: 25,
            },
            TestDataSimple {
                input: "20 + 2 * -10",
                expected: 0,
            },
            TestDataSimple {
                input: "50 / 2 * 2 + 10",
                expected: 60,
            },
            TestDataSimple {
                input: "2 * (5 + 10)",
                expected: 30,
            },
            TestDataSimple {
                input: "3 * 3 * 3 + 10",
                expected: 37,
            },
            TestDataSimple {
                input: "3 * (3 * 3) + 10",
                expected: 37,
            },
            TestDataSimple {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: 50,
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_integer_object(actual, test_datum.expected);
        });
    }

    #[test]
    fn eval_boolean_expression() {
        let test_data = vec![
            TestDataSimple {
                input: "true",
                expected: true,
            },
            TestDataSimple {
                input: "false",
                expected: false,
            },
            TestDataSimple {
                input: "1 < 2",
                expected: true,
            },
            TestDataSimple {
                input: "1 > 2",
                expected: false,
            },
            TestDataSimple {
                input: "1 < 1",
                expected: false,
            },
            TestDataSimple {
                input: "1 > 1",
                expected: false,
            },
            TestDataSimple {
                input: "1 == 1",
                expected: true,
            },
            TestDataSimple {
                input: "1 != 1",
                expected: false,
            },
            TestDataSimple {
                input: "1 == 2",
                expected: false,
            },
            TestDataSimple {
                input: "1 != 2",
                expected: true,
            },
            TestDataSimple {
                input: "true == true",
                expected: true,
            },
            TestDataSimple {
                input: "false == false",
                expected: true,
            },
            TestDataSimple {
                input: "true == false",
                expected: false,
            },
            TestDataSimple {
                input: "true != false",
                expected: true,
            },
            TestDataSimple {
                input: "false != true",
                expected: true,
            },
            TestDataSimple {
                input: "(1 < 2) == true",
                expected: true,
            },
            TestDataSimple {
                input: "(1 < 2) == false",
                expected: false,
            },
            TestDataSimple {
                input: "(1 > 2) == true",
                expected: false,
            },
            TestDataSimple {
                input: "(1 > 2) == false",
                expected: true,
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_boolean_object(actual, test_datum.expected);
        })
    }

    #[test]
    fn bang_operator_expression() {
        let test_data = vec![
            TestDataSimple {
                input: "!true",
                expected: false,
            },
            TestDataSimple {
                input: "!false",
                expected: true,
            },
            TestDataSimple {
                input: "!5",
                expected: false,
            },
            TestDataSimple {
                input: "!!true",
                expected: true,
            },
            TestDataSimple {
                input: "!!false",
                expected: false,
            },
            TestDataSimple {
                input: "!!5",
                expected: true,
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_boolean_object(actual, test_datum.expected);
        })
    }

    #[test]
    fn if_else_expressions() {
        let test_data = vec![
            TestDataSimple {
                input: "if (true) { 10 }",
                expected: Object::Integer(10),
            },
            TestDataSimple {
                input: "if (false) { 10 }",
                expected: Object::Null,
            },
            TestDataSimple {
                input: "if (1) { 10 }",
                expected: Object::Integer(10),
            },
            TestDataSimple {
                input: "if (1 < 2) { 10 }",
                expected: Object::Integer(10),
            },
            TestDataSimple {
                input: "if (1 > 2) { 10 }",
                expected: Object::Null,
            },
            TestDataSimple {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Object::Integer(20),
            },
            TestDataSimple {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected: Object::Integer(10),
            },
            TestDataSimple {
                input: "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
                expected: Object::Integer(10),
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        });
    }

    #[test]
    fn return_statements() {
        let test_data = vec![
            TestDataSimple {
                input: "return 10;",
                expected: Object::Integer(10),
            },
            TestDataSimple {
                input: "return 10; 9;",
                expected: Object::Integer(10),
            },
            TestDataSimple {
                input: "return 2 * 5; 9;",
                expected: Object::Integer(10),
            },
            TestDataSimple {
                input: "9; return 2 * 5; 9;",
                expected: Object::Integer(10),
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        });
    }

    #[test]
    fn error_handling() {
        let test_data = vec![
            TestDataSimple {
                input: "5 + true;",
                expected: "type mismatch: Object::Integer(5) + Object::Boolean(true)",
            },
            TestDataSimple {
                input: "5 + true; 5;",
                expected: "type mismatch: Object::Integer(5) + Object::Boolean(true)",
            },
            TestDataSimple {
                input: "-true",
                expected: "unknown operator: -Object::Boolean(true)",
            },
            TestDataSimple {
                input: "true + false;",
                expected: "unknown operator: Object::Boolean(true) + Object::Boolean(false)",
            },
            TestDataSimple {
                input: "5; true + false; 5",
                expected: "unknown operator: Object::Boolean(true) + Object::Boolean(false)",
            },
            TestDataSimple {
                input: "if (10 > 1) { true + false; }",
                expected: "unknown operator: Object::Boolean(true) + Object::Boolean(false)",
            },
            TestDataSimple {
                input: "if (10 > 1) { if ( 10 > 1) { return true + false; } return 1; }",
                expected: "unknown operator: Object::Boolean(true) + Object::Boolean(false)",
            },
            TestDataSimple {
                input: "foobar",
                expected: "identifier not found: foobar",
            },
            TestDataSimple {
                input: r#""Hello" - "World""#,
                expected: r#"unknown operator: Object::String("Hello") - Object::String("World")"#,
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_error(actual, test_datum.expected);
        });
    }

    #[test]
    fn let_statement() {
        let test_data = vec![
            TestDataSimple {
                input: "let a = 5; a;",
                expected: 5,
            },
            TestDataSimple {
                input: "let a = 5 * 5; a;",
                expected: 25,
            },
            TestDataSimple {
                input: "let a = 5; let b = a; b;",
                expected: 5,
            },
            TestDataSimple {
                input: "let a = 5; let b = a; let c = a + b + 5; c;",
                expected: 15,
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_integer_object(actual, test_datum.expected)
        });
    }

    #[test]
    fn function_object() {
        let test_data = vec![TestDataSimple {
            input: "fn(x) { x + 2; };",
            expected: Object::Function(
                vec!["x".to_string()],
                vec![Statement::Expression(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Literal(Literal::Int(2))),
                ))],
                Evaluator::add_builtins(Rc::new(RefCell::new(Environment::new()))),
            ),
        }];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        })
    }

    #[test]
    fn function_application() {
        let test_data = vec![
            TestDataSimple {
                input: "let identity = fn(x) {x; }; identity(5);",
                expected: 5,
            },
            TestDataSimple {
                input: "let identity = fn(x) { return x; }; identity(5)",
                expected: 5,
            },
            TestDataSimple {
                input: "let double = fn(x) { return x * 2; }; double(5);",
                expected: 10,
            },
            TestDataSimple {
                input: "let add = fn(x, y) { x + y; }; add(5, 5);",
                expected: 10,
            },
            TestDataSimple {
                input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                expected: 20,
            },
            TestDataSimple {
                input: "fn(x) { x; }(5)",
                expected: 5,
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_integer_object(actual, test_datum.expected);
        });
    }

    #[test]
    fn closures() {
        let test_data = vec![TestDataSimple {
            input: r#"let newAdder = fn(x) {
                  fn(y) { x + y };
                };
                
                let addTwo = newAdder(2);
                addTwo(2);"#,
            expected: 4,
        }];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_integer_object(actual, test_datum.expected);
        });
    }

    #[test]
    fn string_literal() {
        let test_data = vec![TestDataSimple {
            input: r#""Hello World!""#,
            expected: Object::String("Hello World!".into()),
        }];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        })
    }

    #[test]
    fn builtin_functions() {
        let test_data = vec![
            TestDataSimple {
                input: r#"len("")"#,
                expected: Object::Integer(0),
            },
            TestDataSimple {
                input: r#"len("four")"#,
                expected: Object::Integer(4),
            },
            TestDataSimple {
                input: r#"len("hello world")"#,
                expected: Object::Integer(11),
            },
            TestDataSimple {
                input: r#"len(1)"#,
                expected: Object::Error(
                    "argument to `len` not supported for Object::Integer(1)".into(),
                ),
            },
            TestDataSimple {
                input: r#"len("one", "two")"#,
                expected: Object::Error("wrong number of arguments. got=2, want=1".into()),
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        })
    }

    #[test]
    fn array_literal() {
        let test_data = vec![TestDataSimple {
            input: "[1, 2 * 2, 3 + 3]",
            expected: Object::Array(vec![
                Object::Integer(1),
                Object::Integer(4),
                Object::Integer(6),
            ]),
        }];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        });
    }

    #[test]
    fn array_index_exprs() {
        let test_data = vec![
            TestDataSimple {
                input: "[1, 2, 3][0]",
                expected: Object::Integer(1),
            },
            TestDataSimple {
                input: "[1, 2, 3][1]",
                expected: Object::Integer(2),
            },
            TestDataSimple {
                input: "[1, 2, 3][2]",
                expected: Object::Integer(3),
            },
            TestDataSimple {
                input: "let i = 0; [1][i]",
                expected: Object::Integer(1),
            },
            TestDataSimple {
                input: "[1, 2, 3][1 + 1];",
                expected: Object::Integer(3),
            },
            TestDataSimple {
                input: "let myArray = [1, 2, 3]; myArray[2];",
                expected: Object::Integer(3),
            },
            TestDataSimple {
                input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                expected: Object::Integer(6),
            },
            TestDataSimple {
                input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                expected: Object::Integer(2),
            },
            TestDataSimple {
                input: "[1, 2, 3][3]",
                expected: Object::Null,
            },
            TestDataSimple {
                input: "[1, 2, 3][-1]",
                expected: Object::Null,
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        })
    }

    #[test]
    fn hash_literals() {
        let test_data = vec![TestDataSimple {
            input: r#"let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }"#,
            expected: {
                let mut map = HashMap::new();
                map.insert(Object::String("one".to_string()), Object::Integer(1));
                map.insert(Object::String("two".to_string()), Object::Integer(2));
                map.insert(Object::String("three".to_string()), Object::Integer(3));
                map.insert(Object::Integer(4), Object::Integer(4));
                map.insert(Object::Boolean(true), Object::Integer(5));
                map.insert(Object::Boolean(false), Object::Integer(6));
                Object::Hash(map)
            },
        }];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        })
    }

    #[test]
    fn hash_index_expressions() {
        let test_data = vec![
            TestDataSimple {
                input: r#"{"foo": 5}["foo"]"#,
                expected: Object::Integer(5),
            },
            TestDataSimple {
                input: r#"{"foo": 5}["bar"]"#,
                expected: Object::Null,
            },
            TestDataSimple {
                input: r#"let key = "foo"; {"foo": 5}[key]"#,
                expected: Object::Integer(5),
            },
            TestDataSimple {
                input: r#"{}["foo"]"#,
                expected: Object::Null,
            },
            TestDataSimple {
                input: r#"{5: 5}[5]"#,
                expected: Object::Integer(5),
            },
            TestDataSimple {
                input: r#"{true: 5}[true]"#,
                expected: Object::Integer(5),
            },
            TestDataSimple {
                input: r#"{false: 5}[false]"#,
                expected: Object::Integer(5),
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let actual = eval(test_datum.input);
            assert_object(actual, test_datum.expected);
        });
    }

    fn eval(input: &str) -> Object {
        let tokens = Lexer::new().lex_input(input).unwrap();
        let program = Parser::parse(TokenSlice::from_tokens(&tokens)).unwrap();

        Evaluator::new().eval(program)
    }

    fn assert_integer_object(actual: Object, expected: i64) {
        assert!(
            matches!(actual, Object::Integer(_)),
            "expected Object::Integer(_), received {:?}",
            actual
        );
        if let Object::Integer(actual) = actual {
            assert_eq!(actual, expected);
        }
    }

    fn assert_boolean_object(actual: Object, expected: bool) {
        assert!(
            matches!(actual, Object::Boolean(_)),
            "Expected Object::Boolean(_), Received {:?}",
            actual
        );
        if let Object::Boolean(actual) = actual {
            assert_eq!(actual, expected);
        }
    }

    fn assert_object(actual: Object, expected: Object) {
        assert_eq!(actual, expected);
    }

    fn assert_error(actual: Object, expected: &str) {
        assert!(
            matches!(actual, Object::Error(_)),
            "Expected Object::Error(_), received {:?}",
            actual
        );
        if let Object::Error(actual) = actual {
            assert_eq!(actual, expected);
        }
    }
}
