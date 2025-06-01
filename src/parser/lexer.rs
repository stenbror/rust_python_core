#[derive(Debug, Clone, PartialEq)]
pub enum Token
{
    EOF(usize, usize),
    Indent(usize, usize),
    Dedent(usize, usize),
    Newline(usize, usize),
    False(usize, usize),
    None(usize, usize),
    True(usize, usize),
    And(usize, usize),
    As(usize, usize),
    Assert(usize, usize),
    Async(usize, usize),
    Await(usize, usize),
    Break(usize, usize),
    Case(usize, usize),
    Class(usize, usize),
    Continue(usize, usize),
    Def(usize, usize),
    Default(usize, usize),
    Del(usize, usize),
    Elif(usize, usize),
    Else(usize, usize),
    Except(usize, usize),
    Finally(usize, usize),
    For(usize, usize),
    From(usize, usize),
    Global(usize, usize),
    If(usize, usize),
    Import(usize, usize),
    In(usize, usize),
    Is(usize, usize),
    Lambda(usize, usize),
    Nonlocal(usize, usize),
    Not(usize, usize),
    Match(usize, usize),
    Or(usize, usize),
    Pass(usize, usize),
    Raise(usize, usize),
    Return(usize, usize),
    Try(usize, usize),
    While(usize, usize),
    With(usize, usize),
    Yield(usize, usize),
    Plus(usize, usize),
    Minus(usize, usize),
    Star(usize, usize),
    Power(usize, usize),
    Slash(usize, usize),
    SlashSlash(usize, usize),
    Modulo(usize, usize),
    Decorator(usize, usize),
    ShiftLeft(usize, usize),
    ShiftRight(usize, usize),
    BitwiseAnd(usize, usize),
    BitwiseOr(usize, usize),
    BitwiseXor(usize, usize),
    BitwiseInvert(usize, usize),
    ColonEqual(usize, usize),
    Less(usize, usize),
    Greater(usize, usize),
    LessEqual(usize, usize),
    GreaterEqual(usize, usize),
    Equal(usize, usize),
    NotEqual(usize, usize),
    Bang(usize, usize),
    LeftParen(usize, usize),
    RightParen(usize, usize),
    LeftBracket(usize, usize),
    RightBracket(usize, usize),
    LeftCurly(usize, usize),
    RightCurly(usize, usize),
    Comma(usize, usize),
    Colon(usize, usize),
    BitwiseNot(usize, usize),
    Period(usize, usize),
    Semicolon(usize, usize),
    Assign(usize, usize),
    Arrow(usize, usize),
    PlusAssign(usize, usize),
    MinusAssign(usize, usize),
    StarAssign(usize, usize),
    SlashAssign(usize, usize),
    SlashSlashAssign(usize, usize),
    ModuloAssign(usize, usize),
    DecoratorAssign(usize, usize),
    BitwiseAndAssign(usize, usize),
    BitwiseOrAssign(usize, usize),
    BitwiseXorAssign(usize, usize),
    ShiftLeftAssign(usize, usize),
    ShiftRightAssign(usize, usize),
    PowerAssign(usize, usize),
    Name(usize, usize, Box<str>),
    Number(usize, usize, Box<str>),
    String(usize, usize, Vec::<Box<str>>)
}


pub struct SyntaxError {
    line: usize,
    column: usize,
    message: String
}

impl SyntaxError {
    pub fn new(line: usize, column: usize, message: String) -> Self {
        SyntaxError { line: line, column: column, message: message }
    }
}



struct PythonCoreLexer {
    buffer: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
    indent_stack: Vec<usize>,
    parenthesis_stack: Vec<char>
}

impl PythonCoreLexer {
    pub fn new(text: &str) -> Self {
        PythonCoreLexer {
            buffer: text.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
            indent_stack: vec![0],
            parenthesis_stack: Vec::new()
        }
    }

    fn peek(&self) -> Option<char> {
        if self.position < self.buffer.len() {
            Some(self.buffer[self.position])
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<char> {
        if self.position < self.buffer.len() {
            let ch = self.buffer[self.position];
            self.position += 1;
            self.column += 1;
            Some(ch)
        } else {
            None
        }
    }



    fn tokenize_source(&mut self) -> Result<Vec<Token>, SyntaxError> {
        let mut nodes: Vec<Token> = Vec::new();

        while let Some(ch) = self.peek() {
            match ch {
                '(' => {
                    self.advance();
                    self.parenthesis_stack.push(ch);
                    nodes.push(Token::LeftParen(self.line, self.column - 1));
                },
                ')' => {
                    self.advance();
                    match self.parenthesis_stack.pop() {
                        Some(x) => {
                            if x == '(' {
                                nodes.push(Token::RightParen(self.line, self.column - 1));
                            } else {
                                return Err(SyntaxError::new(self.line, self.column - 1, format!("Mismatched parenthesis, found ')' without matching '('!")));
                            }
                        },
                        None => {
                            return Err(SyntaxError::new(self.line, self.column - 1, format!("No opening parenthesis found for ')'!")));
                        }
                    }
                },
                '[' => {
                    self.advance();
                    self.parenthesis_stack.push(ch);
                    nodes.push(Token::LeftBracket(self.line, self.column - 1));
                },
                ']' => {
                    self.advance();
                    match self.parenthesis_stack.pop() {
                        Some(x) => {
                            if x == '[' {
                                nodes.push(Token::RightBracket(self.line, self.column - 1));
                            } else {
                                return Err(SyntaxError::new(self.line, self.column - 1, format!("Mismatched parenthesis, found ']' without matching '['!")));
                            }
                        },
                        None => {
                            return Err(SyntaxError::new(self.line, self.column - 1, format!("No opening parenthesis found for ']'!")));
                        }
                    }
                },
                '{' => {
                    self.advance();
                    self.parenthesis_stack.push(ch);
                    nodes.push(Token::LeftCurly(self.line, self.column - 1));
                },
                '}' => {
                    self.advance();
                    match self.parenthesis_stack.pop() {
                        Some(x) => {
                            if x == '{' {
                                nodes.push(Token::RightCurly(self.line, self.column - 1));
                            } else {
                                return Err(SyntaxError::new(self.line, self.column - 1, format!("Mismatched parenthesis, found '}}' without matching '{{'!")));
                            }
                        },
                        None => {
                            return Err(SyntaxError::new(self.line, self.column - 1, format!("No opening parenthesis found for '}}'!")));
                        }
                    }
                },
                '+' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::PlusAssign(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::Plus(self.line, self.column - 1))
                    }
                },
                '-' => {
                    self.advance();
                    if let Some('>') = self.peek() {
                        self.advance();
                        nodes.push(Token::Arrow(self.line, self.column - 2))
                    } else if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::MinusAssign(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::Minus(self.line, self.column - 1))
                    }
                },
                '*' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::StarAssign(self.line, self.column - 2))
                    } else if let Some('*') = self.peek() {
                        self.advance();
                         if let Some('=') = self.peek() {
                            self.advance();
                            nodes.push(Token::PowerAssign(self.line, self.column - 3))
                         } else {
                            nodes.push(Token::Power(self.line, self.column - 2))
                         }
                    } else {
                        nodes.push(Token::Star(self.line, self.column - 1))
                    }
                },
                '/' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::SlashAssign(self.line, self.column - 2))
                    } else if let Some('/') = self.peek() {
                        self.advance();
                         if let Some('=') = self.peek() {
                            self.advance();
                            nodes.push(Token::SlashSlashAssign(self.line, self.column - 3))
                         } else {
                            nodes.push(Token::SlashSlash(self.line, self.column - 2))
                         }
                    } else {
                        nodes.push(Token::Slash(self.line, self.column - 1))
                    }
                },
                 '%' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::ModuloAssign(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::Modulo(self.line, self.column - 1))
                    }
                },
                 '@' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::DecoratorAssign(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::Decorator(self.line, self.column - 1))
                    }
                },
                '<' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::LessEqual(self.line, self.column - 2))
                    } else if let Some('<') = self.peek() {
                        self.advance();
                         if let Some('=') = self.peek() {
                            self.advance();
                            nodes.push(Token::ShiftLeftAssign(self.line, self.column - 3))
                         } else {
                            nodes.push(Token::ShiftLeft(self.line, self.column - 2))
                         }
                    } else {
                        nodes.push(Token::Less(self.line, self.column - 1))
                    }
                },
                '>' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::GreaterEqual(self.line, self.column - 2))
                    } else if let Some('>') = self.peek() {
                        self.advance();
                         if let Some('=') = self.peek() {
                            self.advance();
                            nodes.push(Token::ShiftRightAssign(self.line, self.column - 3))
                         } else {
                            nodes.push(Token::ShiftRight(self.line, self.column - 2))
                         }
                    } else {
                        nodes.push(Token::Greater(self.line, self.column - 1))
                    }
                },
                '=' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::Equal(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::Assign(self.line, self.column - 1))
                    }
                },
                '!' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::NotEqual(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::Bang(self.line, self.column - 1))
                    }
                },
                ':' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::ColonEqual(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::Colon(self.line, self.column - 1))
                    }
                },
                '&' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::BitwiseAndAssign(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::BitwiseAnd(self.line, self.column - 1))
                    }
                },
                '|' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::BitwiseOrAssign(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::BitwiseOr(self.line, self.column - 1))
                    }
                },
                '^' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        nodes.push(Token::BitwiseXorAssign(self.line, self.column - 2))
                    } else {
                        nodes.push(Token::BitwiseXor(self.line, self.column - 1))
                    }
                },
                ';' => {
                    self.advance();
                    nodes.push(Token::Semicolon(self.line, self.column - 1))
                },
                ',' => {
                    self.advance();
                    nodes.push(Token::Comma(self.line, self.column - 1))
                },
                '~' => {
                    self.advance();
                    nodes.push(Token::BitwiseInvert(self.line, self.column - 1))
                },
                _ => {
                    let _ = self.advance();
                }
            }
        }

        nodes.push(Token::EOF(self.line, self.column));
        Ok(nodes)
    }
}


#[cfg(test)]
mod lexical_analyzer_tests {
    use crate::parser::lexer::{PythonCoreLexer, Token};

    #[test]
    fn test_parenthesis() {
        let symbols = PythonCoreLexer::new("(([{}]))").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::LeftParen(1, 1),
            Token::LeftParen(1, 2),
            Token::LeftBracket(1, 3),
            Token::LeftCurly(1, 4),
            Token::RightCurly(1, 5),
            Token::RightBracket(1, 6),
            Token::RightParen(1, 7),
            Token::RightParen(1, 8),
            Token::EOF(1, 9)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(9, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_parenthesis_mismatch() {
        let symbols = PythonCoreLexer::new("[)").tokenize_source();

        match symbols {
            Ok(x) => {
                assert!(false)
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(2, e.column);
                assert_eq!("Mismatched parenthesis, found ')' without matching '('!".to_string(), e.message);
            }
        }
    }

    #[test]
    fn test_plus_operator() {
        let symbols = PythonCoreLexer::new("+").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Plus(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_plus_assign_operator() {
        let symbols = PythonCoreLexer::new("+=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::PlusAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_minus_operator() {
        let symbols = PythonCoreLexer::new("-").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Minus(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_arrow_operator() {
        let symbols = PythonCoreLexer::new("->").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Arrow(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_minus_assign_operator() {
        let symbols = PythonCoreLexer::new("-=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::MinusAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_star_operator() {
        let symbols = PythonCoreLexer::new("*").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Star(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_power_operator() {
        let symbols = PythonCoreLexer::new("**").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Power(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_power_assign_operator() {
        let symbols = PythonCoreLexer::new("**=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::PowerAssign(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_star_assign_operator() {
        let symbols = PythonCoreLexer::new("*=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::StarAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_slash_operator() {
        let symbols = PythonCoreLexer::new("/").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Slash(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_slash_slash_operator() {
        let symbols = PythonCoreLexer::new("//").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::SlashSlash(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_slash_slash_assign_operator() {
        let symbols = PythonCoreLexer::new("//=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::SlashSlashAssign(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_slash_assign_operator() {
        let symbols = PythonCoreLexer::new("/=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::SlashAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_modulo_operator() {
        let symbols = PythonCoreLexer::new("%").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Modulo(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_modulo_assign_operator() {
        let symbols = PythonCoreLexer::new("%=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::ModuloAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_decorator_operator() {
        let symbols = PythonCoreLexer::new("@").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Decorator(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_decorator_assign_operator() {
        let symbols = PythonCoreLexer::new("@=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::DecoratorAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_shift_left_operator() {
        let symbols = PythonCoreLexer::new("<<").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::ShiftLeft(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_shift_left_assign_operator() {
        let symbols = PythonCoreLexer::new("<<=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::ShiftLeftAssign(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_less_equal_operator() {
        let symbols = PythonCoreLexer::new("<=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::LessEqual(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_less_operator() {
        let symbols = PythonCoreLexer::new("<").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Less(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_shift_right_operator() {
        let symbols = PythonCoreLexer::new(">>").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::ShiftRight(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_shift_right_assign_operator() {
        let symbols = PythonCoreLexer::new(">>=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::ShiftRightAssign(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_greater_equal_operator() {
        let symbols = PythonCoreLexer::new(">=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::GreaterEqual(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_greater_operator() {
        let symbols = PythonCoreLexer::new(">").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Greater(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_assign_operator() {
        let symbols = PythonCoreLexer::new("=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Assign(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_equal_operator() {
        let symbols = PythonCoreLexer::new("==").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Equal(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_bang_operator() {
        let symbols = PythonCoreLexer::new("!").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Bang(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_not_equal_operator() {
        let symbols = PythonCoreLexer::new("!=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::NotEqual(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_colon_operator() {
        let symbols = PythonCoreLexer::new(":").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Colon(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_colon_assign_operator() {
        let symbols = PythonCoreLexer::new(":=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::ColonEqual(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_bitwise_and_operator() {
        let symbols = PythonCoreLexer::new("&").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::BitwiseAnd(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_bitwise_and_assign_operator() {
        let symbols = PythonCoreLexer::new("&=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::BitwiseAndAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_bitwise_or_operator() {
        let symbols = PythonCoreLexer::new("|").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::BitwiseOr(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_bitwise_or_assign_operator() {
        let symbols = PythonCoreLexer::new("|=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::BitwiseOrAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_bitwise_xor_operator() {
        let symbols = PythonCoreLexer::new("^").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::BitwiseXor(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_bitwise_xor_assign_operator() {
        let symbols = PythonCoreLexer::new("^=").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::BitwiseXorAssign(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_semicolon_operator() {
        let symbols = PythonCoreLexer::new(";").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Semicolon(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_comma_operator() {
        let symbols = PythonCoreLexer::new(",").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Comma(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_bitwise_invert_operator() {
        let symbols = PythonCoreLexer::new("~").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::BitwiseInvert(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(e) => {
                assert!(false)
            }
        }
    }
}
