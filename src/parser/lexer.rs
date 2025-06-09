use crate::parser::errors::SyntaxError;
use crate::parser::tokens::Token;

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

    fn is_identifier_start(&self, ch: char) -> bool {
        match ch {
            '_' => true,
            'a'..='z' | 'A'..='Z' => true,
            _ => false
        }
    }

    fn is_identifier_char(&self, ch: char) -> bool {
        match ch {
            '_' => true,
            'a'..='z' | 'A'..='Z' => true,
            '0'..='9' => true,
            _ => false
        }
    }

    fn handle_imaginary(&mut self) -> Option<String> {
        let mut text = String::new();
        match self.peek() {
            Some('j') => text.push(self.advance()?),
            Some('J') => text.push(self.advance()?),
            _ => return None
        }
        Some(text)
    }

    fn handle_exponent(&mut self) -> Result<String, SyntaxError> {
        let mut text = String::new();

        match self.peek() {
            Some('e') => {
                self.advance();
                text.push('e');
            },
            Some('E') => {
                self.advance();
                text.push('E');
            },
            _ => return Ok(text)
        }

        match self.peek() {
            Some('+') => {
                self.advance();
                text.push('+');
            },
            Some('-') => {
                self.advance();
                text.push('-');
            },
            _ => ()
        }

        let check = self.peek();
        match check {
            Some(ch) if ch >= '0' && ch <= '9' => {
                text.push(ch);
                self.advance();

                loop {
                    while let Some(ch) = self.peek() {
                        match ch {
                            '0'..='9' => {
                                text.push(ch);
                                self.advance();
                            },
                            _ => break
                        }
                    }

                    match self.peek() {
                        Some('_') => {
                            text.push('_');
                            self.advance();
                        },
                        _ => break
                    }

                    match self.peek() {
                        Some(ch) if ch >= '0' && ch <= '9' => (),
                        _ => return Err(SyntaxError::new(self.line, self.column, String::from("Invalid number")))
                    }

                }
            },
            _ => return Err(SyntaxError::new(self.line, self.column, String::from("Invalid number literal in exponent part!")))
        }

        Ok(text)
    }

    fn handle_fraction(&mut self, dotted_number: bool) -> Result<String, SyntaxError> {
        let mut text = String::new();

        match dotted_number {
            true => (),
            _ => {
                text.push('.');
                self.advance();
            }
        }

        let check = self.peek();
        match check {
            Some(ch) => {
                match ch {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        text.push(ch);
                        self.advance();
                    },
                    _ => {}
                }
            },
            _ => {}
        }

        match self.peek() {
            Some(_) => {
                loop {
                    while let Some(ch) = self.peek() {
                        match ch {
                            '0'..='9' => {
                                text.push(ch);
                                self.advance();
                            },
                            _ => break
                        }
                    }

                    match self.peek() {
                        Some('_') => {
                            text.push('_');
                            self.advance();
                        },
                        _ => break
                    }

                    match self.peek() {
                        Some(ch) if ch >= '0' && ch <= '9' => (),
                        _ => return Err(SyntaxError::new(self.line, self.column, String::from("Invalid number")))
                    }

                }
            },
            None => ()
        }

        Ok(text)
    }

    fn handle_numbers(&mut self, prefix_dot: Option<char>, line: usize, column: usize) -> Result<Token, SyntaxError> {
        let mut text = String::new();
        let dotted_number = match prefix_dot {
            Some(_) => {
                text.push('.');
                true
            },
            _ => false
        };

        let is_hex_digit = |ch: char| -> bool { ch.is_ascii_hexdigit()};

        /* Handle main number loop */
        match dotted_number{
            true => {
                text.push_str(self.handle_fraction(dotted_number)?.as_str());

                if self.peek() == Some('e') || self.peek() == Some('E') {
                    text.push_str(self.handle_exponent()?.as_str());
                }

                match self.handle_imaginary() {
                    Some(text2) => text.push_str(&text2),
                    _ => ()
                }
            },
            _ => {
                match self.peek() {
                    Some('0') => {
                        text.push('0');
                        self.advance();
                        match self.peek() {
                            Some('x') | Some('X') => {
                                text.push(self.advance().unwrap());

                                loop {
                                    if self.peek() == Some('_') {
                                        text.push('_');
                                        self.advance();
                                    }

                                    let check = self.peek();
                                    match check {
                                        Some(ch) if is_hex_digit(ch) => (),
                                        _ => return Err(SyntaxError::new(line, column, String::from("Expecting digit after '_' in hexadecimal number!")))
                                    }

                                    while let Some(ch) = self.peek() {
                                        match ch {
                                            ch if is_hex_digit(ch) => {
                                                text.push(self.advance().unwrap());
                                            },
                                            _ => break
                                        }
                                    }

                                    if self.peek() != Some('_') { break; }
                                }
                            },
                            Some('o') | Some('O') => {
                                text.push(self.advance().unwrap());

                                loop {
                                    if self.peek() == Some('_') {
                                        text.push('_');
                                        self.advance();
                                    }

                                    let check = self.peek();
                                    match check {
                                        Some(ch) if ch.is_digit(8) => (),
                                        _ => return Err(SyntaxError::new(line, column, String::from("Expecting digit after '_' in octet number!")))
                                    }

                                    while let Some(ch) = self.peek() {
                                        match ch {
                                            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => {
                                                text.push(self.advance().unwrap());
                                            },
                                            _ => break
                                        }
                                    }

                                    if self.peek() != Some('_') { break; }
                                }
                            },
                            Some('b') | Some('B') => {
                                text.push(self.advance().unwrap());

                                loop {
                                    if self.peek() == Some('_') {
                                        text.push('_');
                                        self.advance();
                                    }

                                    if self.peek() != Some('0') && self.peek() != Some('1') { return Err(SyntaxError::new(self.line, self.column, String::from("Expecting '0' or '1' in binary number!"))); }

                                    while let Some(ch) = self.peek() {
                                        match ch {
                                            '0' | '1' => {
                                                text.push(self.advance().unwrap());
                                            },
                                            _ => break
                                        }
                                    }

                                    if self.peek() != Some('_') { break; }
                                }
                            },
                            _ => {
                                match self.peek() {
                                    Some('.') => {
                                        text.push_str(self.handle_fraction(dotted_number)?.as_str());

                                        if self.peek() == Some('e') || self.peek() == Some('E') {
                                            text.push_str(self.handle_exponent()?.as_str());
                                        }

                                        match self.handle_imaginary() {
                                            Some(text2) => text.push_str(&text2),
                                            _ => ()
                                        }
                                    },
                                    Some('e') | Some('E') => {
                                        if self.peek() == Some('e') || self.peek() == Some('E') {
                                            text.push_str(self.handle_exponent()?.as_str());
                                        }

                                        match self.handle_imaginary() {
                                            Some(text2) => text.push_str(&text2),
                                            _ => ()
                                        }
                                    },
                                    Some('j') | Some('J') => {
                                        match self.handle_imaginary() {
                                            Some(text2) => text.push_str(&text2),
                                            _ => ()
                                        }
                                    },
                                    Some(ch) if ch.is_digit(8) => {
                                        return Err(SyntaxError::new(self.line, self.column, String::from("Old style octet staring with zero is not allowed!")))
                                    },
                                    _ => ()
                                }
                            }
                        }
                    },
                    _ => {
                        match self.peek() {
                            Some(_) => {
                                loop {
                                    while let Some(ch) = self.peek() {
                                        match ch {
                                            '0'..='9' => {
                                                text.push(ch);
                                                self.advance();
                                            },
                                            _ => break
                                        }
                                    }

                                    match self.peek() {
                                        Some('_') => {
                                            text.push('_');
                                            self.advance();
                                        },
                                        _ => break
                                    }

                                    match self.peek() {
                                        Some(ch) if ch >= '0' && ch <= '9' => (),
                                        _ => return Err(SyntaxError::new(self.line, self.column, String::from("Invalid number")))
                                    }

                                }
                            },
                            None => ()
                        }

                        match self.peek() {
                            Some('.') => text.push_str(self.handle_fraction(dotted_number)?.as_str()),
                            _ => ()
                        }

                        if self.peek() == Some('e') || self.peek() == Some('E') {
                            text.push_str(self.handle_exponent()?.as_str());
                        }

                        match self.handle_imaginary() {
                            Some(text2) => text.push_str(&text2),
                            _ => ()
                        }
                    }
                }
            }
        }

        Ok(Token::Number(line, column, text))
    }

    fn handle_strings(&mut self, prefix: Option<String>, ch: char, start_line: usize, start_column: usize) -> Result<Token, SyntaxError> {
        let mut text = String::new();
        let mut is_tripple = false;
        let mut complete = false;
        match prefix {
            Some(prefix) => {
                text += &prefix
            },
            _ => ()
        }
        text.push(ch);
        self.advance();

        /* Handle start of strings and possible empty strings */
        if let Some(ch2) = self.peek() {
            match ch2 {
                '\'' if ch2 == ch  => {
                    text.push(ch2);
                    self.advance();
                    let ch3 = self.peek();
                    match ch3 {
                        Some('\'') => {
                            text.push('\'');
                            self.advance();
                            is_tripple = true;
                        },
                        _ => {
                            return Ok(Token::String(start_line, start_column, text))
                        }
                    }
                },
                '"' if ch2 == ch => {
                    text.push(ch2);
                    self.advance();
                    let ch3 = self.peek();
                    match ch3 {
                        Some('"') => {
                            text.push('"');
                            self.advance();
                            is_tripple = true;
                        },
                        _ => {
                            return Ok(Token::String(start_line, start_column, text))
                        }
                    }
                },
                _ => ()
            }
        }

        /* Handle rest of string including terminating quotes */
        while let Some(ch4) = self.peek() {
            match ch4 {
                '\'' if ch4 == ch  => {
                    text.push(ch4);
                    self.advance();

                    if !is_tripple {
                        complete = true;
                        break
                    }

                    match self.peek() {
                        Some('\'') if is_tripple => {
                            text.push('\'');
                            self.advance();

                            match self.peek() {
                                Some('\'') if is_tripple => {
                                    text.push('\'');
                                    self.advance();
                                    complete = true
                                },
                                _ => {
                                    continue
                                }
                            }
                        },
                        _ => {
                            complete = true
                        }
                    }
                },
                '"' if ch4 == ch => {
                    text.push(ch4);
                    self.advance();

                    if !is_tripple {
                        complete = true;
                        break
                    }

                    match self.peek() {
                        Some('"') if is_tripple => {
                            text.push('"');
                            self.advance();

                            match self.peek() {
                                Some('"') if is_tripple => {
                                    text.push('"');
                                    self.advance();
                                    complete = true
                                },
                                _ => {
                                    continue
                                }
                            }
                        },
                        _ => {
                            complete = true
                        }
                    }
                },
                '\\' => {
                    text.push(ch4);
                    self.advance();
                    let ch5 = self.peek();
                    match ch5 {
                        Some(ch6) => {
                            text.push(ch6);
                            self.advance();
                        },
                        _ => ()
                    }
                },
                _ => {
                    text.push(ch4);
                    self.advance();
                }
            }
        }

        if !complete {
            return Err(SyntaxError::new(start_line, start_column, "Unterminated string".to_string()));
        }

        Ok(Token::String(start_line, start_column, text))
    }



    fn tokenize_source(&mut self) -> Result<Vec<Token>, SyntaxError> {
        let mut nodes: Vec<Token> = Vec::new();

        while let Some(ch) = self.peek() {
            match ch {
                ' ' => {
                    self.advance();
                    continue
                },
                '\t' => {
                    self.advance();
                    continue
                },
                '\'' | '"' => {
                    nodes.push(self.handle_strings(None, ch, self.line, self.column)?);
                    continue;
                },
                ch if self.is_identifier_start(ch) => {
                    let pos = self.column;
                    let mut text = String::new();

                    while let Some(ch) = self.peek() {
                        if self.is_identifier_char(ch) {
                            text.push(ch);
                            self.advance();
                        } else {
                            break;
                        }
                    }

                    /* Check for prefix to string */
                    match text.as_str() {
                        "r" | "u" | "R" | "U" | "f" | "F" | "t" | "T"
                        | "fr" | "Fr" | "fR" | "FR" | "rf" | "rF" | "Rf" | "RF"
                        | "tr" | "Tr" | "tR" | "TR" | "rt" | "rT" | "Rt" | "RT" => {

                            let peek_char = self.peek();
                            match &peek_char {
                                Some('"') | Some('\'') => {
                                    nodes.push(self.handle_strings(Some(text), peek_char.unwrap(), self.line, pos)?);
                                    continue;
                                },
                                _ => ()
                            }
                        },
                        _ => ()
                    }

                    match text.as_str() {
                        "False" => nodes.push(Token::False(self.line, pos)),
                        "True" => nodes.push(Token::True(self.line, pos)),
                        "None" => nodes.push(Token::None(self.line, pos)),
                        "and" => nodes.push(Token::And(self.line, pos)),
                        "as" => nodes.push(Token::As(self.line, pos)),
                        "assert" => nodes.push(Token::Assert(self.line, pos)),
                        "async" => nodes.push(Token::Async(self.line, pos)),
                        "await" => nodes.push(Token::Await(self.line, pos)),
                        "break" => nodes.push(Token::Break(self.line, pos)),
                        "class" => nodes.push(Token::Class(self.line, pos)),
                        "continue" => nodes.push(Token::Continue(self.line, pos)),
                        "def" => nodes.push(Token::Def(self.line, pos)),
                        "del" => nodes.push(Token::Del(self.line, pos)),
                        "elif" => nodes.push(Token::Elif(self.line, pos)),
                        "else" => nodes.push(Token::Else(self.line, pos)),
                        "except" => nodes.push(Token::Except(self.line, pos)),
                        "finally" => nodes.push(Token::Finally(self.line, pos)),
                        "for" => nodes.push(Token::For(self.line, pos)),
                        "from" => nodes.push(Token::From(self.line, pos)),
                        "global" => nodes.push(Token::Global(self.line, pos)),
                        "if" => nodes.push(Token::If(self.line, pos)),
                        "import" => nodes.push(Token::Import(self.line, pos)),
                        "in" => nodes.push(Token::In(self.line, pos)),
                        "is" => nodes.push(Token::Is(self.line, pos)),
                        "lambda" => nodes.push(Token::Lambda(self.line, pos)),
                        "nonlocal" => nodes.push(Token::Nonlocal(self.line, pos)),
                        "not" => nodes.push(Token::Not(self.line, pos)),
                        "or" => nodes.push(Token::Or(self.line, pos)),
                        "pass" => nodes.push(Token::Pass(self.line, pos)),
                        "raise" => nodes.push(Token::Raise(self.line, pos)),
                        "return" => nodes.push(Token::Return(self.line, pos)),
                        "try" => nodes.push(Token::Try(self.line, pos)),
                        "while" => nodes.push(Token::While(self.line, pos)),
                        "with" => nodes.push(Token::With(self.line, pos)),
                        "yield" => nodes.push(Token::Yield(self.line, pos)),
                        _ => nodes.push(Token::Name(self.line, pos, text))
                    }
                },
                '0'..='9' | '.' => {
                    let pos = self.column;

                    if ch == '.' {
                        self.advance();

                        match self.peek() {
                            Some('0'..='9') => {
                                nodes.push(self.handle_numbers(Some('.'), self.line, pos)?);
                            },
                            _ => {
                                nodes.push(Token::Period(self.line, pos));
                            }
                        }
                        continue
                    }
                    nodes.push(self.handle_numbers(None, self.line, pos)?);
                }
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
    use crate::parser::lexer::{PythonCoreLexer, SyntaxError, Token};

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
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_parenthesis_mismatch() {
        let symbols = PythonCoreLexer::new("[)").tokenize_source();

        match symbols {
            Ok(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
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
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_periodt_operator() {
        let symbols = PythonCoreLexer::new(".").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Period(1, 1),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_name_literal() {
        let symbols = PythonCoreLexer::new("__init__").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Name(1, 1, "__init__".to_string()),
            Token::EOF(1, 9)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_false() {
        let symbols = PythonCoreLexer::new("False").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::False(1, 1),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_true() {
        let symbols = PythonCoreLexer::new("True").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::True(1, 1),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_none() {
        let symbols = PythonCoreLexer::new("None").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::None(1, 1),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_and() {
        let symbols = PythonCoreLexer::new("and").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::And(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_as() {
        let symbols = PythonCoreLexer::new("as").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::As(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_assert() {
        let symbols = PythonCoreLexer::new("assert").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Assert(1, 1),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_async() {
        let symbols = PythonCoreLexer::new("async").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Async(1, 1),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_await() {
        let symbols = PythonCoreLexer::new("await").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Await(1, 1),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_break() {
        let symbols = PythonCoreLexer::new("break").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Break(1, 1),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_class() {
        let symbols = PythonCoreLexer::new("class").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Class(1, 1),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_continue() {
        let symbols = PythonCoreLexer::new("continue").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Continue(1, 1),
            Token::EOF(1, 9)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_def() {
        let symbols = PythonCoreLexer::new("def").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Def(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_del() {
        let symbols = PythonCoreLexer::new("del").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Del(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_elif() {
        let symbols = PythonCoreLexer::new("elif").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Elif(1, 1),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_except() {
        let symbols = PythonCoreLexer::new("except").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Except(1, 1),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_finally() {
        let symbols = PythonCoreLexer::new("finally").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Finally(1, 1),
            Token::EOF(1, 8)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_for() {
        let symbols = PythonCoreLexer::new("for").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::For(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_from() {
        let symbols = PythonCoreLexer::new("from").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::From(1, 1),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_global() {
        let symbols = PythonCoreLexer::new("global").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Global(1, 1),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_if() {
        let symbols = PythonCoreLexer::new("if").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::If(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_import() {
        let symbols = PythonCoreLexer::new("import").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Import(1, 1),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_in() {
        let symbols = PythonCoreLexer::new("in").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::In(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_is() {
        let symbols = PythonCoreLexer::new("is").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Is(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_lambda() {
        let symbols = PythonCoreLexer::new("lambda").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Lambda(1, 1),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_nonlocal() {
        let symbols = PythonCoreLexer::new("nonlocal").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Nonlocal(1, 1),
            Token::EOF(1, 9)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_not() {
        let symbols = PythonCoreLexer::new("not").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Not(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_or() {
        let symbols = PythonCoreLexer::new("or").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Or(1, 1),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_pass() {
        let symbols = PythonCoreLexer::new("pass").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Pass(1, 1),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_raise() {
        let symbols = PythonCoreLexer::new("raise").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Raise(1, 1),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_return() {
        let symbols = PythonCoreLexer::new("return").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Return(1, 1),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_try() {
        let symbols = PythonCoreLexer::new("try").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Try(1, 1),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_while() {
        let symbols = PythonCoreLexer::new("while").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::While(1, 1),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_with() {
        let symbols = PythonCoreLexer::new("with").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::With(1, 1),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_keyword_yield() {
        let symbols = PythonCoreLexer::new("yield").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Yield(1, 1),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_whitespace_between_tokens() {
        let symbols = PythonCoreLexer::new("yield a").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Yield(1, 1),
            Token::Name(1, 7, "a".to_string()),
            Token::EOF(1, 8)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(3, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_sinqle_quote_string_token() {
        let symbols = PythonCoreLexer::new("''").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("''")),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_token() {
        let symbols = PythonCoreLexer::new("\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("\"\"")),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_r_token() {
        let symbols = PythonCoreLexer::new("r\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("r\"\"")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_capital_r_token() {
        let symbols = PythonCoreLexer::new("R\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("R\"\"")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_u_token() {
        let symbols = PythonCoreLexer::new("u\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("u\"\"")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_capital_u_token() {
        let symbols = PythonCoreLexer::new("U\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("U\"\"")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_f_token() {
        let symbols = PythonCoreLexer::new("f\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("f\"\"")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_capital_f_token() {
        let symbols = PythonCoreLexer::new("F\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("F\"\"")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_t_token() {
        let symbols = PythonCoreLexer::new("t\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("t\"\"")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_capital_t_token() {
        let symbols = PythonCoreLexer::new("T\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("T\"\"")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_fr1_token() {
        let symbols = PythonCoreLexer::new("fr\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("fr\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_fr2_token() {
        let symbols = PythonCoreLexer::new("Fr\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("Fr\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_fr3_token() {
        let symbols = PythonCoreLexer::new("fR\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("fR\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_fr4_token() {
        let symbols = PythonCoreLexer::new("FR\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("FR\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_fr5_token() {
        let symbols = PythonCoreLexer::new("rf\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("rf\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_fr6_token() {
        let symbols = PythonCoreLexer::new("rF\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("rF\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_fr7_token() {
        let symbols = PythonCoreLexer::new("Rf\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("Rf\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_fr8_token() {
        let symbols = PythonCoreLexer::new("RF\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("RF\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_tr1_token() {
        let symbols = PythonCoreLexer::new("tr\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("tr\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_tr2_token() {
        let symbols = PythonCoreLexer::new("Tr\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("Tr\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_tr3_token() {
        let symbols = PythonCoreLexer::new("tR\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("tR\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_tr4_token() {
        let symbols = PythonCoreLexer::new("TR\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("TR\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_tr5_token() {
        let symbols = PythonCoreLexer::new("rt\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("rt\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_tr6_token() {
        let symbols = PythonCoreLexer::new("rT\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("rT\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_tr7_token() {
        let symbols = PythonCoreLexer::new("Rt\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("Rt\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_double_quote_string_with_prefix_tr8_token() {
        let symbols = PythonCoreLexer::new("RT\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("RT\"\"")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_triple_double_quote_string_token() {
        let symbols = PythonCoreLexer::new("\"\"\"\"\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("\"\"\"\"\"\"")),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_triple_single_quote_string_token() {
        let symbols = PythonCoreLexer::new("''''''").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("''''''")),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_triple_double_quote_string_token() {
        let symbols = PythonCoreLexer::new("\"\"\"Hello, World!\"\"\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("\"\"\"Hello, World!\"\"\"")),
            Token::EOF(1, 20)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_triple_single_quote_string_token() {
        let symbols = PythonCoreLexer::new("'''Hello, World!'''").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("'''Hello, World!'''")),
            Token::EOF(1, 20)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_double_quote_string_token() {
        let symbols = PythonCoreLexer::new("\"Hello, World!\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("\"Hello, World!\"")),
            Token::EOF(1, 16)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_single_quote_string_token() {
        let symbols = PythonCoreLexer::new("'Hello, World!'").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("'Hello, World!'")),
            Token::EOF(1, 16)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_multiple_string_single_quote_token() {
        let symbols = PythonCoreLexer::new("'Hello, World!''Test'").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("'Hello, World!'")),
            Token::String(1, 16, String::from("'Test'")),
            Token::EOF(1, 22)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(3, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_multiple_string_double_quote_token() {
        let symbols = PythonCoreLexer::new("\"Hello, World!\"\"Test\"").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::String(1, 1, String::from("\"Hello, World!\"")),
            Token::String(1, 16, String::from("\"Test\"")),
            Token::EOF(1, 22)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(3, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_unterminated_double_quote_string_token() {
        let symbols = PythonCoreLexer::new("\"Hello, World!").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Unterminated string"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(1, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_unterminated_single_quote_string_token() {
        let symbols = PythonCoreLexer::new("'Hello, World!").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Unterminated string"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(1, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_unterminated_triple_double_quote_string_token() {
        let symbols = PythonCoreLexer::new("\"\"\"Hello, World!").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Unterminated string"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(1, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_unterminated_triple_single_quote_string_token() {
        let symbols = PythonCoreLexer::new("'''Hello, World!").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Unterminated string"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(1, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_binary_number_1_token() {
        let symbols = PythonCoreLexer::new("0b1101").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0b1101")),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_binary_number_2_token() {
        let symbols = PythonCoreLexer::new("0B1101").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0B1101")),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_binary_number_3_token() {
        let symbols = PythonCoreLexer::new("0b_11_0_1").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0b_11_0_1")),
            Token::EOF(1, 10)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_binary_number_error_1_token() {
        let symbols = PythonCoreLexer::new("0b_").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Expecting '0' or '1' in binary number!"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(4, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_binary_number_error_2_token() {
        let symbols = PythonCoreLexer::new("0b_7").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Expecting '0' or '1' in binary number!"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(4, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_octet_number_1_token() {
        let symbols = PythonCoreLexer::new("0o071").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0o071")),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_octet_number_2_token() {
        let symbols = PythonCoreLexer::new("0O167").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0O167")),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_octet_number_3_token() {
        let symbols = PythonCoreLexer::new("0o_17_6_3").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0o_17_6_3")),
            Token::EOF(1, 10)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_octet_number_error_1_token() {
        let symbols = PythonCoreLexer::new("0o0_").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Expecting digit after '_' in octet number!"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(1, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_octet_number_error_2_token() {
        let symbols = PythonCoreLexer::new("0o_8").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Expecting digit after '_' in octet number!"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(1, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_hex_number_1_token() {
        let symbols = PythonCoreLexer::new("0x7f").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0x7f")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_hex_number_2_token() {
        let symbols = PythonCoreLexer::new("0X7F").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0X7F")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_hex_number_3_token() {
        let symbols = PythonCoreLexer::new("0x_FA_7c").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0x_FA_7c")),
            Token::EOF(1, 9)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_hex_number_error_1_token() {
        let symbols = PythonCoreLexer::new("0x0_").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Expecting digit after '_' in hexadecimal number!"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(1, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_hex_number_error_2_token() {
        let symbols = PythonCoreLexer::new("0X_Z").tokenize_source();

        let expected = SyntaxError::new(1, 1, String::from("Expecting digit after '_' in hexadecimal number!"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(1, e.line);
                assert_eq!(1, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_dot_number_j_token() {
        let symbols = PythonCoreLexer::new(".1j").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".1j")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_dot_number_j2_token() {
        let symbols = PythonCoreLexer::new(".0J").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".0J")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_dot_number_simple_token() {
        let symbols = PythonCoreLexer::new(".0").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".0")),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_dot_number_simple_underscore_token() {
        let symbols = PythonCoreLexer::new(".1_0").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".1_0")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_dot_number_with_exponent_underscore_token() {
        let symbols = PythonCoreLexer::new(".1_0e-4").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".1_0e-4")),
            Token::EOF(1, 8)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_dot_number_with_exponent_with_j_underscore_token() {
        let symbols = PythonCoreLexer::new(".1_0e-4J").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".1_0e-4J")),
            Token::EOF(1, 9)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_dot_number_with_exponent_plus_underscore_token() {
        let symbols = PythonCoreLexer::new(".1_0E+4").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".1_0E+4")),
            Token::EOF(1, 8)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_dot_number_with_exponent_with_plus_j_underscore_token() {
        let symbols = PythonCoreLexer::new(".1_0E+4J").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".1_0E+4J")),
            Token::EOF(1, 9)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_dot_number_with_exponent_with_underscore_token() {
        let symbols = PythonCoreLexer::new(".1_0E45").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from(".1_0E45")),
            Token::EOF(1, 8)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_zero_number_token() {
        let symbols = PythonCoreLexer::new("0").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0")),
            Token::EOF(1, 2)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_zero_dot_zero_number_token() {
        let symbols = PythonCoreLexer::new("0.0").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0.0")),
            Token::EOF(1, 4)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_zero_dot_number_with_exponent_token() {
        let symbols = PythonCoreLexer::new("0.134e-34").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0.134e-34")),
            Token::EOF(1, 10)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_zero_dot_number_with_exponent_and_j_token() {
        let symbols = PythonCoreLexer::new("0.134e-34J").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0.134e-34J")),
            Token::EOF(1, 11)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_zero_number_error_token() {
        let symbols = PythonCoreLexer::new("07").tokenize_source();

        let expected = SyntaxError::new(1, 2, String::from("Old style octet staring with zero is not allowed!"));

        match symbols {
            Ok(_) => {
                assert!(false);
            },
            Err(e) => {
                assert_eq!(expected.line, e.line);
                assert_eq!(expected.column, e.column);
                assert_eq!(expected.message, e.message)
            }
        }
    }

    #[test]
    fn test_number_zero_j_token() {
        let symbols = PythonCoreLexer::new("0j").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("0j")),
            Token::EOF(1, 3)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_number_integer_token() {
        let symbols = PythonCoreLexer::new("1000").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("1000")),
            Token::EOF(1, 5)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_number_integer_imaginary_token() {
        let symbols = PythonCoreLexer::new("1000J").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("1000J")),
            Token::EOF(1, 6)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn test_number_integer_exp_token() {
        let symbols = PythonCoreLexer::new("1.0E34").tokenize_source();

        let expected: Vec<Token> = vec![
            Token::Number(1, 1, String::from("1.0E34")),
            Token::EOF(1, 7)
        ];

        match symbols {
            Ok(x) => {
                assert_eq!(2, x.len());
                assert_eq!(expected, x);
            },
            Err(_) => {
                assert!(false)
            }
        }
    }
}
