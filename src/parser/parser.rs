use crate::parser::tokens::Token;
use crate::parser::ast::Node;
use crate::parser::errors::SyntaxError;

pub struct PythonCoreParser {
    symbols: Vec<Token>,
    current_symbol_index: usize
}

impl PythonCoreParser {
    pub fn new() -> Self {
        PythonCoreParser {
            symbols: Vec::new(),
            current_symbol_index: 0
        }
    }
    
    pub fn set_symbols(&mut self, symbols: Vec<Token>) -> &mut Self {
        self.symbols = symbols;
        self
    }
    
    pub fn parse_eval_input(&mut self) -> Result<Node, SyntaxError> {
        Ok(Node::Eof)
    }
}

#[cfg(test)]
mod parser_entry_tests {
    use crate::parser::ast::Node;
    use crate::parser::errors::SyntaxError;
    use crate::parser::lexer::PythonCoreLexer;
    use crate::parser::parser::PythonCoreParser;

    #[test]
    fn test_parser_setup() {
        let root = PythonCoreParser::new()
                        .set_symbols(PythonCoreLexer::new("")
                            .set_tab_size(8)
                            .tokenize_source()
                            .unwrap_or_default())
                        .parse_eval_input();
        
        let expected : Result<Node, SyntaxError> = Ok(Node::Eof);
        
        match (root, expected) {
            (Ok(expected), Ok(actual)) => {
                assert_eq!(expected, actual)
            },
            _ => assert!(false)
        }
    }
}