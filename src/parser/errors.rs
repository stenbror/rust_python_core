pub struct SyntaxError {
    pub line: usize,
    pub column: usize,
    pub message: String
}

impl SyntaxError {
    pub fn new(line: usize, column: usize, message: String) -> Self {
        SyntaxError { line: line, column: column, message: message }
    }
}

#[cfg(test)]
mod test_syntax_error {
    use crate::parser::errors::SyntaxError;
    
    #[test]
    fn test_syntax_error() {
        let failure = SyntaxError::new(1, 1, "foo".to_string());
        
        assert_eq!(1, failure.line);
        assert_eq!(1, failure.column);
        assert_eq!(String::from("foo"), failure.message);
    }
}