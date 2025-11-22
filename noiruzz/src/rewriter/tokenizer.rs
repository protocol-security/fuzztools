use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number,
    Boolean,
    Identifier,
    Operator,
    ParenthesisLeft,
    ParenthesisRight,
    CurlyParenthesisLeft,
    CurlyParenthesisRight,
    QuestionMark,
    Dollar,
    Colon,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Number => write!(f, "NUMBER"),
            TokenKind::Boolean => write!(f, "BOOLEAN"),
            TokenKind::Identifier => write!(f, "IDENTIFIER"),
            TokenKind::Operator => write!(f, "OPERATOR"),
            TokenKind::ParenthesisLeft => write!(f, "PARENTHESIS_LEFT"),
            TokenKind::ParenthesisRight => write!(f, "PARENTHESIS_RIGHT"),
            TokenKind::CurlyParenthesisLeft => write!(f, "CURLY_PARENTHESIS_LEFT"),
            TokenKind::CurlyParenthesisRight => write!(f, "CURLY_PARENTHESIS_RIGHT"),
            TokenKind::QuestionMark => write!(f, "QUESTION_MARK"),
            TokenKind::Dollar => write!(f, "DOLLAR"),
            TokenKind::Colon => write!(f, "COLON"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Number(i64),
    Boolean(bool),
    String(String),
    None,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: TokenValue,
    pub pos_start: usize,
    pub pos_end: usize,
}

impl Token {
    pub fn new(kind: TokenKind, value: TokenValue, pos_start: usize, pos_end: usize) -> Self {
        Self { kind, value, pos_start, pos_end }
    }
}

pub struct Tokenizer {
    pattern: String,
    pos: usize,
}

impl Tokenizer {
    pub fn new() -> Self {
        Self { pattern: String::new(), pos: 0 }
    }

    pub fn tokenize(&mut self, pattern: &str) -> Result<Vec<Token>, String> {
        self.pattern = pattern.to_string();
        self.pos = 0;
        let mut tokens = Vec::new();

        while self.pos < self.pattern.len() {
            self.skip_whitespace();
            if self.pos >= self.pattern.len() {
                break;
            }

            let token = self.next_token()?;
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.pattern.len()
            && self.pattern.chars().nth(self.pos).unwrap().is_whitespace()
        {
            self.pos += 1;
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.pattern.chars().nth(self.pos + offset)
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek(0);
        self.pos += 1;
        ch
    }

    fn next_token(&mut self) -> Result<Token, String> {
        let start_pos = self.pos;
        let ch = self.peek(0).ok_or_else(|| "Unexpected end of input".to_string())?;

        match ch {
            '(' => {
                self.advance();
                Ok(Token::new(TokenKind::ParenthesisLeft, TokenValue::None, start_pos, self.pos))
            },
            ')' => {
                self.advance();
                Ok(Token::new(TokenKind::ParenthesisRight, TokenValue::None, start_pos, self.pos))
            },
            '{' => {
                self.advance();
                Ok(Token::new(
                    TokenKind::CurlyParenthesisLeft,
                    TokenValue::None,
                    start_pos,
                    self.pos,
                ))
            },
            '}' => {
                self.advance();
                Ok(Token::new(
                    TokenKind::CurlyParenthesisRight,
                    TokenValue::None,
                    start_pos,
                    self.pos,
                ))
            },
            '?' => {
                self.advance();
                Ok(Token::new(TokenKind::QuestionMark, TokenValue::None, start_pos, self.pos))
            },
            '$' => {
                self.advance();
                Ok(Token::new(TokenKind::Dollar, TokenValue::None, start_pos, self.pos))
            },
            ':' => {
                self.advance();
                Ok(Token::new(TokenKind::Colon, TokenValue::None, start_pos, self.pos))
            },
            '-' if self.peek(1).map(|c| c.is_ascii_digit()).unwrap_or(false) => self.parse_number(),
            '0'..='9' => self.parse_number(),
            'T' | 'F'
                if !self.peek(1).map(|c| c.is_alphanumeric() || c == '_').unwrap_or(false) =>
            {
                self.parse_boolean()
            },
            'a'..='z' | 'A'..='Z' | '_' => self.parse_identifier_or_keyword(),
            '+' | '*' | '/' | '%' | '^' | '&' | '|' | '!' | '<' | '>' | '=' => {
                self.parse_operator()
            },
            _ => Err(format!("Unexpected character '{}' at position {}", ch, self.pos)),
        }
    }

    fn parse_number(&mut self) -> Result<Token, String> {
        let start_pos = self.pos;
        let mut num_str = String::new();

        if self.peek(0) == Some('-') {
            num_str.push('-');
            self.advance();
        }

        while let Some(ch) = self.peek(0) {
            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let number = num_str
            .parse::<i64>()
            .map_err(|_| format!("Invalid number '{}' at position {}", num_str, start_pos))?;

        Ok(Token::new(TokenKind::Number, TokenValue::Number(number), start_pos, self.pos))
    }

    fn parse_boolean(&mut self) -> Result<Token, String> {
        let start_pos = self.pos;
        let ch = self.advance().unwrap();
        let value = ch == 'T';
        Ok(Token::new(TokenKind::Boolean, TokenValue::Boolean(value), start_pos, self.pos))
    }

    fn parse_identifier_or_keyword(&mut self) -> Result<Token, String> {
        let start_pos = self.pos;
        let mut identifier = String::new();

        while let Some(ch) = self.peek(0) {
            if ch.is_alphanumeric() || ch == '_' {
                identifier.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for keywords like "true", "false", "assert"
        match identifier.as_str() {
            "true" => {
                Ok(Token::new(TokenKind::Boolean, TokenValue::Boolean(true), start_pos, self.pos))
            },
            "false" => {
                Ok(Token::new(TokenKind::Boolean, TokenValue::Boolean(false), start_pos, self.pos))
            },
            _ => Ok(Token::new(
                TokenKind::Identifier,
                TokenValue::String(identifier),
                start_pos,
                self.pos,
            )),
        }
    }

    fn parse_operator(&mut self) -> Result<Token, String> {
        let start_pos = self.pos;
        let mut op = String::new();

        // Try to match multi-character operators first
        let ch1 = self.peek(0).unwrap();
        let ch2 = self.peek(1);

        // Check for two-character operators
        let two_char = match (ch1, ch2) {
            ('=', Some('=')) => Some("=="),
            ('!', Some('=')) => Some("!="),
            ('<', Some('=')) => Some("<="),
            ('>', Some('=')) => Some(">="),
            ('<', Some('<')) => Some("<<"),
            ('>', Some('>')) => Some(">>"),
            ('&', Some('&')) => Some("&&"),
            ('|', Some('|')) => Some("||"),
            ('^', Some('^')) => Some("^^"),
            ('*', Some('*')) => Some("**"),
            _ => None,
        };

        if let Some(two_char_op) = two_char {
            op = two_char_op.to_string();
            self.advance();
            self.advance();
        } else {
            op.push(ch1);
            self.advance();
        }

        Ok(Token::new(TokenKind::Operator, TokenValue::String(op), start_pos, self.pos))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple_expression() {
        let mut tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("(?a + ?b)").unwrap();
        assert_eq!(tokens.len(), 6);
    }

    #[test]
    fn test_tokenize_with_numbers() {
        let mut tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("(?a + 42)").unwrap();
        assert_eq!(tokens.len(), 6);
        assert!(matches!(tokens[3].kind, TokenKind::Number));
    }

    #[test]
    fn test_tokenize_with_boolean() {
        let mut tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("(?a & T)").unwrap();
        assert_eq!(tokens.len(), 6);
        assert!(matches!(tokens[3].kind, TokenKind::Boolean));
    }
}
