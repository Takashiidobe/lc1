#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Keywords
    If,
    Else,
    Fn,
    Let,
    Return,
    Print,
    Struct,

    // Identifiers and literals
    Ident(String),
    Int(i64),
    Str(String),

    // Arithmetic
    Plus,
    Minus,

    // Operators
    Assign,
    Arrow, // '->'

    // Relational Ops
    Lt,
    Le,
    Eq,
    Ne,
    Gt,
    Ge,

    // Delimiters
    LBracket,
    RBracket,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Semi,
    Dot,

    Eof,
}

impl<'input> Lexer<'input> {
    pub fn new(src: &'input str) -> Self {
        let mut lx = Lexer {
            input: src,
            pos: 0,
            ch: None,
        };
        lx.read_char();
        lx
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            tokens.push(tok.clone());
            if tok == Token::Eof {
                break;
            }
        }
        tokens
    }
}

pub struct Lexer<'input> {
    input: &'input str,
    pos: usize,
    ch: Option<char>,
}

impl Lexer<'_> {
    fn read_char(&mut self) {
        self.ch = self.input[self.pos..].chars().next();
        if let Some(c) = self.ch {
            self.pos += c.len_utf8();
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.ch, Some(c) if c.is_whitespace()) {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(c) = self.ch {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.read_char();
            } else {
                break;
            }
        }
        ident
    }

    fn read_number(&mut self) -> i64 {
        let mut num_str = String::new();
        while let Some(c) = self.ch {
            if c.is_ascii_digit() {
                num_str.push(c);
                self.read_char();
            } else {
                break;
            }
        }
        num_str.parse().unwrap()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        // two‐char arrow
        if self.ch == Some('-') && self.peek_char() == Some('>') {
            self.read_char();
            self.read_char();
            return Token::Arrow;
        }

        if self.ch == Some('=') && self.peek_char() == Some('=') {
            self.read_char();
            self.read_char();
            return Token::Eq;
        }
        if self.ch == Some('!') && self.peek_char() == Some('=') {
            self.read_char();
            self.read_char();
            return Token::Ne;
        }
        if self.ch == Some('<') && self.peek_char() == Some('=') {
            self.read_char();
            self.read_char();
            return Token::Le;
        }
        if self.ch == Some('>') && self.peek_char() == Some('=') {
            self.read_char();
            self.read_char();
            return Token::Ge;
        }

        let tok = match self.ch {
            Some('=') => {
                self.read_char();
                Token::Assign
            }
            Some('+') => {
                self.read_char();
                Token::Plus
            }
            Some('-') => {
                self.read_char();
                Token::Minus
            }
            Some('<') => {
                self.read_char();
                Token::Lt
            }
            Some('>') => {
                self.read_char();
                Token::Gt
            }
            Some('(') => {
                self.read_char();
                Token::LParen
            }
            Some(')') => {
                self.read_char();
                Token::RParen
            }
            Some('[') => {
                self.read_char();
                Token::LBracket
            }
            Some(']') => {
                self.read_char();
                Token::RBracket
            }
            Some('{') => {
                self.read_char();
                Token::LBrace
            }
            Some('}') => {
                self.read_char();
                Token::RBrace
            }
            Some(',') => {
                self.read_char();
                Token::Comma
            }
            Some(':') => {
                self.read_char();
                Token::Colon
            }
            Some(';') => {
                self.read_char();
                Token::Semi
            }
            Some('.') => {
                self.read_char();
                Token::Dot
            }
            Some('"') => {
                let lit = self.read_string();
                Token::Str(lit)
            }
            Some(c) if c.is_ascii_digit() => {
                let n = self.read_number();
                Token::Int(n)
            }
            Some(c) if c.is_alphabetic() || c == '_' => {
                let ident = self.read_identifier();
                match ident.as_str() {
                    "if" => Token::If,
                    "else" => Token::Else,
                    "fn" => Token::Fn,
                    "let" => Token::Let,
                    "return" => Token::Return,
                    "print" => Token::Print,
                    "struct" => Token::Struct,
                    other => Token::Ident(other.to_string()),
                }
            }
            None => Token::Eof,
            Some(c) => panic!("Unexpected character: {:?}", c),
        };

        tok
    }

    fn read_string(&mut self) -> String {
        self.read_char();
        let mut s = String::new();
        while let Some(c) = self.ch {
            if c == '"' {
                break;
            }
            if c == '\\' {
                if let Some(n) = self.peek_char() {
                    self.read_char();
                    s.push(match n {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                    self.read_char();
                    continue;
                }
            }
            s.push(c);
            self.read_char();
        }
        self.read_char();
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_sample() {
        let src = r#"
            fn greet(name: str, age: int) -> str {
                let msg = "Hello, " + name;
                print(msg);
            }
            greet("world");
        "#;
        let mut lx = Lexer::new(src);
        let mut toks = Vec::new();
        loop {
            let t = lx.next_token();
            if t == Token::Eof {
                toks.push(t);
                break;
            }
            toks.push(t);
        }
        assert!(toks.contains(&Token::Fn));
        assert!(toks.contains(&Token::Ident("greet".into())));
        assert!(toks.contains(&Token::Str("Hello, ".into())));
        assert!(toks.contains(&Token::Plus));
        assert!(toks.contains(&Token::Print));
        assert!(toks.contains(&Token::LBrace));
        assert!(toks.contains(&Token::RBrace));
        assert!(toks.contains(&Token::Eof));
    }
}
