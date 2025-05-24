#[cfg(test)]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(Serialize, Deserialize))]
pub enum Token {
    If,
    Else,
    Fn,
    For,
    Let,
    Return,
    Print,
    Struct,
    Ident(String),
    Int(i64),
    Str(String),
    Plus,
    Minus,
    Star,
    Slash,
    Assign,
    Arrow,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
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

pub struct Lexer<'input> {
    input: &'input str,
    pos: usize,
    ch: Option<char>,
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
            let token = self.next_token();
            tokens.push(token.clone());
            if token == Token::Eof {
                break;
            }
        }
        tokens
    }

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
        while self.ch.is_some_and(|c| c.is_whitespace()) {
            self.read_char();
        }
    }

    fn read_while<F>(&mut self, mut pred: F) -> String
    where
        F: FnMut(char) -> bool,
    {
        let mut buf = String::new();
        while let Some(c) = self.ch {
            if pred(c) {
                buf.push(c);
                self.read_char();
            } else {
                break;
            }
        }
        buf
    }

    fn read_identifier(&mut self) -> Token {
        let ident = self.read_while(|c| c.is_alphanumeric() || c == '_');
        match ident.as_str() {
            "if" => Token::If,
            "else" => Token::Else,
            "fn" => Token::Fn,
            "for" => Token::For,
            "let" => Token::Let,
            "return" => Token::Return,
            "print" => Token::Print,
            "struct" => Token::Struct,
            _ => Token::Ident(ident),
        }
    }

    fn read_number(&mut self) -> Token {
        let num = self.read_while(|c| c.is_ascii_digit());
        Token::Int(num.parse().unwrap())
    }

    fn read_string(&mut self) -> Token {
        self.read_char(); // skip opening "
        let mut result = String::new();
        while let Some(c) = self.ch {
            if c == '"' {
                break;
            } else if c == '\\' {
                if let Some(esc) = self.peek_char() {
                    self.read_char();
                    result.push(match esc {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                    self.read_char();
                }
            } else {
                result.push(c);
                self.read_char();
            }
        }
        self.read_char(); // skip closing "
        Token::Str(result)
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        macro_rules! punct {
            ($single:expr => $tok:expr) => {{
                self.read_char();
                $tok
            }};
        }

        if let Some(tok) = match (self.ch, self.peek_char()) {
            (Some('-'), Some('>')) => Some(Token::Arrow),
            (Some('='), Some('=')) => Some(Token::Eq),
            (Some('!'), Some('=')) => Some(Token::Ne),
            (Some('<'), Some('=')) => Some(Token::Le),
            (Some('>'), Some('=')) => Some(Token::Ge),
            _ => None,
        } {
            self.read_char();
            self.read_char();
            return tok;
        }

        match self.ch {
            Some('=') => punct!('=' => Token::Assign),
            Some('+') => punct!('+' => Token::Plus),
            Some('-') => punct!('-' => Token::Minus),
            Some('*') => punct!('*' => Token::Star),
            Some('/') => punct!('/' => Token::Slash),
            Some('<') => punct!('<' => Token::Lt),
            Some('>') => punct!('>' => Token::Gt),
            Some('(') => punct!('(' => Token::LParen),
            Some(')') => punct!(')' => Token::RParen),
            Some('[') => punct!('[' => Token::LBracket),
            Some(']') => punct!(']' => Token::RBracket),
            Some('{') => punct!('{' => Token::LBrace),
            Some('}') => punct!('}' => Token::RBrace),
            Some(',') => punct!(',' => Token::Comma),
            Some(':') => punct!(':' => Token::Colon),
            Some(';') => punct!(';' => Token::Semi),
            Some('.') => punct!('.' => Token::Dot),
            Some('"') => self.read_string(),
            Some(c) if c.is_ascii_digit() => self.read_number(),
            Some(c) if c.is_alphabetic() || c == '_' => self.read_identifier(),
            Some(c) => panic!("Unexpected character: {:?}", c),
            None => Token::Eof,
        }
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_yaml_snapshot;

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
        let mut lexer = Lexer::new(src);
        let tokens = lexer.lex();

        assert_yaml_snapshot!(tokens);
    }
}
