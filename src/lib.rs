#[derive(PartialEq, Debug)]
pub enum Token {
    Indent,
    Dedent,
    Def,
    From,
    Import,

    If,
    Else,
    Elif,
    For,
    While,
    Colon,

    Plus,
    Minus,
    Div,
    Mul,
    Equals,
    DoubleEquals,
    NotDoubleEquals,

    False,
    True,

    Continue,
    Break,
    Del,
    
    Global,
    Local,
    Nonlocal,

    Try,
    Except,
    As,
    Finally,
    Raise,

    In,
    Is,
    Not,
    Lambda,
    Return,
    With,
    Yield,

    Ident(String),

    Float(String),
    Int(String),
    Dict,
    List,
    Min,
    Max,

    Eof,
    Newline
}

pub struct Lexer {
    pub input: Vec<u8>,
    pub position: usize,
    pub read_position: usize,
    pub tokens: Vec<Token>,
    pub character: u8,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lex = Lexer {
            character: 0,
            position: 0,
            read_position: 0,
            input: input.into(),
            tokens: Vec::<Token>::new(),
        };
        lex.read_character();
        lex
    }

    pub fn handle_next_whitespace(&mut self) {
        while self.read_position < self.input.len() && self.input[self.read_position].is_ascii_whitespace() {
            self.read_character();
        }
    }

    pub fn read_character(&mut self) {
        if self.read_position >= self.input.len() {
            self.character = 0;
        } else {
            self.character = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn read_ident(&mut self) -> String {
        let pos = self.position;
        while self.character.is_ascii_alphanumeric() || self.character == b'_' {
            self.read_character();
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    pub fn read_num(&mut self) -> String {
        let pos = self.position;
        while self.character.is_ascii_digit() {
            self.read_character();
        }
        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    pub fn tokenize_next_character(&mut self) -> Result<Token, ()> {
        self.handle_next_whitespace();

        let token = match self.character {
            b'=' => Token::Equals,
            b'0'..=b'9' => {
                let num = self.read_num();
                Token::Int(num)
            },
            b'\n' => {
                Token::Newline
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return Ok(match ident.as_str() {
                    "def" => Token::Def,
                    _ => {
                        Token::Ident(ident)
                    },
                })
            }
            0 => Token::Eof,
            char => unreachable!("shouldn't reach this, tried to match {} \n tokens: {:?}", char as char, self.tokens),
        };
        
        self.read_character();
        Ok(token)
    }

    pub fn tokenize_input(&mut self) {
        while self.read_position <= self.input.len() {
            if let Ok(token) = self.tokenize_next_character() {
                self.tokens.push(token);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, Token};

    #[test]
    fn single_tokens() {
        let exps = vec![
            ("variable_name", vec![Token::Ident(String::from("variable_name"))]),
            ("def", vec![Token::Def])
        ];
        run_tests(exps)
    }

    #[test]
    fn one_line_expressions() {
        let exps = vec![
            (
                "x=10", 
                vec![
                    Token::Ident(String::from("x")),
                    Token::Equals, 
                    Token::Int(String::from("10"))
                ]
            )
        ];
        run_tests(exps)
    }

    fn run_tests(exps: Vec<(&str, Vec<Token>)>) {
        for e in exps {
            let mut lex = Lexer::new(e.0.into());
            lex.tokenize_input();
            println!("Need: {:?}, found {:?}", e.1, lex.tokens);
            assert!(lex.tokens == e.1)
        }
    }
}
