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
        Lexer {
            character: 0,
            position: 0,
            read_position: 0,
            input: input.into(),
            tokens: Vec::<Token>::new(),
        }
    }

    pub fn handle_next_whitespace(&mut self) {
        loop {
            if self.read_position < self.input.len() && self.input[self.read_position] == b' ' {
                let _ = self.read_character();
                self.position += 1;
            } else {
                break
            }
        }
    }

    pub fn read_character(&mut self) -> Result<(), ()> {
        if self.read_position == self.input.len() {
            return Err(());
        }
        self.character = self.input[self.read_position];
        self.read_position += 1;
        Ok(())
    }

    pub fn read_ident(&mut self) -> Vec<u8> {
        let mut reached_end = false;
        while self.character != b' ' {
            match self.read_character() {
                Err(_) => {
                    reached_end = true;
                    break
                },
                Ok(_) => continue
            }
        }
        if reached_end {
            return self.input[self.position..self.read_position].to_vec();
        }
        return self.input[self.position..self.read_position-1].to_vec()
    }

    pub fn tokenize_next_character(&mut self) {
        self.handle_next_whitespace();
        let mut token: Option<Token> = None;
        if let Err(_) = self.read_character() {
            return
        }
        match self.character {
            b'=' => token = Some(Token::Equals),
            b'0'..=b'9' => {
                let number = self.read_ident();
                token = Some(Token::Int(String::from(std::str::from_utf8(&number).unwrap())))
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                match std::str::from_utf8(&ident).unwrap() {
                    "def" => token = Some(Token::Def),
                    val => {
                        token = Some(Token::Ident(String::from(val)));
                    },
                }
            }
            0 => token = Some(Token::Eof),
            char => unreachable!("shouldn't reach this, tried to match {} \n tokens: {:?}", char as char, self.tokens),
        }
        self.tokens.push(token.unwrap());
    }

    pub fn tokenize(&mut self) {
        loop {
            if self.read_position == self.input.len() {
                break
            }
            self.tokenize_next_character();
            self.position = self.read_position
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, Token};

    #[test]
    fn individual_tokens() {
        let exps = vec![
            ("variable_name", vec![Token::Ident("variable_name".to_string())]),
            ("def", vec![Token::Def])
        ];

        run_tests(exps)
    }

    #[test]
    fn one_line_expressions() {
        let exps = vec![
            ("x =   10", vec![Token::Ident("x".to_string()), Token::Equals, Token::Int("10".to_string())])
        ];

        run_tests(exps)
    }

    fn run_tests(exps: Vec<(&str, Vec<Token>)>) {
        for e in exps {
            let mut l = Lexer::new(e.0.into());
            l.tokenize();
            println!("Need: {:?}, found {:?}", e.1, l.tokens);
            assert!(l.tokens == e.1)
        }
    }
}
