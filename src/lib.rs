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
    Lambda,
    Return,
    With,
    Yield,

    Ident(String),

    Float,
    Int,
    Dict,
    List,
    Min,
    Max,

    Eof,
}

pub struct Lexer {
    pub input: Vec<u8>,
    pub position: usize,
    // current read position, trailed by `position`
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
            if self.read_position + 1 < self.input.len() && self.input[self.read_position + 1] == b' ' {
                let _ = self.read_character();
            } else {
                break
            }
        }
    }

    pub fn read_character(&mut self) -> Result<(), ()> {
        if self.read_position + 1 == self.input.len() {
            return Err(());
        }
        self.read_position += 1;
        self.character = *self.input.get(self.read_position).unwrap();
        Ok(())
    }

    pub fn read_ident(&mut self) -> Vec<u8> {
        while let Ok(()) = self.read_character() {
            if self.character == b' ' {
                break;
            };
        }
        return self.input[self.position..=self.read_position].to_vec();
    }

    pub fn tokenize_next_character(&mut self) -> bool {
        self.handle_next_whitespace();

        if let Ok(()) = self.read_character() {
            let mut token: Option<Token> = None;
            match self.character {
                b'=' => token = Some(Token::Equals),
                b'0'..=b'9' => {
                    todo!("handle numbers")
                },
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    let ident = self.read_ident();
                    match std::str::from_utf8(&ident).unwrap() {
                        "def" => token = Some(Token::Def),
                        val => {
                            token = Some(Token::Ident(String::from(val)));
                            println!("FOUND: {:?}", token);
                        },
                    }
                }
                0 => token = Some(Token::Eof),
                char => unreachable!("shouldn't reach this, tried to match {:}", char as char),
            }
            self.tokens.push(token.unwrap());
            return true;
        }
        false
    }

    pub fn tokenize(&mut self) {
        loop {
            if !self.tokenize_next_character() {
                break;
            };
            self.position = self.read_position;
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
            ("x = 10", vec![Token::Ident("x".to_string()), Token::Equals, Token::Int])
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
