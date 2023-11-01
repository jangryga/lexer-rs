#[derive(PartialEq, Debug)]
pub enum Token {
    INDENT,
    DEDENT,
    DEF,
    FROM,
    IMPORT,

    IF,
    ELSE,
    ELIF,
    FOR,
    WHILE,
    COLON,

    PLUS,
    MINUS,
    DIV,
    MUL,

    FALSE,
    TRUE,

    CONTINUE,
    BREAK,
    DEL,
    GLOBAL,

    TRY,
    RAISE,
    EXCEPT,
    FINALLY,
    AS,

    IN,
    IS,
    LAMBDA,
    RETURN,
    WITH,
    YIELD,

    IDENT(String),

    EOF,
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
        // advance
        self.position = self.read_position;
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
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    let ident = self.read_ident();
                    match std::str::from_utf8(&ident).unwrap() {
                        "def" => token = Some(Token::DEF),
                        val => {
                            token = Some(Token::IDENT(String::from(val)));
                            println!("FOUND: {:?}", token);
                        },
                    }
                }
                0 => token = Some(Token::EOF),
                _ => unreachable!("shouldn't reach this"),
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
    fn if_sees_tokens() {
        let data: Vec<(&str, Vec<Token>)> = vec![
            ("variable_name", vec![Token::IDENT("variable_name".to_string())]),
            ("def", vec![Token::DEF])
        ];

        for entry in data {
            let mut l = Lexer::new(entry.0.into());
            l.tokenize();
            assert!(l.tokens == entry.1)
        }
    }
}
