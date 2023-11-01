#[derive(PartialEq)]
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

    IDENT,

    EOF,
}

pub struct Lexer {
    pub input: Vec<u8>,
    pub position: u32,
    pub read_position: u32,
    pub tokens: Vec<Token>,
    pub character: u8
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
    }

    pub fn read_character(&mut self) -> Option<u8> {
        if self.read_position as usize >= self.input.len() {
            return None
        }
        self.position = self.read_position;
        self.read_position += 1;
        Some(self.input[self.read_position as usize])
    }

    pub fn read_ident(&self) {
        !unimplemented!()
    }

    pub fn tokenize_character(&mut self) -> bool {
        self.handle_next_whitespace();

        match self.read_character() {
            Some(ch) => {
                let mut token: Option<Token> = None;
                match ch {
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                        let indent = self.read_ident();
                    }
                    0 => token = Some(Token::EOF),
                    _ => unreachable!("shouldn't reach this")
                }
                
                self.tokens.push(token.unwrap());
                true
            }
            None => false
        }
    }

    pub fn tokenize(&mut self) {
        loop {
            if !self.tokenize_character() {
                break;
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Token, Lexer};

    #[test]
    fn if_sees_tokens() {
        let mut l = Lexer::new("");
        let data: Vec<(&str, Vec<Token>)> = vec![("variable_name", vec!(Token::IDENT))];

        for entry in data {
            l.input = entry.0.into();
            l.tokenize();
            assert!(l.tokens == entry.1)
        }
        
    }

}
