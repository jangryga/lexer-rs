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
    pub input: String,
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
            input: String::from(input),
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
        Some(self.input.chars().nth(self.read_position as usize).unwrap() as u8)
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
mod tests {}
