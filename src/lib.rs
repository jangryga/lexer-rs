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

    EOF,
}

pub struct Lexer {
    pub input: String,
    pub cursor: u32,
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            cursor: 0,
            input: String::from(input),
            tokens: Vec::<Token>::new(),
        }
    }

    pub fn handle_next_whitespace(&mut self) {}

    pub fn tokenize_character(&mut self) -> bool {
        self.handle_next_whitespace();

        true
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
