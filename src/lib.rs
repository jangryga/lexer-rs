use wasm_bindgen::prelude::*;
use serde::{Serialize, Deserialize};

#[wasm_bindgen]
extern {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen(typescript_custom_section)]
const TS_TOKEN_INTERFACE: &'static str = r#"
export interface Token {
  kind: TokenType;
  value?: string; // `value` is optional because it's an Option<String> in Rust
  category: TokenCategory;
}
"#;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "Token[]")]
    pub type Tokens;

    // Use JsValue for representing an array in JS.
    #[wasm_bindgen(js_name = Array)]
    pub type JsArray;

    #[wasm_bindgen(constructor)]
    pub fn new() -> JsArray;

    // Method to push items into the JS array.
    #[wasm_bindgen(method, js_name = push)]
    pub fn push(this: &JsArray, item: JsValue);
}


#[wasm_bindgen]
pub fn parse(input: &str) -> Result<JsValue, JsValue> {
    let mut lexer = Lexer::new(input);
    lexer.tokenize_input();
    let result: Vec<Token> = lexer.tokens;

    // serde_wasm_bindgen::to_value(&result).map_err(|e| JsValue::from_str(&e.to_string()))
    // Create a new JS array
    let js_array: JsArray = JsArray::new();
    for token in result {
        // Convert each Token to a JsValue and push it to the JavaScript array
        let js_value = token.into_js_value();
        js_array.push(js_value);
    }
    Ok(JsValue::from(js_array))
}

#[wasm_bindgen]
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum TokenCategory {
    Keyword,
    Dunder,
    BuiltInType,
    BuiltInFn,
    PunctuationAndGroup,
    Operators,
    Comparison,
    Literal,
    Identifier,
    Whitespace,
}

#[wasm_bindgen]
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Token {
    kind: TokenType,
    value: Option<String>,
    category: TokenCategory,
}

#[wasm_bindgen]
impl Token {
    pub fn new(kind: TokenType, value: Option<String>, category: TokenCategory) -> Token {
        Token {
            kind,
            value,
            category,
        }
    }
    pub fn into_js_value(self) -> JsValue {
        // Convert the Token to a JsValue. This could be a JavaScript object.
        // Use serde_wasm_bindgen if you need to serialize complex structures.
        serde_wasm_bindgen::to_value(&self).unwrap()
    }
    // Getters can be added here for the fields if you want to provide access from JS.
}

#[wasm_bindgen]
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum TokenType {
    Indent,
    Dedent,

    // Standard keywords
    False,
    None,
    True,
    And,
    As,
    Assert,
    Async,
    Await,
    Break,
    Class,
    Continue,
    Def,
    Del,
    Elif,
    Else,
    Except,
    Finally,
    For,
    From,
    Global,
    If,
    Import,
    In,
    Is,
    Lambda,
    Local,
    Nonlocal,
    Not,
    Or,
    Pass,
    Raise,
    Return,
    Try,
    While,
    With,
    Yield,

    // Special methods (commonly used, but not reserved keywords)
    Init,    // __init__
    New,     // __new__
    Delitem, // __delitem__
    Getitem, // __getitem__
    Setitem, // __setitem__
    Str,     // __str__
    Repr,    // __repr__
    Len,     // __len__
    // Special cases
    Name,        // __name__
    Doc,         // __doc__
    Package,     // __package__
    Loader,      // __loader__
    Spec,        // __spec__
    Annotations, // __annotations__
    Builtins,    // __builtins__

    // Built-in types
    Int,
    Float,
    Complex,
    List,
    Tuple,
    RangeType,
    String,
    Set,
    Dict,
    FrozenSet,
    ByteArray,
    Bytes,
    MemoryView,
    Bool,

    // Built-in functions
    Abs,
    All,
    Any,
    Ascii,
    Bin,
    BoolFn,
    Breakpoint,
    Bytearray,
    BytesFn,
    Callable,
    Chr,
    Classmethod,
    Compile,
    ComplexFn,
    Delattr,
    DictFn,
    Dir,
    Divmod,
    Enumerate,
    Eval,
    Exec,
    Filter,
    FloatFn,
    Format,
    Frozenset,
    Getattr,
    Globals,
    Hasattr,
    Hash,
    Help,
    Hex,
    Id,
    Input,
    IntFn,
    Isinstance,
    Issubclass,
    Iter,
    LenFn,
    ListFn,
    Locals,
    Map,
    Max,
    Memoryview,
    Min,
    Next,
    Object,
    Oct,
    Open,
    Ord,
    Pow,
    Print,
    Property,
    Range,
    ReprFn,
    Reversed,
    Round,
    SetFn,
    Setattr,
    Slice,
    Sorted,
    Staticmethod,
    StrFn,
    Sum,
    Super,
    TupleFn,
    Type,
    Vars,
    Zip,
    ImportFn, // __import__

    // Punctuation and grouping
    LeftParenthesis,  // (
    RightParenthesis, // )
    LeftBracket,      // [
    RightBracket,     // ]
    LeftBrace,        // {
    RightBrace,       // }
    Comma,            // ,
    Dot,              // .
    Semicolon,        // ;
    Colon,            // :
    Arrow,            // ->
    Ellipsis,         // ...

    // Operators
    Plus,             // +
    Minus,            // -
    Multiply,         // *
    Divide,           // /
    Modulo,           // %
    Power,            // **
    FloorDivide,      // //
    Increment,        // ++
    Decrement,        // --
    PlusEqual,        // +=
    MinusEqual,       // -=
    MultiplyEqual,    // *=
    DivideEqual,      // /=
    ModuloEqual,      // %=
    PowerEqual,       // **=
    FloorDivideEqual, // //=
    AndEqual,         // &=
    OrEqual,          // |=
    XorEqual,         // ^=
    ShiftLeftEqual,   // <<=
    ShiftRightEqual,  // >>=
    Assign,           // =

    // Comparison and logical operators
    Equal,        // ==
    NotEqual,     // !=
    Greater,      // >
    Less,         // <
    GreaterEqual, // >=
    LessEqual,    // <=
    AndCmp,       // &&
    OrCmp,        // ||
    NotCmp,       // !
    BitwiseAnd,   // &
    BitwiseOr,    // |
    BitwiseXor,   // ^
    BitwiseNot,   // ~
    ShiftLeft,    // <<
    ShiftRight,   // >>

    Eof,
    Ident,
    Newline,
}

#[derive(Serialize, Deserialize)]
pub struct Lexer {
    pub input: Vec<u8>,
    pub position: usize,
    pub read_position: usize,
    pub current_indent: i32,
    pub tokens: Vec<Token>,
    pub character: u8,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lex = Lexer {
            character: 0,
            position: 0,
            read_position: 0,
            current_indent: 0,
            input: input.into(),
            tokens: Vec::<Token>::new(),
        };
        lex.read_character();
        lex
    }

    pub fn tokenize_next_character(&mut self) -> Result<Token, ()> {
        self.handle_next_whitespace();

        let token = match self.character {
            b'(' => Token::new(
                TokenType::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            b')' => Token::new(
                TokenType::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            b'[' => Token::new(
                TokenType::LeftBracket,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            b']' => Token::new(
                TokenType::RightBracket,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            b'{' => Token::new(
                TokenType::LeftBrace,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            b'}' => Token::new(
                TokenType::RightBrace,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            b',' => Token::new(TokenType::Comma, None, TokenCategory::PunctuationAndGroup),
            b'.' => {
                if self.double_peek() == Some(String::from("..")) {
                    self.read_character();
                    self.read_character();
                    Token::new(
                        TokenType::Ellipsis,
                        None,
                        TokenCategory::PunctuationAndGroup,
                    )
                } else {
                    Token::new(TokenType::Dot, None, TokenCategory::PunctuationAndGroup)
                }
            }
            b';' => Token::new(
                TokenType::Semicolon,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            b':' => Token::new(TokenType::Colon, None, TokenCategory::PunctuationAndGroup),
            b'-' => {
                if let Some(val) = self.peek() {
                    self.read_character();
                    match val {
                        b'>' => Token::new(TokenType::Arrow, None, TokenCategory::Operators),
                        b'-' => Token::new(TokenType::Decrement, None, TokenCategory::Operators),
                        b'=' => Token::new(TokenType::MinusEqual, None, TokenCategory::Operators),
                        _ => unreachable!("Lexer error on '-'"),
                    }
                } else {
                    Token::new(TokenType::Minus, None, TokenCategory::Operators)
                }
            }
            b'+' => {
                if let Some(val) = self.peek() {
                    self.read_character();
                    match val {
                        b'+' => Token::new(TokenType::Increment, None, TokenCategory::Operators),
                        b'=' => Token::new(TokenType::PlusEqual, None, TokenCategory::Operators),
                        _ => unreachable!("Lexer error on '+'"),
                    }
                } else {
                    Token::new(TokenType::Plus, None, TokenCategory::Operators)
                }
            }
            b'*' => {
                if self.peek() == Some(b'*') {
                    self.read_character();
                    Token::new(TokenType::Power, None, TokenCategory::Operators)
                } else {
                    Token::new(TokenType::Multiply, None, TokenCategory::Operators)
                }
            }
            b'/' => {
                if self.peek() == Some(b'/') {
                    self.read_character();
                    Token::new(TokenType::FloorDivide, None, TokenCategory::Operators)
                } else {
                    Token::new(TokenType::Divide, None, TokenCategory::Operators)
                }
            }
            b'%' => Token::new(TokenType::Modulo, None, TokenCategory::Operators),
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.read_character();
                    Token::new(TokenType::Equal, None, TokenCategory::Comparison)
                } else {
                    Token::new(TokenType::Assign, None, TokenCategory::Operators)
                }
            }
            b'!' => {
                if self.peek() == Some(b'=') {
                    self.read_character();
                    Token::new(TokenType::NotEqual, None, TokenCategory::Comparison)
                } else {
                    Token::new(TokenType::NotCmp, None, TokenCategory::Comparison)
                }
            }
            b'>' => {
                if let Some(val) = self.peek() {
                    self.read_character();
                    match val {
                        b'=' => {
                            Token::new(TokenType::GreaterEqual, None, TokenCategory::Comparison)
                        }
                        b'>' => Token::new(TokenType::ShiftRight, None, TokenCategory::Operators),
                        _ => unreachable!("Lexer error on '>'"),
                    }
                } else {
                    Token::new(TokenType::Greater, None, TokenCategory::Comparison)
                }
            }
            b'<' => {
                if let Some(val) = self.peek() {
                    self.read_character();
                    match val {
                        b'=' => Token::new(TokenType::LessEqual, None, TokenCategory::Comparison),
                        b'<' => Token::new(TokenType::ShiftLeft, None, TokenCategory::Operators),
                        _ => unreachable!("Lexer error on '<'"),
                    }
                } else {
                    Token::new(TokenType::Less, None, TokenCategory::Comparison)
                }
            }
            // b'&' => {
            //     if self.peek() == Some(b'&') {
            //         self.read_character();
            //         Token::new(TokenType::LogicalAnd, None, TokenCategory::Operators)
            //     } else {
            //         Token::new(TokenType::BitwiseAnd, None, TokenCategory::Operators)
            //     }
            // },
            // b'|' => {
            //     if self.peek() == Some(b'|') {
            //         self.read_character();
            //         Token::new(TokenType::LogicalOr, None, TokenCategory::Operators)
            //     } else {
            //         Token::new(TokenType::BitwiseOr, None, TokenCategory::Operators)
            //     }
            // },
            b'^' => Token::new(TokenType::BitwiseXor, None, TokenCategory::Operators),
            b'~' => Token::new(TokenType::BitwiseNot, None, TokenCategory::Operators),
            b'\n' => match self.indent_diff() {
                val if val > 0 => Token::new(
                    TokenType::Indent,
                    Some(val.to_string()),
                    TokenCategory::Whitespace,
                ),
                val if val < 0 => Token::new(
                    TokenType::Dedent,
                    Some(val.abs().to_string()),
                    TokenCategory::Whitespace,
                ),
                0 => Token::new(TokenType::Newline, None, TokenCategory::Whitespace),
                _ => unreachable!("Indentation error"),
            },
            // b'\n' => {
            //     match self.indent_diff() {
            //         val if val > 0 => Token::new(TokenType::Indent, None, TokenCategory::Literal),
            //         val if val < 0 => Token::new(TokenType::Dedent, None, TokenCategory::Literal),
            //         0 => Token::new(TokenType::Newline, None, TokenCategory::Literal),
            //         _ => unreachable!("Broke numbers")
            //     }
            // },
            b'0'..=b'9' => {
                // Start of a number literal
                let mut number_str = self.read_num(); // You need to define read_number method
                if let Some(next_char) = self.peek() {
                    // Check for a floating-point literal
                    if next_char == b'.' {
                        self.read_character(); // Consume the '.'
                        number_str.push('.'); // Add '.' to the number string
                        number_str.push_str(&self.read_num()); // Append the rest of the floating-point number
                    }
                }
                return Ok(Token::new(
                    TokenType::Ident,
                    Some(number_str),
                    TokenCategory::Literal,
                ));
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();

                let (token_type, token_category) = match ident.as_str() {
                    "def" => (TokenType::Def, TokenCategory::Keyword),
                    "from" => (TokenType::From, TokenCategory::Keyword),
                    "import" => (TokenType::Import, TokenCategory::Keyword),
                    "if" => (TokenType::If, TokenCategory::Keyword),
                    "else" => (TokenType::Else, TokenCategory::Keyword),
                    "elif" => (TokenType::Elif, TokenCategory::Keyword),
                    "for" => (TokenType::For, TokenCategory::Keyword),
                    "while" => (TokenType::While, TokenCategory::Keyword),
                    "False" => (TokenType::False, TokenCategory::Keyword),
                    "True" => (TokenType::True, TokenCategory::Keyword),
                    "continue" => (TokenType::Continue, TokenCategory::Keyword),
                    "break" => (TokenType::Break, TokenCategory::Keyword),
                    "del" => (TokenType::Del, TokenCategory::Keyword),
                    "global" => (TokenType::Global, TokenCategory::Keyword),
                    "local" => (TokenType::Local, TokenCategory::Keyword),
                    "nonlocal" => (TokenType::Nonlocal, TokenCategory::Keyword),
                    "try" => (TokenType::Try, TokenCategory::Keyword),
                    "except" => (TokenType::Except, TokenCategory::Keyword),
                    "as" => (TokenType::As, TokenCategory::Keyword),
                    "finally" => (TokenType::Finally, TokenCategory::Keyword),
                    "is" => (TokenType::Is, TokenCategory::Keyword),
                    "in" => (TokenType::In, TokenCategory::Keyword),
                    "not" => (TokenType::Not, TokenCategory::Keyword),
                    "lambda" => (TokenType::Lambda, TokenCategory::Keyword),
                    "return" => (TokenType::Return, TokenCategory::Keyword),
                    "with" => (TokenType::With, TokenCategory::Keyword),
                    "yield" => (TokenType::Yield, TokenCategory::Keyword),
                    "pass" => (TokenType::Pass, TokenCategory::Keyword),
                    // Add cases for built-in functions, types, etc.
                    "print" => (TokenType::Print, TokenCategory::BuiltInFn),
                    // ...
                    // Default case for identifiers
                    _ => (TokenType::Ident, TokenCategory::Identifier),
                };
                if token_type == TokenType::Ident {
                    return Ok(Token::new(
                        token_type,
                        Some(String::from(ident)),
                        token_category,
                    ));
                }
                return Ok(Token::new(token_type, None, token_category));
            }
            0 => Token::new(TokenType::Eof, None, TokenCategory::Literal),
            char => unreachable!("shouldn't reach this, tried to match {}", char as char),
        };

        self.read_character();
        Ok(token)
    }

    pub fn handle_next_whitespace(&mut self) {
        while self.read_position < self.input.len() && self.character == b' ' {
            self.read_character();
        }
    }

    pub fn indent_diff(&mut self) -> i32 {
        let mut indent_length = 0;
        let initial = self.current_indent;

        while self.read_position < self.input.len() && self.input[self.read_position] == b' ' {
            self.read_character();
            indent_length += 1;
        }

        self.current_indent = indent_length;
        indent_length - initial
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

    pub fn peek(&mut self) -> Option<u8> {
        if self.read_position >= self.input.len() || self.input[self.read_position] == b' ' {
            return None;
        }
        Some(self.input[self.read_position])
    }

    pub fn double_peek(&mut self) -> Option<String> {
        if self.read_position + 1 >= self.input.len() {
            return None;
        }

        Some(
            String::from_utf8_lossy(&self.input[self.read_position..=self.position + 1])
                .to_string(),
        )
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
    use crate::{Lexer, Token, TokenCategory, TokenType};

    #[test]
    fn single_tokens() {
        let exps = vec![
            (
                "variable_name",
                vec![Token::new(
                    TokenType::Ident,
                    Some(String::from("variable_name")),
                    TokenCategory::Identifier,
                )],
            ),
            (
                "def",
                vec![Token::new(TokenType::Def, None, TokenCategory::Keyword)],
            ),
            (
                "**",
                vec![Token::new(TokenType::Power, None, TokenCategory::Operators)],
            ),
            (
                "my_func",
                vec![Token::new(
                    TokenType::Ident,
                    Some(String::from("my_func")),
                    TokenCategory::Identifier,
                )],
            ),
        ];
        run_tests(exps)
    }

    #[test]
    fn one_line_expressions() {
        let tokens = vec![
            Token::new(TokenType::Def, None, TokenCategory::Keyword),
            Token::new(
                TokenType::Ident,
                Some(String::from("my_func")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenType::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenType::Ident,
                Some(String::from("a")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenType::Comma, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenType::Ident,
                Some(String::from("b")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenType::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenType::Colon, None, TokenCategory::PunctuationAndGroup),
        ];
        run_tests_explicit("def my_func(a, b):", tokens);
    }

    #[test]
    fn multiline() {
        let input = r#"def my_func(a, b):
        return a + b
    
    res = my_func(1, 32)
    print(res)
    "#;

        let tokens = vec![
            Token::new(TokenType::Def, None, TokenCategory::Keyword),
            Token::new(
                TokenType::Ident,
                Some(String::from("my_func")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenType::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenType::Ident,
                Some(String::from("a")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenType::Comma, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenType::Ident,
                Some(String::from("b")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenType::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenType::Colon, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenType::Indent,
                Some(String::from("4")),
                TokenCategory::Whitespace,
            ), // Assuming you have a Whitespace category
            Token::new(TokenType::Return, None, TokenCategory::Keyword),
            Token::new(
                TokenType::Ident,
                Some(String::from("a")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenType::Plus, None, TokenCategory::Operators),
            Token::new(
                TokenType::Ident,
                Some(String::from("b")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenType::Dedent,
                Some(String::from("4")),
                TokenCategory::Whitespace,
            ), // Assuming you have a Whitespace category
            Token::new(TokenType::Newline, None, TokenCategory::Whitespace),
            Token::new(
                TokenType::Ident,
                Some(String::from("res")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenType::Assign, None, TokenCategory::Operators),
            Token::new(
                TokenType::Ident,
                Some(String::from("my_func")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenType::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenType::Ident,
                Some(String::from("1")),
                TokenCategory::Literal,
            ),
            Token::new(TokenType::Comma, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenType::Ident,
                Some(String::from("32")),
                TokenCategory::Literal,
            ),
            Token::new(
                TokenType::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenType::Newline, None, TokenCategory::Whitespace),
            Token::new(TokenType::Print, None, TokenCategory::BuiltInFn),
            Token::new(
                TokenType::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenType::Ident,
                Some(String::from("res")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenType::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenType::Newline, None, TokenCategory::Whitespace),
        ];

        run_tests_explicit(input, tokens);
    }

    fn run_tests_explicit(input: &str, tokens: Vec<Token>) {
        let mut lexer = Lexer::new(input);
        lexer.current_indent = 4;

        for token in tokens {
            let current = lexer.tokenize_next_character();
            println!("{:?} {:?}", token, current);
            assert!(token == current.unwrap());
        }
    }

    fn run_tests(exps: Vec<(&str, Vec<Token>)>) {
        for (input, expected) in exps {
            let mut lexer = Lexer::new(input);
            for (i, exp_token) in expected.iter().enumerate() {
                match lexer.tokenize_next_character() {
                    Ok(token) => assert_eq!(&token, exp_token, "Test failed at token index: {}", i),
                    Err(_) => panic!("Tokenization failed at index: {}", i),
                }
            }
        }
    }
}
