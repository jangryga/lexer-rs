use console_error_panic_hook::set_once;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

static WHITESPACE: [u32; 2] = [32, 160];

macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[cfg(target_arch = "wasm32")]
pub fn log(s: &str) {
    web_sys::console::log_1(&s.into());
}

#[cfg(not(target_arch = "wasm32"))]
fn log(message: &str) {
    println!("{}", message);
}

#[wasm_bindgen(typescript_custom_section)]
const TS_TOKEN_INTERFACE: &'static str = r#"
export interface TokenType {
  kind: string; // TokenKind
  value: string | undefined; // `value` is optional because it's an Option<String> in Rust
  category: string // TokenCategory; 
}
"#;

// TODO: move to utils or init code
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    set_once();
    Ok(())
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
    Eof,
    Comment,
}

#[wasm_bindgen]
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Token {
    kind: TokenKind,
    value: Option<String>,
    category: TokenCategory,
}

#[wasm_bindgen]
impl Token {
    pub fn new(kind: TokenKind, value: Option<String>, category: TokenCategory) -> Token {
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
}

#[wasm_bindgen]
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum TokenKind {
    Indent,
    Dedent,

    // Comments
    StringMultiline, // it can be both comment and string so it cannot be ignored
    CommentSingleline,

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
    Whitespace,
}

#[wasm_bindgen]
pub struct LexerWrapper {
    lexer: Lexer,
}

#[wasm_bindgen]
impl LexerWrapper {
    #[wasm_bindgen(constructor)]
    pub fn new() -> LexerWrapper {
        LexerWrapper {
            lexer: Lexer::new(None, LexerMode::Editor),
        }
    }

    pub fn tokenize(&mut self, input: &str) -> Result<JsValue, JsValue> {
        self.lexer.input = input.chars().map(|ch| ch as u32).collect();
        self.lexer.position = 0;
        self.lexer.read_position = 0;
        self.lexer.current_indent = 0;
        self.lexer.read_character();
        let mut tokens: Vec<Token> = Vec::new();

        while self.lexer.read_position <= self.lexer.input.len() + 1 {
            // this is a hack because tokenize_next_character doesn't return more than one token, hence why newline gets skipped
            if let Ok(token) = self.lexer.tokenize_next_character() {
                if token.kind == TokenKind::Indent || token.kind == TokenKind::Dedent {
                    tokens.push(Token::new(
                        TokenKind::Newline,
                        None,
                        TokenCategory::Whitespace,
                    ))
                }
                tokens.push(token);
            }
        }

        serde_wasm_bindgen::to_value(&tokens).map_err(|e| JsValue::from_str(&e.to_string()))
    }
}

#[derive(Serialize, Deserialize, PartialEq)]
pub enum LexerMode {
    Ast,
    Editor,
}

#[derive(Serialize, Deserialize, PartialEq)]
pub enum CommentMode {
    Active(CommentType),
    Inactive,
}

#[derive(Serialize, Deserialize, PartialEq)]
pub enum CommentType {
    SingleLine,
    MultiLine,
}

#[derive(Serialize, Deserialize)]
pub struct Lexer {
    pub input: Vec<u32>,
    pub position: usize,
    pub read_position: usize,
    pub current_indent: i32,
    pub character: u32,
    pub mode: LexerMode,
    pub comment_mode: CommentMode,
}

impl Lexer {
    pub fn new(input: Option<&str>, mode: LexerMode) -> Lexer {
        match input {
            Some(text) => {
                let mut lex = Lexer {
                    character: 0,
                    position: 0,
                    read_position: 0,
                    current_indent: 0,
                    input: text.chars().map(|ch| ch as u32).collect(),
                    mode,
                    comment_mode: CommentMode::Inactive,
                };
                // I don't like this - initializing with input exhibits different behavior
                lex.read_character();
                lex
            }
            None => Lexer {
                character: 0,
                position: 0,
                read_position: 0,
                current_indent: 0,
                input: Vec::new(),
                mode,
                comment_mode: CommentMode::Inactive,
            },
        }
    }

    pub fn skip_comment(&mut self) {
        todo!()
    }

    pub fn read_multiline_string(&mut self) -> String {
        todo!()
    }

    pub fn read_string_literal(
        &mut self,
        end_character: u32, /* ' or "" => 34 or 39 */
    ) -> String {
        let pos: usize = self.position;

        loop {
            self.read_character();
            if self.character == end_character && self.input[self.position - 1] != 92 {
                break;
            }
        }

        let sequence = &self.input[pos..self.position];
        sequence
            .iter()
            .filter_map(|&code_point| std::char::from_u32(code_point))
            .collect()
    }

    pub fn read_singleline_comment(&mut self) -> String {
        let pos: usize = self.position;

        while self.character != 10
        /* '\n' */
        {
            self.read_character();
        }

        let sequence = &self.input[pos..self.position];
        sequence
            .iter()
            .filter_map(|&code_point| std::char::from_u32(code_point))
            .collect()
    }

    pub fn tokenize_next_character(&mut self) -> Result<Token, ()> {
        if self.mode == LexerMode::Ast {
            self.handle_next_whitespace();
        }

        let token = match self.character {
            32 | 160 => {
                let mut length = 0;
                while self.read_position <= self.input.len() && WHITESPACE.contains(&self.character) {
                    length += 1;
                    self.read_character();
                }
                return Ok(Token::new(TokenKind::Whitespace, Some(length.to_string()), TokenCategory::Whitespace))
            },
            34 /* '"' */ => {
                if self.double_peek() == Some(String::from("\"\"")) {
                    self.read_character();
                    self.read_character();
                    let value = self.read_multiline_string();
                    return Ok(Token::new(
                        TokenKind::StringMultiline,
                        Some(value),
                        TokenCategory::Literal
                    ))
                }
                let value: String = self.read_string_literal(34);
                Token::new(TokenKind::String, Some(value), TokenCategory::Literal)
            },
            35 /* '#' */ => {
                if self.mode == LexerMode::Ast {
                    self.skip_comment();
                    return self.tokenize_next_character();
                }
                let value = self.read_singleline_comment();
                Token::new(TokenKind::CommentSingleline, Some(value), TokenCategory::Comment)
            },
            39 /* ''' */ => {
                // it could be start of a string
                if self.double_peek() == Some(String::from("''")) {
                    self.read_character();
                    self.read_character();
                    let value = self.read_multiline_string();
                    return Ok(Token::new(
                        TokenKind::StringMultiline,
                        Some(value),
                        TokenCategory::Literal,
                    ))
                }
                let value = self.read_string_literal(39);

                Token::new(
                    TokenKind::String,
                    Some(value),
                    TokenCategory::Literal,
                )
            },
            40 /* '(' */ => Token::new(
                TokenKind::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            41 /* ')' */ => Token::new(
                TokenKind::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            91 /* '[' */ => Token::new(
                TokenKind::LeftBracket,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            92 /* \ */ => {
                // start of a string <- end will be auto-detected
                if self.peek() == Some(34) || self.peek() == Some(39) {
                    return self.tokenize_next_character();
                }
                Token::new(TokenKind::Ident, Some(String::from("\\")), TokenCategory::Identifier)
            },
            93 /* ']' */ => Token::new(
                TokenKind::RightBracket,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            123 /* '{' */ => Token::new(
                TokenKind::LeftBrace,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            125 /* '}' */ => Token::new(
                TokenKind::RightBrace,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            44 /* ',' */ => Token::new(TokenKind::Comma, None, TokenCategory::PunctuationAndGroup),
            46 /* '.' */ => {
                if self.double_peek() == Some(String::from("..")) {
                    self.read_character();
                    self.read_character();
                    Token::new(
                        TokenKind::Ellipsis,
                        None,
                        TokenCategory::PunctuationAndGroup,
                    )
                } else {
                    Token::new(TokenKind::Dot, None, TokenCategory::PunctuationAndGroup)
                }
            },
            59 /* ';' */ => Token::new(
                TokenKind::Semicolon,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            58 /* ':' */ => Token::new(TokenKind::Colon, None, TokenCategory::PunctuationAndGroup),
            45 /* '-' */ => {
                if [Some(62), Some(45), Some(61)].contains(&self.peek()) {
                    self.read_character();
                    match self.character {
                        62 /* '>' */ => Token::new(TokenKind::Arrow, None, TokenCategory::Operators),
                        45 /* '-' */ => Token::new(TokenKind::Decrement, None, TokenCategory::Operators),
                        61 /* '=' */ => Token::new(TokenKind::MinusEqual, None, TokenCategory::Operators),
                        _ => unreachable!("Lexer error on '-'"),
                    }
                } else {
                    Token::new(TokenKind::Minus, None, TokenCategory::Operators)
                }
            },
            43 /* '+' */ => {
                if [Some(43), Some(61)].contains(&self.peek())  {
                    self.read_character();
                    match self.character {
                        43 /* '+' */ => Token::new(TokenKind::Increment, None, TokenCategory::Operators),
                        61 /* '=' */ => Token::new(TokenKind::PlusEqual, None, TokenCategory::Operators),
                        _ => unreachable!("Lexer error on '+'"),
                    }
                } else {
                    Token::new(TokenKind::Plus, None, TokenCategory::Operators)
                }
            },
            42 /* '*' */ => {
                if self.peek() == Some(42 /* '*' */) {
                    self.read_character();
                    Token::new(TokenKind::Power, None, TokenCategory::Operators)
                } else {
                    Token::new(TokenKind::Multiply, None, TokenCategory::Operators)
                }
            },
            47 /* '/' */ => {
                if self.peek() == Some(47 /* '/' */) {
                    self.read_character();
                    Token::new(TokenKind::FloorDivide, None, TokenCategory::Operators)
                } else {
                    Token::new(TokenKind::Divide, None, TokenCategory::Operators)
                }
            },
            37 /* '%' */ => Token::new(TokenKind::Modulo, None, TokenCategory::Operators),
            61 /* '=' */ => {
                if self.peek() == Some(61 /* '=' */) {
                    self.read_character();
                    Token::new(TokenKind::Equal, None, TokenCategory::Comparison)
                } else {
                    Token::new(TokenKind::Assign, None, TokenCategory::Operators)
                }
            },
            33 /* '!' */ => {
                if self.peek() == Some(61 /* '=' */) {
                    self.read_character();
                    Token::new(TokenKind::NotEqual, None, TokenCategory::Comparison)
                } else {
                    Token::new(TokenKind::NotCmp, None, TokenCategory::Comparison)
                }
            },
            62 /* '>' */ => {
                if [Some(61), Some(62)].contains(&self.peek()) {
                    self.read_character();
                    match self.character {
                        61 /* '=' */ => Token::new(TokenKind::GreaterEqual, None, TokenCategory::Comparison),
                        62 /* '>' */ => Token::new(TokenKind::ShiftRight, None, TokenCategory::Operators),
                        _ => unreachable!("Lexer error on '>'"),
                    }
                } else {
                    Token::new(TokenKind::Greater, None, TokenCategory::Comparison)
                }
            },
            60 /* '<' */ => {
                if [Some(61), Some(60)].contains(&self.peek()) {
                    self.read_character();
                    match self.character {
                        61 /* '=' */ => Token::new(TokenKind::LessEqual, None, TokenCategory::Comparison),
                        60 /* '<' */ => Token::new(TokenKind::ShiftLeft, None, TokenCategory::Operators),
                        _ => unreachable!("Lexer error on '<'"),
                    }
                } else {
                    Token::new(TokenKind::Less, None, TokenCategory::Comparison)
                }
            },
            94 /* '^' */ => Token::new(TokenKind::BitwiseXor, None, TokenCategory::Operators),
            126 /* '~' */ => Token::new(TokenKind::BitwiseNot, None, TokenCategory::Operators),
            10 /* '\n' */ => {
                if self.peek() == Some(10) {
                    self.read_character();
                }
                match self.indent_diff() {
                    val if val > 0 => Token::new(
                        TokenKind::Indent,
                        Some(val.to_string()),
                        TokenCategory::Whitespace,
                    ),
                    val if val < 0 => Token::new(
                        TokenKind::Dedent,
                        Some(val.abs().to_string()),
                        TokenCategory::Whitespace,
                    ),
                    0 => Token::new(TokenKind::Newline, None, TokenCategory::Whitespace),
                    _ => unreachable!("Indentation error"),
                }
            },
            48..=57 /* '0'..'9' */  => {
                // Start of a number literal
                let mut number_str = self.read_num(); // You need to define read_number method
                if let Some(next_char) = self.peek() {
                    // Check for a floating-point literal
                    if next_char == 46 /* '.' */ {
                        self.read_character(); // Consume the '.'
                        number_str.push('.'); // Add '.' to the number string
                        number_str.push_str(&self.read_num()); // Append the rest of the floating-point number
                    }
                }
                return Ok(Token::new(
                    TokenKind::Ident,
                    Some(number_str),
                    TokenCategory::Literal,
                ));
            },
            // b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
            _letter @ 97..=122 /* 'a'..'z' */ | _letter @ 65..=90 /* 'A'..'Z' */ | _letter @ 95 /* '_' */ => {
                let ident = self.read_ident();

                let (token_type, token_category) = match ident.as_str() {
                    "def" => (TokenKind::Def, TokenCategory::Keyword),
                    "from" => (TokenKind::From, TokenCategory::Keyword),
                    "import" => (TokenKind::Import, TokenCategory::Keyword),
                    "if" => (TokenKind::If, TokenCategory::Keyword),
                    "else" => (TokenKind::Else, TokenCategory::Keyword),
                    "elif" => (TokenKind::Elif, TokenCategory::Keyword),
                    "for" => (TokenKind::For, TokenCategory::Keyword),
                    "while" => (TokenKind::While, TokenCategory::Keyword),
                    "False" => (TokenKind::False, TokenCategory::Keyword),
                    "True" => (TokenKind::True, TokenCategory::Keyword),
                    "continue" => (TokenKind::Continue, TokenCategory::Keyword),
                    "break" => (TokenKind::Break, TokenCategory::Keyword),
                    "del" => (TokenKind::Del, TokenCategory::Keyword),
                    "global" => (TokenKind::Global, TokenCategory::Keyword),
                    "local" => (TokenKind::Local, TokenCategory::Keyword),
                    "nonlocal" => (TokenKind::Nonlocal, TokenCategory::Keyword),
                    "try" => (TokenKind::Try, TokenCategory::Keyword),
                    "except" => (TokenKind::Except, TokenCategory::Keyword),
                    "as" => (TokenKind::As, TokenCategory::Keyword),
                    "finally" => (TokenKind::Finally, TokenCategory::Keyword),
                    "is" => (TokenKind::Is, TokenCategory::Keyword),
                    "in" => (TokenKind::In, TokenCategory::Keyword),
                    "not" => (TokenKind::Not, TokenCategory::Keyword),
                    "lambda" => (TokenKind::Lambda, TokenCategory::Keyword),
                    "return" => (TokenKind::Return, TokenCategory::Keyword),
                    "with" => (TokenKind::With, TokenCategory::Keyword),
                    "yield" => (TokenKind::Yield, TokenCategory::Keyword),
                    "pass" => (TokenKind::Pass, TokenCategory::Keyword),
                    // Add cases for built-in functions, types, etc.
                    "print" => (TokenKind::Print, TokenCategory::BuiltInFn),
                    // ...
                    // Default case for identifiers
                    _ => (TokenKind::Ident, TokenCategory::Identifier),
                };
                if token_type == TokenKind::Ident {
                    return Ok(Token::new(
                        token_type,
                        Some(String::from(ident)),
                        token_category,
                    ));
                }
                return Ok(Token::new(token_type, None, token_category));
            },
            0 => Token::new(TokenKind::Eof, None, TokenCategory::Eof),
            char => unreachable!("shouldn't reach this, tried to match {}", char),
        };

        self.read_character();
        Ok(token)
    }

    pub fn handle_next_whitespace(&mut self) {
        while self.read_position <= self.input.len() && WHITESPACE.contains(&self.character) {
            self.read_character();
        }
    }

    pub fn indent_diff(&mut self) -> i32 {
        let mut indent_length = 0;
        let initial = self.current_indent;
        while self.read_position < self.input.len()
            && WHITESPACE.contains(&self.input[self.read_position])
        {
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
        while let _letter @ 97..=122 /* 'a'..'z' */ | _letter @ 65..=90 /* 'A'..'Z' */ | _letter @ 95 /* '_' */ = self.character {
            self.read_character();
        }
        let sequence = &self.input[pos..self.position];
        sequence
            .iter()
            .filter_map(|&code_point| std::char::from_u32(code_point))
            .collect()
    }

    pub fn read_num(&mut self) -> String {
        let pos = self.position;
        while let _number @ 48..=57 = self.character {
            self.read_character();
        }
        let sequence = &self.input[pos..self.position];
        sequence
            .iter()
            .filter_map(|&code_point| std::char::from_u32(code_point))
            .collect()
    }

    pub fn peek(&mut self) -> Option<u32> {
        if self.read_position >= self.input.len()
            || WHITESPACE.contains(&self.input[self.read_position])
        {
            return None;
        }
        Some(self.input[self.read_position])
    }

    pub fn double_peek(&mut self) -> Option<String> {
        if self.read_position + 1 >= self.input.len() {
            return None;
        }

        let sequence = &self.input[self.read_position..self.position + 1];
        Some(
            sequence
                .iter()
                .filter_map(|&code_point| std::char::from_u32(code_point))
                .collect(),
        )
    }
}
