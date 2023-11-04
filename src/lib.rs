#[derive(PartialEq, Debug)]
pub enum TokenCategory {
    Keyword,
    Dunder,
    BuiltInType,
    BuiltInFn,
    PunctuationAndGroup,
    Operators,
    Comparison
}

pub enum TokenKind {
    Abs
}

#[derive(PartialEq, Debug)]
pub enum Token {
    Indent(i32),
    Dedent(i32),

    // Standard keywords
    False(TokenCategory),
    None(TokenCategory),
    True(TokenCategory),
    And(TokenCategory),
    As(TokenCategory),
    Assert(TokenCategory),
    Async(TokenCategory),
    Await(TokenCategory),
    Break(TokenCategory),
    Class(TokenCategory),
    Continue(TokenCategory),
    Def(TokenCategory),
    Del(TokenCategory),
    Elif(TokenCategory),
    Else(TokenCategory),
    Except(TokenCategory),
    Finally(TokenCategory),
    For(TokenCategory),
    From(TokenCategory),
    Global(TokenCategory),
    If(TokenCategory),
    Import(TokenCategory),
    In(TokenCategory),
    Is(TokenCategory),
    Lambda(TokenCategory),
    Local(TokenCategory),
    Nonlocal(TokenCategory),
    Not(TokenCategory),
    Or(TokenCategory),
    Pass(TokenCategory),
    Raise(TokenCategory),
    Return(TokenCategory),
    Try(TokenCategory),
    While(TokenCategory),
    With(TokenCategory),
    Yield(TokenCategory),

    // Special methods (commonly used(TokenCategory), but not reserved keywords)
    Init(TokenCategory),           // __init__
    New(TokenCategory),            // __new__
    Delitem(TokenCategory),        // __delitem__
    Getitem(TokenCategory),        // __getitem__
    Setitem(TokenCategory),        // __setitem__
    Str(TokenCategory),            // __str__
    Repr(TokenCategory),           // __repr__
    Len(TokenCategory),            // __len__
    // Special cases
    Name(TokenCategory),           // __name__
    Doc(TokenCategory),            // __doc__
    Package(TokenCategory),        // __package__
    Loader(TokenCategory),         // __loader__
    Spec(TokenCategory),           // __spec__
    Annotations(TokenCategory),    // __annotations__
    Builtins(TokenCategory),       // __builtins__

    // Built-in types
    Int(TokenCategory),
    Float(TokenCategory),
    Complex(TokenCategory),
    List(TokenCategory),
    Tuple(TokenCategory),
    RangeType(TokenCategory),
    String(TokenCategory),
    Set(TokenCategory),
    Dict(TokenCategory),
    FrozenSet(TokenCategory),
    ByteArray(TokenCategory),
    Bytes(TokenCategory),
    MemoryView(TokenCategory),
    Bool(TokenCategory),

    // Built-in functions
    Abs(TokenCategory),
    All(TokenCategory),
    Any(TokenCategory),
    Ascii(TokenCategory),
    Bin(TokenCategory),
    BoolFn(TokenCategory),
    Breakpoint(TokenCategory),
    Bytearray(TokenCategory),
    BytesFn(TokenCategory),
    Callable(TokenCategory),
    Chr(TokenCategory),
    Classmethod(TokenCategory),
    Compile(TokenCategory),
    ComplexFn(TokenCategory),
    Delattr(TokenCategory),
    DictFn(TokenCategory),
    Dir(TokenCategory),
    Divmod(TokenCategory),
    Enumerate(TokenCategory),
    Eval(TokenCategory),
    Exec(TokenCategory),
    Filter(TokenCategory),
    FloatFn(TokenCategory),
    Format(TokenCategory),
    Frozenset(TokenCategory),
    Getattr(TokenCategory),
    Globals(TokenCategory),
    Hasattr(TokenCategory),
    Hash(TokenCategory),
    Help(TokenCategory),
    Hex(TokenCategory),
    Id(TokenCategory),
    Input(TokenCategory),
    IntFn(TokenCategory),
    Isinstance(TokenCategory),
    Issubclass(TokenCategory),
    Iter(TokenCategory),
    LenFn(TokenCategory),
    ListFn(TokenCategory),
    Locals(TokenCategory),
    Map(TokenCategory),
    Max(TokenCategory),
    Memoryview(TokenCategory),
    Min(TokenCategory),
    Next(TokenCategory),
    Object(TokenCategory),
    Oct(TokenCategory),
    Open(TokenCategory),
    Ord(TokenCategory),
    Pow(TokenCategory),
    Print(TokenCategory),
    Property(TokenCategory),
    Range(TokenCategory),
    ReprFn(TokenCategory),
    Reversed(TokenCategory),
    Round(TokenCategory),
    SetFn(TokenCategory),
    Setattr(TokenCategory),
    Slice(TokenCategory),
    Sorted(TokenCategory),
    Staticmethod(TokenCategory),
    StrFn(TokenCategory),
    Sum(TokenCategory),
    Super(TokenCategory),
    TupleFn(TokenCategory),
    Type(TokenCategory),
    Vars(TokenCategory),
    Zip(TokenCategory),
    ImportFn(TokenCategory),         // __import__()

    // Punctuation and grouping
    LeftParenthesis(TokenCategory),  // (
    RightParenthesis(TokenCategory), // )
    LeftBracket(TokenCategory),      // [
    RightBracket(TokenCategory),     // ]
    LeftBrace(TokenCategory),        // {
    RightBrace(TokenCategory),       // }
    Comma(TokenCategory),            // ,
    Dot(TokenCategory),              // .
    Semicolon(TokenCategory),        // ;
    Colon(TokenCategory),            // :
    Arrow(TokenCategory),            // ->
    Ellipsis(TokenCategory),         // ...
    
    // Operators
    Plus(TokenCategory),             // +
    Minus(TokenCategory),            // -
    Multiply(TokenCategory),         // *
    Divide(TokenCategory),           // /
    Modulo(TokenCategory),           // %
    Power(TokenCategory),            // **
    FloorDivide(TokenCategory),      // //
    Increment(TokenCategory),        // ++
    Decrement(TokenCategory),        // --
    PlusEqual(TokenCategory),        // +=
    MinusEqual(TokenCategory),       // -=
    MultiplyEqual(TokenCategory),    // *=
    DivideEqual(TokenCategory),      // /=
    ModuloEqual(TokenCategory),      // %=
    PowerEqual(TokenCategory),       // **=
    FloorDivideEqual(TokenCategory), // //=
    AndEqual(TokenCategory),         // &=
    OrEqual(TokenCategory),          // |=
    XorEqual(TokenCategory),         // ^=
    ShiftLeftEqual(TokenCategory),   // <<=
    ShiftRightEqual(TokenCategory),  // >>=
    Assign(TokenCategory),           // =

    // Comparison and logical operators
    Equal(TokenCategory),            // ==
    NotEqual(TokenCategory),         // !=
    Greater(TokenCategory),          // >
    Less(TokenCategory),             // <
    GreaterEqual(TokenCategory),     // >=
    LessEqual(TokenCategory),        // <=
    AndCmp(TokenCategory),           // &&
    OrCmp(TokenCategory),            // ||
    NotCmp(TokenCategory),           // !
    BitwiseAnd(TokenCategory),       // &
    BitwiseOr(TokenCategory),        // |
    BitwiseXor(TokenCategory),       // ^
    BitwiseNot(TokenCategory),       // ~
    ShiftLeft(TokenCategory),        // <<
    ShiftRight(TokenCategory),       // >>

    Eof,
    Ident(String),
    Newline
}

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
            b'(' => Token::LeftParenthesis(TokenCategory::PunctuationAndGroup),
            b')' => Token::RightParenthesis(TokenCategory::PunctuationAndGroup),
            b'[' => Token::LeftBracket(TokenCategory::PunctuationAndGroup),
            b']' => Token::RightBracket(TokenCategory::PunctuationAndGroup),
            b'{' => Token::LeftBrace(TokenCategory::PunctuationAndGroup),
            b'}' => Token::RightBrace(TokenCategory::PunctuationAndGroup),
            b',' => Token::Comma(TokenCategory::PunctuationAndGroup),
            b'.' => {
                if self.double_peek() == Some(String::from("..")) {
                    self.read_character();
                    self.read_character();
                    Token::Ellipsis(TokenCategory::PunctuationAndGroup)
                } else {
                    Token::Dot(TokenCategory::PunctuationAndGroup)
                }
            },
            b';' => Token::Semicolon(TokenCategory::PunctuationAndGroup),
            b':' => Token::Colon(TokenCategory::PunctuationAndGroup),
            b'-' => {
                if let Some(val) = self.peek() {
                    self.read_character();
                    match val {
                        b'>' => Token::Arrow(TokenCategory::PunctuationAndGroup),
                        b'-' => Token::Decrement(TokenCategory::Operators),
                        b'=' => Token::MinusEqual(TokenCategory::Operators),
                        _ => unreachable!("Shouldn't reach this!")
                    }
                } else {
                    Token::Minus(TokenCategory::Operators)
                }
            },
            b'+' => {
                if let Some(val) = self.peek() {
                    self.read_character();
                    match val {
                        b'+' => Token::Increment(TokenCategory::Operators),
                        b'=' => Token::PlusEqual(TokenCategory::Operators),
                        _ => unreachable!("Shouldn't reach this on '+', got {}", val as char)
                    }
                } else {
                    Token::Plus(TokenCategory::Operators)
                }
            },
            b'*' => {
                if self.peek() == Some(b'*') {
                    self.read_character();
                    Token::Power(TokenCategory::Operators)
                } else {
                    Token::Multiply(TokenCategory::Operators)
                }
            },
            b'/' => {
                if self.peek() == Some(b'/') {
                    self.read_character();
                    Token::FloorDivide(TokenCategory::Operators)
                } else {
                    Token::Divide(TokenCategory::Operators)
                }
            },
            b'%' => Token::Modulo(TokenCategory::Operators),
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.read_character();
                    Token::Equal(TokenCategory::Comparison)
                } else {
                    Token::Assign(TokenCategory::Operators)
                }
            },
            b'!' => {
                if self.peek() == Some(b'=') {
                    self.read_character();
                    Token::NotEqual(TokenCategory::Comparison)
                } else {
                    Token::NotCmp(TokenCategory::Comparison)
                }
            },
            b'>' => {
                if let Some(val) = self.peek() {
                    self.read_character();
                    match val {
                        b'=' => Token::GreaterEqual(TokenCategory::Comparison),
                        b'>' => Token::ShiftRight(TokenCategory::Comparison),
                        _ => unreachable!("Shouldn't reach this on >")
                    } 
                } else {
                    Token::Greater(TokenCategory::Comparison)
                }
            }
            b'\n' => {
                match self.indent_diff() {
                    val if val > 0 => Token::Indent(val),
                    val if val < 0 => Token::Dedent(val),
                    0 => Token::Newline,
                    _ => unreachable!("Broke numbers")
                }
            },
            b'0'..=b'9' => {
                let num = self.read_num();
                return Ok(Token::Ident(num))
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return Ok(match ident.as_str() {
                    "def" => Token::Def(TokenCategory::Keyword),
                    "from" => Token::From(TokenCategory::Keyword),
                    "import" => Token::Import(TokenCategory::Keyword),
                    "if" => Token::If(TokenCategory::Keyword),
                    "else" => Token::Else(TokenCategory::Keyword),
                    "elif" => Token::Elif(TokenCategory::Keyword),
                    "for" => Token::For(TokenCategory::Keyword),
                    "while" => Token::While(TokenCategory::Keyword),
                    "False" => Token::False(TokenCategory::Keyword),
                    "True" => Token::True(TokenCategory::Keyword),
                    "continue" => Token::Continue(TokenCategory::Keyword),
                    "break" => Token::Break(TokenCategory::Keyword),
                    "del" => Token::Del(TokenCategory::Keyword),
                    "global" => Token::Global(TokenCategory::Keyword),
                    "local" => Token::Local(TokenCategory::Keyword),
                    "nonlocal" => Token::Nonlocal(TokenCategory::Keyword),
                    "try" => Token::Try(TokenCategory::Keyword),
                    "except" => Token::Except(TokenCategory::Keyword),
                    "as" => Token::As(TokenCategory::Keyword),
                    "finally" => Token::Finally(TokenCategory::Keyword),
                    "is" => Token::Is(TokenCategory::Keyword),
                    "in" => Token::In(TokenCategory::Keyword),
                    "not" => Token::Not(TokenCategory::Keyword),
                    "lambda" => Token::Lambda(TokenCategory::Keyword),
                    "return" => Token::Return(TokenCategory::Keyword),
                    "with" => Token::With(TokenCategory::Keyword),
                    "yield" => Token::Yield(TokenCategory::Keyword),
                    "pass" => Token::Pass(TokenCategory::Keyword),
                    "print" => Token::Print(TokenCategory::BuiltInFn),
                    _ => {
                        Token::Ident(ident)
                    },
                })
            },
            0 => Token::Eof,
            char => unreachable!("shouldn't reach this, tried to match {} \n tokens: {:?}", char as char, self.tokens),
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
            return None
        } 
        Some(self.input[self.read_position])
    }

    pub fn double_peek(&mut self) -> Option<String> {
        if self.read_position + 1 >= self.input.len() {
            return None
        }

        Some(String::from_utf8_lossy(&self.input[self.read_position..=self.position+1]).to_string())
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
    use crate::{Lexer, Token, TokenCategory};

    #[test]
    fn single_tokens() {
        let exps = vec![
            ("variable_name", vec![Token::Ident(String::from("variable_name"))]),
            ("def", vec![Token::Def(TokenCategory::Keyword)]),
            ("**", vec![Token::Power(TokenCategory::Operators)]),
            ("my_func", vec![Token::Ident(String::from("my_func"))])
        ];
        run_tests(exps)
    }

    #[test]
    fn one_line_expressions() {
        let tokens = vec![
            Token::Def(TokenCategory::Keyword),
            Token::Ident(String::from("my_func")),
            Token::LeftParenthesis(TokenCategory::PunctuationAndGroup),
            Token::Ident(String::from("a")),
            Token::Comma(TokenCategory::PunctuationAndGroup),
            Token::Ident(String::from("b")),
            Token::RightParenthesis(TokenCategory::PunctuationAndGroup),
            Token::Colon(TokenCategory::PunctuationAndGroup),
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
        Token::Def(TokenCategory::Keyword),
        Token::Ident(String::from("my_func")),
        Token::LeftParenthesis(TokenCategory::PunctuationAndGroup),
        Token::Ident(String::from("a")),
        Token::Comma(TokenCategory::PunctuationAndGroup),
        Token::Ident(String::from("b")),
        Token::RightParenthesis(TokenCategory::PunctuationAndGroup),
        Token::Colon(TokenCategory::PunctuationAndGroup),
        Token::Indent(4),
        Token::Return(TokenCategory::Keyword),
        Token::Ident(String::from("a")),
        Token::Plus(TokenCategory::Operators),
        Token::Ident(String::from("b")),
        Token::Dedent(-4),
        Token::Newline,
        Token::Ident(String::from("res")),
        Token::Assign(TokenCategory::Operators),
        Token::Ident(String::from("my_func")),
        Token::LeftParenthesis(TokenCategory::PunctuationAndGroup),
        Token::Ident(String::from("1")),
        Token::Comma(TokenCategory::PunctuationAndGroup),
        Token::Ident(String::from("32")),
        Token::RightParenthesis(TokenCategory::PunctuationAndGroup),
        Token::Newline,
        Token::Print(TokenCategory::BuiltInFn),
        Token::LeftParenthesis(TokenCategory::PunctuationAndGroup),
        Token::Ident(String::from("res")),
        Token::RightParenthesis(TokenCategory::PunctuationAndGroup),
        Token::Newline
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
        for e in exps {
            let mut lex = Lexer::new(e.0.into());
            lex.tokenize_input();
            println!("Need: {:?}, found {:?}", e.1, lex.tokens);
            assert!(lex.tokens == e.1)
        }
    }
}
