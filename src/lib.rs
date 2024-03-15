#[allow(unused_macros, dead_code)]
pub mod lexer;

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, LexerMode, Token, TokenCategory, TokenKind};

    #[test]
    fn if_handles_trailing_whitespace() {
        let mut lexer = Lexer {
            character: 0,
            current_indent: 0,
            position: 0,
            read_position: 0,
            input: vec![97, 97, 97, 32],
            mode: LexerMode::Ast,
            debug_mode: false
        };
        lexer.read_character();

        let output = vec![
            Token::new(
                TokenKind::Ident,
                Some(String::from("aaa")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenKind::Eof, None, TokenCategory::Eof),
        ];
        let mut tokens: Vec<Token> = Vec::new();

        while lexer.read_position <= lexer.input.len() {
            if let Ok(token) = lexer.tokenize_next_character() {
                tokens.push(token);
            }
        }
        assert_eq!(tokens, output)
    }

    #[test]
    fn if_handles_single_tokens() {
        let exps = vec![
            (
                "variable_name",
                vec![Token::new(
                    TokenKind::Ident,
                    Some(String::from("variable_name")),
                    TokenCategory::Identifier,
                )],
            ),
            (
                "def",
                vec![Token::new(TokenKind::Def, None, TokenCategory::Keyword)],
            ),
            (
                "**",
                vec![Token::new(TokenKind::Power, None, TokenCategory::Operators)],
            ),
            (
                "my_func",
                vec![Token::new(
                    TokenKind::Ident,
                    Some(String::from("my_func")),
                    TokenCategory::Identifier,
                )],
            ),
        ];
        test_multiple(exps)
    }

    #[test]
    fn numbers_in_identity_names() {
        let tokens = vec![

            Token::new(TokenKind::Ident, Some(String::from("d1")), TokenCategory::Identifier),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("1")),
                TokenCategory::Whitespace,
            ),
            Token::new(TokenKind::Ident, Some(String::from("d2")), TokenCategory::Identifier),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("1")),
                TokenCategory::Whitespace,
            ),
            Token::new(TokenKind::Ident, Some(String::from("d3")), TokenCategory::Identifier),
        ];

        test_single("d1 d2 d3", tokens, LexerMode::Editor)
    }

    #[test]
    fn if_handles_one_line_expressions() {
        let tokens = vec![
            Token::new(TokenKind::Def, None, TokenCategory::Keyword),
            Token::new(
                TokenKind::Ident,
                Some(String::from("my_func")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenKind::Ident,
                Some(String::from("a")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenKind::Comma, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenKind::Ident,
                Some(String::from("b")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenKind::Colon, None, TokenCategory::PunctuationAndGroup),
        ];

        test_single("def my_func(a, b)\u{200B}:", tokens, LexerMode::Ast);
    }

    #[test]
    fn if_handles_multiline_input() {
        let input = r#"def my_func(a, b):
        return a +b

    res = my_func(1, 32)
    print(res)
    "#;

        let tokens = vec![
            Token::new(TokenKind::Def, None, TokenCategory::Keyword),
            Token::new(
                TokenKind::Ident,
                Some(String::from("my_func")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenKind::Ident,
                Some(String::from("a")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenKind::Comma, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenKind::Ident,
                Some(String::from("b")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenKind::Colon, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenKind::Indent,
                Some(String::from("4")),
                TokenCategory::Whitespace,
            ),
            Token::new(TokenKind::Return, None, TokenCategory::Keyword),
            Token::new(
                TokenKind::Ident,
                Some(String::from("a")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenKind::Plus, None, TokenCategory::Operators),
            Token::new(
                TokenKind::Ident,
                Some(String::from("b")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::Dedent,
                Some(String::from("4")),
                TokenCategory::Whitespace,
            ),
            Token::new(TokenKind::Newline, None, TokenCategory::Whitespace),
            Token::new(
                TokenKind::Ident,
                Some(String::from("res")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenKind::Assign, None, TokenCategory::Operators),
            Token::new(
                TokenKind::Ident,
                Some(String::from("my_func")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenKind::Ident,
                Some(String::from("1")),
                TokenCategory::Literal,
            ),
            Token::new(TokenKind::Comma, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenKind::Ident,
                Some(String::from("32")),
                TokenCategory::Literal,
            ),
            Token::new(
                TokenKind::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenKind::Newline, None, TokenCategory::Whitespace),
            Token::new(TokenKind::Print, None, TokenCategory::BuiltInFn),
            Token::new(
                TokenKind::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenKind::Ident,
                Some(String::from("res")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenKind::Newline, None, TokenCategory::Whitespace),
            Token::new(TokenKind::Eof, None, TokenCategory::Eof),
        ];

        test_single(input, tokens, LexerMode::Ast);
    }

    #[test]
    fn if_handles_singleline_comments() {
        let input = r#"# this is my comment
    "#;
        let tokens: Vec<Token> = vec![
            Token::new(
                TokenKind::CommentSingleline,
                Some(String::from("# this is my comment")),
                TokenCategory::Comment,
            ),
            Token::new(TokenKind::Newline, None, TokenCategory::Whitespace),
            Token::new(TokenKind::Eof, None, TokenCategory::Eof),
        ];

        test_single(input, tokens, LexerMode::Editor);
    }

    #[test]
    fn if_handles_string_1() {
        let input = r#""abc""#;
        let tokens: Vec<Token> = vec![Token::new(
            TokenKind::String,
            Some(String::from("abc")),
            TokenCategory::Literal,
        )];

        test_single(input, tokens, LexerMode::Editor);
    }

    #[test]
    fn if_editor_mode_keeps_whitespace_short() {
        let input = "def  ";

        let tokens: Vec<Token> = vec![
            Token::new(TokenKind::Def, None, TokenCategory::Keyword),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("2")),
                TokenCategory::Whitespace,
            ),
            Token::new(TokenKind::Eof, None, TokenCategory::Eof),
        ];

        test_single(input, tokens, LexerMode::Editor)
    }

    #[test]
    fn if_editor_mode_keeps_whitespace_long() {
        let input = r#"def my_func(a, b):
        return a +b
    "#;
        let tokens: Vec<Token> = vec![
            Token::new(TokenKind::Def, None, TokenCategory::Keyword),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("1")),
                TokenCategory::Whitespace,
            ),
            Token::new(
                TokenKind::Ident,
                Some(String::from("my_func")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::LeftParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(
                TokenKind::Ident,
                Some(String::from("a")),
                TokenCategory::Identifier,
            ),
            Token::new(TokenKind::Comma, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("1")),
                TokenCategory::Whitespace,
            ),
            Token::new(
                TokenKind::Ident,
                Some(String::from("b")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::RightParenthesis,
                None,
                TokenCategory::PunctuationAndGroup,
            ),
            Token::new(TokenKind::Colon, None, TokenCategory::PunctuationAndGroup),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("2")),
                TokenCategory::Whitespace,
            ),
            Token::new(
                TokenKind::Indent,
                Some(String::from("4")),
                TokenCategory::Whitespace,
            ),
            Token::new(TokenKind::Return, None, TokenCategory::Keyword),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("1")),
                TokenCategory::Whitespace,
            ),
            Token::new(
                TokenKind::Ident,
                Some(String::from("a")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("1")),
                TokenCategory::Whitespace,
            ),
            Token::new(TokenKind::Plus, None, TokenCategory::Operators),
            Token::new(
                TokenKind::Ident,
                Some(String::from("b")),
                TokenCategory::Identifier,
            ),
            Token::new(
                TokenKind::Whitespace,
                Some(String::from("3")),
                TokenCategory::Whitespace,
            ),
            Token::new(
                TokenKind::Dedent,
                Some(String::from("4")),
                TokenCategory::Whitespace,
            ),
            Token::new(TokenKind::Eof, None, TokenCategory::Eof),
        ];
        test_single(input, tokens, LexerMode::Editor);
    }

    fn test_single(input: &str, tokens: Vec<Token>, mode: LexerMode) {
        let mut lexer = Lexer::new(Some(input), mode, false);
        lexer.current_indent = 4;

        for token in tokens {
            let current = lexer.tokenize_next_character();
            println!("[INFO] {:?} {:?}", token, current);
            assert!(token == current.unwrap());
        }
    }

    fn test_multiple(exps: Vec<(&str, Vec<Token>)>) {
        for (input, expected) in exps {
            let mut lexer = Lexer::new(Some(input), LexerMode::Ast, false);
            for (i, exp_token) in expected.iter().enumerate() {
                match lexer.tokenize_next_character() {
                    Ok(token) => assert_eq!(&token, exp_token, "Test failed at token index: {}", i),
                    Err(_) => panic!("Tokenization failed at index: {}", i),
                }
            }
        }
    }
}
