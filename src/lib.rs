#[allow(unused_macros)]

pub mod lexer;

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token, TokenCategory, TokenKind};

    #[test]
    fn handle_trailing_whitespace() {
        let mut lexer = Lexer {
            character: 0,
            current_indent: 0,
            position: 0,
            read_position: 0,
            tokens: Vec::<Token>::new(),
            // input: vec![97, 97, 97, 32],
            input: vec![62, 160],
        };
        lexer.read_character();

        // let output = vec![Token::new(TokenKind::Ident, Some(String::from("aaa")), TokenCategory::Identifier)];
        let output = vec![
            Token::new(TokenKind::Greater, None, TokenCategory::Comparison),
            Token::new(TokenKind::Eof, None, TokenCategory::Eof),
        ];

        lexer.tokenize_input();
        assert_eq!(lexer.tokens, output)
    }

    #[test]
    fn single_tokens() {
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
        run_tests(exps)
    }

    #[test]
    fn one_line_expressions() {
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

        run_tests_explicit(input, tokens);
    }

    fn run_tests_explicit(input: &str, tokens: Vec<Token>) {
        let mut lexer = Lexer::new(Some(input));
        lexer.current_indent = 4;

        for token in tokens {
            let current = lexer.tokenize_next_character();
            println!("{:?} {:?}", token, current);
            assert!(token == current.unwrap());
        }
    }

    fn run_tests(exps: Vec<(&str, Vec<Token>)>) {
        for (input, expected) in exps {
            let mut lexer = Lexer::new(Some(input));
            for (i, exp_token) in expected.iter().enumerate() {
                match lexer.tokenize_next_character() {
                    Ok(token) => assert_eq!(&token, exp_token, "Test failed at token index: {}", i),
                    Err(_) => panic!("Tokenization failed at index: {}", i),
                }
            }
        }
    }
}
