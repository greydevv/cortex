use std::iter::Peekable;

use crate::io::file::SourceLocation;
use crate::io::error::LexicalError;
use crate::lexer::token::{ Token, TokenKind, OpKind, KwdKind };

pub mod token;

pub struct Lexer<'a> {
    c: char,
    loc: SourceLocation,
    chars: Peekable<std::str::Chars<'a>>
}

impl<'a> Lexer<'_> {
    pub fn new(src: &'a String) -> Lexer<'a> {
        let mut chars = src.chars().peekable();
        let c = match chars.next() {
            Some(c) => c,
            None => '\0'
        };
        Lexer {
            c,
            loc: SourceLocation::default(),
            chars,
        }
    }

    fn next_char(&mut self) {
        self.c = match self.chars.next() {
            Some(c) => c,
            None => '\0',
        };

        match self.c {
            '\n' => {
                self.loc.line += 1;
                self.loc.col = 1;
            },
            '\0' => (),
            _ => self.loc.col += 1,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexicalError> {
        self.skip_junk();
        if self.eof() {
            return Ok(Token::eof(self.loc));
        } else if self.c.is_alphabetic() {
            return Ok(self.lex_alpha());
        } else if self.c.is_numeric() {
            return Ok(self.lex_num());
        } else {
            return self.lex_other();
        }
    }

    fn lex_alpha(&mut self) -> Token {
        let mut val = String::new();
        let loc = self.loc;
        while self.c.is_alphanumeric() {
            val.push(self.c);
            self.next_char();
        }
        match KwdKind::from_string(&val) {
            Some(kind) => Token::new(TokenKind::Kwd(kind), val, loc),
            None => Token::new(TokenKind::Id, val, loc),
        }
    }

    fn lex_num(&mut self) -> Token {
        let mut val = String::new();
        let loc = self.loc;
        while self.c.is_numeric() {
            val.push(self.c);
            self.next_char();
        }
        Token::new(TokenKind::Num, val, loc)
    }

    fn lex_other(&mut self) -> Result<Token, LexicalError> {
        let loc = self.loc;
        let (kind, val) = match self.c {
            ';' => (TokenKind::Scolon, String::from(";")),
            '+' => (TokenKind::Op(OpKind::Plus), String::from("+")),
            '"' => return self.lex_string(),
            _ => (TokenKind::Unknown, self.c.to_string()),
        };
        self.next_char();
        Ok(Token::new(kind, val, loc))
    }

    fn lex_string(&mut self) -> Result<Token, LexicalError> {
        let mut value = String::new();
        // TODO: when computing length of token for future implementation of SourceLocation, need
        // to make sure that the quotes are included in final token length.
        let loc = self.loc.clone();

        // eat opening quote
        self.next_char();

        loop {
            match self.c {
                '\0' => {
                    return Err(LexicalError::syntax_err("Unterminated string literal."));
                },
                '"' => {
                    // eat closing quote
                    self.next_char();
                    return Ok(Token::new(
                        TokenKind::String,
                        value,
                        loc,
                    ));
                },
                _ => value.push(self.c)
            }
            self.next_char();
        }
    }

    fn skip_junk(&mut self) {
        self.skip_whitespace();
        if self.c == '/' && *self.chars.peek().unwrap() == '/'
        {
            self.skip_comment();
            self.skip_junk();
        }
    }

    fn skip_comment(&mut self) {
        // skip entire line
        while self.c != '\n' && self.c != '\0' {
            self.next_char();
        }
    }

    fn skip_whitespace(&mut self) {
        while self.c.is_whitespace() {
            self.next_char();
        }
    }

    pub fn eof(&self) -> bool {
        self.c == '\0'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_file() -> Result<(), LexicalError> {
        let src = String::new();
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(1, 1));
        Ok(())
    }

    #[test]
    fn leading_whitespace() -> Result<(), LexicalError> {
        let src = String::from("  \n\n");
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(3, 1));
        Ok(())
    }

    #[test]
    fn trailing_whitespace() -> Result<(), LexicalError> {
        let src = String::from(";\n\n  ");
        let mut lexer = Lexer::new(&src);

        lexer.next_token()?;
        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(3, 3));
        Ok(())
    }

    #[test]
    fn leading_and_trailing_whitespace() -> Result<(), LexicalError> {
        let src = String::from("  \n\n  \n  ");
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(4, 3));
        Ok(())
    }

    #[test]
    fn keywords() -> Result<(), LexicalError> {
        let src = String::from("func include for");
        let mut lexer = Lexer::new(&src);
        let expected_toks = vec![
            Token::new(TokenKind::Kwd(KwdKind::Func), String::from("func"), SourceLocation::new(1, 1)),
            Token::new(TokenKind::Kwd(KwdKind::Include), String::from("include"), SourceLocation::new(1, 6)),
            Token::new(TokenKind::Kwd(KwdKind::For), String::from("for"), SourceLocation::new(1, 14)),
            Token::new(TokenKind::EOF, String::from("\0"), SourceLocation::new(1, 16)),
        ];

        for expected in expected_toks {
            let tok = lexer.next_token()?;
            assert_eq!(tok.kind, expected.kind);
            assert_eq!(tok.val, expected.val);
            assert_eq!(tok.loc, expected.loc);
        }
        Ok(())
    }

    #[test]
    fn unterminated_string_literal() {
        // Lexer will encounter EOF before a closing double-quote
        let src = String::from("\"Hello, world!");
        let mut lexer = Lexer::new(&src);
        let result = lexer.next_token();
        let expected = LexicalError::syntax_err("Unterminated string literal.");

        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), expected);
    }


}
