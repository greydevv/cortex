use std::iter::Peekable;

use crate::io::file::SourceLocation;
use crate::io::error::CortexError;
use crate::lexer::token::{ Token, TokenKind, BracketKind, BracketFace, OpKind, KwdKind };

pub mod token;

pub struct Lexer<'a> {
    c: char,
    loc: SourceLocation,
    bracket_stack: Vec<Token>,
    chars: Peekable<std::str::Chars<'a>>,
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
            bracket_stack: Vec::new(),
            loc: SourceLocation::default(),
            chars,
        }
    }

    fn next_char(&mut self) {
        if self.c == '\0' {
            return;
        }
        match self.c {
            '\0' => (),
            '\n' => {
                self.loc.line += 1;
                self.loc.col = 1;
            },
            _ => self.loc.col += 1,
        }
        self.c = match self.chars.next() {
            Some(c) => c,
            None => '\0',
        };

    }

    pub fn next_token(&mut self) -> Result<Token, CortexError> {
        self.skip_junk();
        if self.eof() {
            // bracket balancing state (bracket_stack) should be empty if all opened brackets were
            // closed at some point
            match self.bracket_stack.last() {
                Some(unmatched_tok) => Err(CortexError::SyntaxError(format!("unclosed '{}'", unmatched_tok.val))),
                None => Ok(Token::eof(self.loc))
            }
        } else if self.c.is_alphabetic() {
            Ok(self.lex_alpha())
        } else if self.c.is_numeric() {
            Ok(self.lex_num())
        } else if self.c == '"' {
            self.lex_string()
        } else {
            self.lex_other()
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

    fn lex_other(&mut self) -> Result<Token, CortexError> {
        let loc = self.loc;
        let (kind, val) = match self.c {
            ';' => (TokenKind::Scolon, String::from(";")),
            '+' => (TokenKind::Op(OpKind::Plus), String::from("+")),
            '(' => (TokenKind::Brack(BracketKind::Paren, BracketFace::Open), String::from("(")),
            ')' => (TokenKind::Brack(BracketKind::Paren, BracketFace::Closed), String::from(")")),
            '{' => (TokenKind::Brack(BracketKind::Brace, BracketFace::Open), String::from("{")),
            '}' => (TokenKind::Brack(BracketKind::Brace, BracketFace::Closed), String::from("}")),
            '[' => (TokenKind::Brack(BracketKind::Square, BracketFace::Open), String::from("[")),
            ']' => (TokenKind::Brack(BracketKind::Square, BracketFace::Closed), String::from("]")),
            _ => (TokenKind::Unknown, self.c.to_string()),
        };
        // TODO: will need to loop this from 0..val.len() for multi-char tokens
        self.next_char();
        let tok = Token::new(kind, val, loc);

        match tok.kind {
            TokenKind::Brack(_, _) => self.update_balancing_state(tok.clone())?,
            _ => ()
        }

        Ok(tok)
    }

    fn update_balancing_state(&mut self, tok: Token) -> Result<(), CortexError> {
        match &tok.kind {
            TokenKind::Brack(_, brack_state) =>
                match brack_state {
                    BracketFace::Open => self.bracket_stack.push(tok),
                    BracketFace::Closed => {
                        let top_tok = match self.bracket_stack.pop() {
                            // proceed normally if the current closing bracket matches an opening bracket
                            // on top of stack
                            Some(opening_tok) if tok.closes(&opening_tok) => return Ok(()),
                            Some(opening_tok) => opening_tok,
                            // stack is empty, the current closing bracket is unopened
                            None => return Err(CortexError::SyntaxError(format!("unopened '{}'", tok.val)))
                        };
                        // unwind the balancing state (bracket_stack) to see if the current closing token
                        // was opened somewhere previously
                        while let Some(opening_tok) = self.bracket_stack.pop() {
                            // found a matching opening token, the token on the top of the stack was
                            // unclosed
                            if tok.closes(&opening_tok) {
                                return Err(CortexError::SyntaxError(format!("unclosed '{}'", top_tok.val)))
                            }
                        }
                        // did not find a matching opening token, the current closing token is unopened
                        return Err(CortexError::SyntaxError(format!("unopened '{}'", tok.val)));
                    },
                },
            _ => ()
        }
        Ok(())
    }

    fn lex_string(&mut self) -> Result<Token, CortexError> {
        let mut value = String::new();
        // TODO: when computing length of token for future implementation of SourceLocation, need
        // to make sure that the quotes are included in final token length.
        let loc = self.loc.clone();

        // eat opening quote
        self.next_char();

        loop {
            match self.c {
                '\0' => {
                    return Err(CortexError::syntax_err("unterminated string literal"));
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
    fn empty_file() -> Result<(), CortexError> {
        let src = String::new();
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(1, 1));
        Ok(())
    }

    #[test]
    fn leading_whitespace() -> Result<(), CortexError> {
        let src = String::from("  \n\n");
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(3, 1));
        Ok(())
    }

    #[test]
    fn trailing_whitespace() -> Result<(), CortexError> {
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
    fn leading_and_trailing_whitespace() -> Result<(), CortexError> {
        let src = String::from("  \n\n  \n  ");
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(4, 3));
        Ok(())
    }

    #[test]
    fn keywords() -> Result<(), CortexError> {
        let src = String::from("func include for");
        let mut lexer = Lexer::new(&src);
        let expected_toks = vec![
            Token::new(TokenKind::Kwd(KwdKind::Func), String::from("func"), SourceLocation::new(1, 1)),
            Token::new(TokenKind::Kwd(KwdKind::Include), String::from("include"), SourceLocation::new(1, 6)),
            Token::new(TokenKind::Kwd(KwdKind::For), String::from("for"), SourceLocation::new(1, 14)),
            Token::new(TokenKind::EOF, String::from("\0"), SourceLocation::new(1, 17)),
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
        let expected = CortexError::syntax_err("unterminated string literal");

        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), expected);
    }

    #[test]
    fn closed_brackets() -> Result<(), CortexError> {
        let src = String::from("([\n\n\n()]) [] {\n{{[{}]}\n}\n}");
        let expected_toks = vec![
            Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Open), String::from("("), SourceLocation::new(1, 1)),
            Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Open), String::from("["), SourceLocation::new(1, 2)),
            Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Open), String::from("("), SourceLocation::new(4, 1)),
            Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Closed), String::from(")"), SourceLocation::new(4, 2)),
            Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Closed), String::from("]"), SourceLocation::new(4, 3)),
            Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Closed), String::from(")"), SourceLocation::new(4, 4)),
            Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Open), String::from("["), SourceLocation::new(4, 6)),
            Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Closed), String::from("]"), SourceLocation::new(4, 7)),
            Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Open), String::from("{"), SourceLocation::new(4, 9)),
            Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Open), String::from("{"), SourceLocation::new(5, 1)),
            Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Open), String::from("{"), SourceLocation::new(5, 2)),
            Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Open), String::from("["), SourceLocation::new(5, 3)),
            Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Open), String::from("{"), SourceLocation::new(5, 4)),
            Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Closed), String::from("}"), SourceLocation::new(5, 5)),
            Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Closed), String::from("]"), SourceLocation::new(5, 6)),
            Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Closed), String::from("}"), SourceLocation::new(5, 7)),
            Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Closed), String::from("}"), SourceLocation::new(6, 1)),
            Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Closed), String::from("}"), SourceLocation::new(7, 1)),
            Token::eof(SourceLocation::new(7,2)),
        ];
        let mut lexer = Lexer::new(&src);

        for expected in expected_toks {
            let tok = lexer.next_token()?;
            assert_eq!(tok.kind, expected.kind);
            assert_eq!(tok.val, expected.val);
            assert_eq!(tok.loc, expected.loc);
        }
        Ok(())
    }

    #[test]
    fn unclosed_brackets() {
        let src = String::from("(\n{)");
        let mut lexer = Lexer::new(&src);

        for _ in 0..2 {
            assert!(lexer.next_token().is_ok());
        }

        let expected = CortexError::syntax_err("unclosed '{'");
        let result = lexer.next_token();

        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), expected);
    }

    #[test]
    fn unopened_brackets() {
        let src = String::from("(\n})");
        let mut lexer = Lexer::new(&src);

        // no need to loop, first next_token is only call that should succeed
        assert!(lexer.next_token().is_ok());

        let expected = CortexError::syntax_err("unopened '}'");
        let result = lexer.next_token();

        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), expected);
    }
}
