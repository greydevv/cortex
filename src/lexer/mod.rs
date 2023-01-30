use std::iter::Peekable;

use crate::io::file::SourceLocation;
use crate::lexer::token::{ Token, TokenKind, OpKind };

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

    pub fn next_token(&mut self) -> Token {
        self.skip_junk();
        if self.eof() {
            return Token::eof(self.loc);
        } else if self.c.is_numeric() {
            return self.lex_num();
        } else {
            return self.lex_other();
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

    fn lex_other(&mut self) -> Token {
        let loc = self.loc;
        let (kind, val) = match self.c {
            ';' => (TokenKind::Scolon, String::from(";")),
            '+' => (TokenKind::Op(OpKind::Plus), String::from("+")),
            _ => (TokenKind::Unknown, self.c.to_string()),
        };
        self.next_char();
        Token::new(kind, val, loc)
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
    fn empty_file() {
        let src = String::new();
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token();
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(1, 1));
    }

    #[test]
    fn leading_whitespace() {
        let src = String::from("  \n\n");
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token();
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(3, 1));
    }

    #[test]
    fn trailing_whitespace() {
        let src = String::from(";\n\n  ");
        let mut lexer = Lexer::new(&src);

        lexer.next_token();
        let tok = lexer.next_token();
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(3, 3));
    }

    #[test]
    fn leading_and_trailing_whitespace() {
        let src = String::from("  \n\n  \n  ");
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token();
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.val, String::from("\0"));
        assert_eq!(tok.loc, SourceLocation::new(4, 3));
    }
}
