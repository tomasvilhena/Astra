use miette::SourceSpan;
use super::error::{LexError, LexResult};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind
{
  // Keywords
  Entry,
  Function,
  Return,
  Let,
  If,
  Else,
  Match,
  Repeat,
  While,
  Break,
  Continue,
  Try,
  On,
  As,
  Print,
  Println,

  // Types
  NumberType,
  StringType,
  BoolType,
  VoidType,
  ArrayType,

  // Literals
  Identifier,
  NumberLiteral,
  StringLiteral,
  BoolLiteral,

  // Symbols
  Colon,
  Semicolon,
  Comma,
  Dot,
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  LeftBracket,
  RightBracket,

  // Operators
  Assign,
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  Caret,
  PlusEqual,
  MinusEqual,
  StarEqual,
  SlashEqual,
  PercentEqual,
  CaretEqual,

  // Comparison
  Equal,
  NotEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,

  // Logical
  And,
  Or,
  Not,
  KeywordAnd,
  KeywordOr,
  KeywordNot,

  // Ranges
  RangeExclusive,
  RangeInclusive,

  // Special
  Arrow,
  Underscore,
  EndOfFile,
}

#[derive(Debug, Clone)]
pub struct Token
{
  pub token_kind: TokenKind,
  pub lexed_value: String,
  pub span: SourceSpan,
}


#[derive(Debug, Clone)]
pub struct Lexer
{
  pub source_code: String,
  pub position: usize,
  pub line: usize,
  pub col: usize,
}

impl Lexer
{
  pub fn new(source_code: &str) -> Lexer
  {
    Lexer
    {
      source_code: source_code.to_string(),
      position: 0,
      line: 1,
      col: 0,
    }
  }

  fn next_char(&self, offset: usize) -> Option<char>
  {
    if self.position >= self.source_code.len()
    {
      return None;
    }

    self.source_code[self.position..].chars().nth(offset)
  }

  fn make_span(&self, start_pos: usize) -> SourceSpan
  {
    let end_pos = self.position;
    SourceSpan::new(start_pos.into(), (end_pos - start_pos).into())
  }

  fn make_token(&self, kind: TokenKind, lexed_value: String, start_pos: usize) -> Token
  {
    Token
    {
      token_kind: kind,
      lexed_value,
      span: self.make_span(start_pos),
    }
  }

  pub fn peek(&self) -> Option<char>
  {
    self.next_char(0)
  }

  pub fn advance(&mut self) -> Option<char>
  {
    let ch = self.peek()?;
    let ch_len = ch.len_utf8();
    self.position += ch_len;

    if ch == '\n'
    {
      self.line += 1;
      self.col = 0;
    } else
    {
      self.col += 1;
    }
    Some(ch)
  }

  pub fn skip_whitespace_and_comments(&mut self)
  {
    loop
    {
      let Some(ch) = self.peek() else { break };

      if ch.is_whitespace()
      {
        self.advance();
        continue;
      }

      // comments
      if ch == '/'
      {
        match self.next_char(1)
        {
          Some('/') =>
          {
            // line comment
            self.advance();
            self.advance();
            while let Some(c) = self.peek()
            {
              if c == '\n'
              {
                break;
              }
              self.advance();
            }
            continue;
          },

          Some('*') =>
          {
            // block comment
            self.advance();
            self.advance();
            while let Some(c) = self.peek()
            {
              if c == '*' && self.next_char(1) == Some('/')
              {
                self.advance();
                self.advance();
                break;
              }
              self.advance();
            }
            continue;
          },

          _ => {}
        }
      }

      break;
    }
  }

  pub fn next_token(&mut self) -> LexResult<Option<Token>>
  {
    self.skip_whitespace_and_comments();

    if self.position >= self.source_code.len()
    {
      return Ok(None);
    }

    let start_pos = self.position;

    let ch = self.peek().unwrap();

    // Identifiers & keywords
    if ch.is_ascii_alphabetic() || ch == '_'
    {
      let mut ident = String::new();

      while let Some(c) = self.peek()
      {
        if c.is_ascii_alphanumeric() || c == '_'
        {
          ident.push(c);
          self.advance();
        } else
        {
          break;
        }
      }

      let kind = match ident.as_str()
      {
        "entry" => TokenKind::Entry,
        "fn" => TokenKind::Function,
        "return" => TokenKind::Return,
        "let" => TokenKind::Let,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "match" => TokenKind::Match,
        "while" => TokenKind::While,
        "break" => TokenKind::Break,
        "continue" => TokenKind::Continue,
        "try" => TokenKind::Try,
        "on" => TokenKind::On,
        "as" => TokenKind::As,
        "print" => TokenKind::Print,
        "println" => TokenKind::Println,
        "repeat" => TokenKind::Repeat,
        "number" => TokenKind::NumberType,
        "string" => TokenKind::StringType,
        "bool" => TokenKind::BoolType,
        "void" => TokenKind::VoidType,
        "array" => TokenKind::ArrayType,
        "AND" => TokenKind::KeywordAnd,
        "OR" => TokenKind::KeywordOr,
        "NOT" => TokenKind::KeywordNot,
        "true" | "false" => TokenKind::BoolLiteral,
        "_" => TokenKind::Underscore,
        _ => TokenKind::Identifier,
      };

      return Ok(Some(self.make_token(kind, ident, start_pos)));
    }

    // Number
    if ch.is_ascii_digit()
    {
      let mut value = String::new();
      let mut seen_dot = false;

      while let Some(c) = self.peek()
      {
        if c.is_ascii_digit()
        {
          value.push(c);
          self.advance();
        } else if c == '.' && !seen_dot && self.next_char(1) != Some('.')
        {
          seen_dot = true;
          value.push(c);
          self.advance();
        } else
        {
          break;
        }
      }

      return Ok(Some(self.make_token(
        TokenKind::NumberLiteral,
        value,
        start_pos
      )));
    }

    // String literals
    if ch == '"'
    {
      self.advance(); // opening quote
      let mut string_val = String::new();

      while let Some(c) = self.peek()
      {
        if c == '"'
        {
          self.advance(); // closing quote
          return Ok(Some(self.make_token(
            TokenKind::StringLiteral,
            string_val,
            start_pos
          )));
        }

        string_val.push(c);
        self.advance();
      }

      return Err(LexError::UnterminatedString
      {
        span: SourceSpan::new(start_pos.into(), 1usize.into()),
      });
    }

    if ch == '.' && self.next_char(1) == Some('.')
    {
      if self.next_char(2) == Some('=')
      {
        self.advance();
        self.advance();
        self.advance();

        return Ok(Some(self.make_token(
          TokenKind::RangeInclusive,
          "..=".to_string(),
          start_pos
        )));
      } else
      {
        self.advance();
        self.advance();
        return Ok(Some(self.make_token(
          TokenKind::RangeExclusive,
          "..".to_string(),
          start_pos
        )));
      }
    }

    // Two-character operators
    let two = format!("{}{}", ch, self.next_char(1).unwrap_or('\0'));
    let kind = match two.as_str()
    {
      "==" => Some(TokenKind::Equal),
      "!=" => Some(TokenKind::NotEqual),
      ">=" => Some(TokenKind::GreaterEqual),
      "<=" => Some(TokenKind::LessEqual),
      "+=" => Some(TokenKind::PlusEqual),
      "-=" => Some(TokenKind::MinusEqual),
      "*=" => Some(TokenKind::StarEqual),
      "/=" => Some(TokenKind::SlashEqual),
      "%=" => Some(TokenKind::PercentEqual),
      "^=" => Some(TokenKind::CaretEqual),
      "&&" => Some(TokenKind::And),
      "||" => Some(TokenKind::Or),
      "=>" => Some(TokenKind::Arrow),
      _ => None,
    };

    if let Some(token_kind) = kind
    {
      self.advance();
      self.advance();
      return Ok(Some(self.make_token(token_kind, two, start_pos)));
    }

    // Single-character tokens
    self.advance();
    let token_kind = match ch
    {
      '=' => TokenKind::Assign,
      '+' => TokenKind::Plus,
      '-' => TokenKind::Minus,
      '*' => TokenKind::Star,
      '/' => TokenKind::Slash,
      '%' => TokenKind::Percent,
      '^' => TokenKind::Caret,
      ':' => TokenKind::Colon,
      ';' => TokenKind::Semicolon,
      ',' => TokenKind::Comma,
      '.' => TokenKind::Dot,
      '(' => TokenKind::LeftParen,
      ')' => TokenKind::RightParen,
      '{' => TokenKind::LeftBrace,
      '}' => TokenKind::RightBrace,
      '[' => TokenKind::LeftBracket,
      ']' => TokenKind::RightBracket,
      '!' => TokenKind::Not,
      '_' => TokenKind::Underscore,
      '<' => TokenKind::Less,
      '>' => TokenKind::Greater,

      _ =>
      {
        return Err(LexError::UnexpectedCharacter
        {
          ch,
          span: SourceSpan::new(start_pos.into(), ch.len_utf8().into()),
        });
      }
    };

    Ok(Some(self.make_token(
      token_kind,
      ch.to_string(),
      start_pos
    )))
  }

  pub fn tokenize(&mut self) -> LexResult<Vec<Token>>
  {
    let mut tokens: Vec<Token> = Vec::new();

    while let Some(token) = self.next_token()?
    {
      tokens.push(token);
    }

    tokens.push(Token
    {
      token_kind: TokenKind::EndOfFile,
      lexed_value: String::new(),
      span: SourceSpan::new(self.position.into(), 0usize.into())
    });

    Ok(tokens)
  }
}
