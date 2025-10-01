#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind
{
  // Keywords
  Entry,
  Include,
  Function,
  Return,
  Let,
  If,
  Else,
  Match,
  For,
  While,
  Do,
  Break,
  Continue,
  Try,
  On,
  Debug,
  Print,
  Println,
  Read,
  Clear,

  // Types
  IntType,
  FloatType,
  StringType,
  BoolType,
  VoidType,
  ArrayType,

  // Literals
  Identifier,     // variable or function name
  IntegerLiteral, // 123
  FloatLiteral,   // 12.34
  StringLiteral,  // "hello"
  BoolLiteral,    // true, false

  // Symbols
  Colon,          // :
  Semicolon,      // ;
  Comma,          // ,
  Dot,            // .
  LeftParen,      // (
  RightParen,     // )
  LeftBrace,      // {
  RightBrace,     // }
  LeftBracket,    // [
  RightBracket,   // ]

  // Operators
  Assign,         // =
  Plus,           // +
  Minus,          // -
  Star,           // *
  Slash,          // /
  Percent,        // %
  Caret,          // ^
  PlusEqual,      // +=
  MinusEqual,     // -=
  StarEqual,      // *=
  SlashEqual,     // /=
  PercentEqual,   // %=
  CaretEqual,     // ^=

  // Comparison
  Equal,          // ==
  NotEqual,       // !=
  Greater,        // >
  GreaterEqual,   // >=
  Less,           // <
  LessEqual,      // <=

  // Logical
  And,            // &&
  Or,             // ||
  Not,            // !
  KeywordAnd,     // AND
  KeywordOr,      // OR
  KeywordNot,     // NOT

  // Ranges
  RangeExclusive, // ..
  RangeInclusive, // ..=

  // Special
  Arrow,          // =>
  Underscore,     // _
  EndOfFile,
}


#[derive(Debug, Clone)]
pub struct Token
{
  pub token_kind: TokenKind,
  pub lexed_value: String,
  pub line: usize,
  pub col: usize,
}


#[derive(Debug, Clone)]
pub struct Lexer
{
  pub source_code: Vec<char>,
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
      source_code: source_code.chars().collect(),
      position: 0,
      line: 1,
      col: 0,
    }
  }

  pub fn peek(&self) -> Option<char>
  {
    if self.position >= self.source_code.len()
    {
      None
    } else
    {
      Some(self.source_code[self.position])
    }
  }

  pub fn advance(&mut self) -> Option<char>
  {
    if self.position >= self.source_code.len()
    {
      return None;
    }

    let character: char = self.source_code[self.position];
    self.position += 1;

    if character == '\n'
    {
      self.line += 1;
      self.col = 0;
    } else
    {
      self.col += 1;
    }

    Some(character)
  }

  pub fn skip_whitespace_and_comments(&mut self)
  {
    while let Some(character) = self.peek()
    {
      if character.is_whitespace()
      {
        self.advance();
        continue;
      }

      // /* */

      if character == '/'
      {
        if let Some(next_character) = self.source_code.get(self.position + 1)
        {
          if *next_character == '/'
          {
            self.advance();
            self.advance();

            while let Some(character) = self.peek()
            {
              if character == '\n'
              {
                break;
              }

              self.advance();
            }

            continue;
          } else if *next_character == '*'
          {
            self.advance();
            self.advance();

            while let Some(character) = self.peek()
            {
              if character == '*'
              {
                if let Some(next_character) = self.source_code.get(self.position + 1)
                {
                  if *next_character == '/'
                  {
                    self.advance();
                    self.advance();
                    break;
                  }
                }
              }

              self.advance();
            }

            continue;
          }
        }
      }
      break;
    }
  }

  pub fn next_token(&mut self) -> Option<Token>
  {
    self.skip_whitespace_and_comments();

    if self.position >= self.source_code.len()
    {
      return None;
    }

    let start_line = self.line;
    let start_col = self.col;
    let character = self.peek()?;

    // Identifiers & keywords
    if character.is_ascii_alphabetic() || character == '_'
    {
      let mut ident = String::new();
      while let Some(ch) = self.peek()
      {
        if ch.is_ascii_alphanumeric() || ch == '_'
        {
          ident.push(ch);
          self.advance();
        } else
        {
          break;
        }
      }

      let kind = match ident.as_str()
      {
        "entry" => TokenKind::Entry,
        "include" => TokenKind::Include,
        "fn" => TokenKind::Function,
        "return" => TokenKind::Return,
        "let" => TokenKind::Let,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "match" => TokenKind::Match,
        "for" => TokenKind::For,
        "while" => TokenKind::While,
        "do" => TokenKind::Do,
        "break" => TokenKind::Break,
        "continue" => TokenKind::Continue,
        "try" => TokenKind::Try,
        "on" => TokenKind::On,
        "debug" => TokenKind::Debug,
        "print" => TokenKind::Print,
        "println" => TokenKind::Println,
        "read" => TokenKind::Read,
        "clear" => TokenKind::Clear,
        "int" => TokenKind::IntType,
        "float" => TokenKind::FloatType,
        "string" => TokenKind::StringType,
        "bool" => TokenKind::BoolType,
        "void" => TokenKind::VoidType,
        "array" => TokenKind::ArrayType,
        "AND" => TokenKind::KeywordAnd,
        "OR" => TokenKind::KeywordOr,
        "NOT" => TokenKind::KeywordNot,
        "true" => TokenKind::BoolLiteral,
        "false" => TokenKind::BoolLiteral,
        _ => TokenKind::Identifier,
      };

      return Some(Token {
        token_kind: kind,
        lexed_value: ident,
        line: start_line,
        col: start_col,
      });
    }

    // Numbers (int or float)
    if character.is_ascii_digit()
    {
      let mut value = String::new();
      let mut is_float = false;

      while let Some(ch) = self.peek()
      {
        if ch.is_ascii_digit()
        {
          value.push(ch);
          self.advance();
        } else if ch == '.' && !is_float
        {
          is_float = true;
          value.push(ch);
          self.advance();
        } else
        {
          break;
        }
      }

      let kind = if is_float
      {
        TokenKind::FloatLiteral
      } else
      {
        TokenKind::IntegerLiteral
      };

      return Some(Token {
        token_kind: kind,
        lexed_value: value,
        line: start_line,
        col: start_col,
      });
    }

    // String literals
    if character == '"'
    {
      self.advance(); // skip opening quote
      let mut string_val = String::new();

      while let Some(ch) = self.peek()
      {
        if ch == '"'
        {
          self.advance(); // skip closing quote
          break;
        }
        string_val.push(ch);
        self.advance();
      }

      if self.peek().is_none()
      {
        panic!(
          "Unterminated string literal at line {}, col {}",
          start_line, start_col
        );
      }

      return Some(Token {
        token_kind: TokenKind::StringLiteral,
        lexed_value: string_val,
        line: start_line,
        col: start_col,
      });
    }

    // Helper for looking ahead
    let next = |offset: usize| self.source_code.get(self.position + offset).copied();

    // Ranges
    if character == '.'
    {
      if next(1) == Some('.')
      {
        if next(2) == Some('=')
        {
          self.advance();
          self.advance();
          self.advance();

          return Some(Token {
            token_kind: TokenKind::RangeInclusive,
            lexed_value: "..=".to_string(),
            line: start_line,
            col: start_col,
          });
        } else
        {
          self.advance();
          self.advance();

          return Some(Token {
            token_kind: TokenKind::RangeExclusive,
            lexed_value: "..".to_string(),
            line: start_line,
            col: start_col,
          });
        }
      }
    }

    // Two-character operators
    let two_chars: String = format!("{}{}", character, next(1).unwrap_or('\0'));
    let kind = match two_chars.as_str()
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

      return Some(Token {
        token_kind,
        lexed_value: two_chars,
        line: start_line,
        col: start_col,
      });
    }

    // Single-character tokens
    self.advance();

    let token_kind = match character
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
      _ => panic!("Unexpected character '{}' at line {}, col {}", character, start_line, start_col),
    };

    Some(Token {
      token_kind,
      lexed_value: character.to_string(),
      line: start_line,
      col: start_col,
    })
  }

  pub fn tokenize(&mut self) -> Vec<Token>
  {
    let mut tokens: Vec<Token> = Vec::new();

    while let Some(token) = self.next_token()
    {
      tokens.push(token);
    }

    tokens.push(Token{
      token_kind: TokenKind::EndOfFile,
      lexed_value: String::new(),
      line: self.line,
      col: self.col,
    });

    return tokens;
  }

}
