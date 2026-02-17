use super::ast::{BinaryOperator, Expr, Stmt, UnaryOperator, Pattern};
use super::lexer::{Token, TokenKind};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

pub struct Parser 
{
  tokens: Vec<Token>,
  position: usize,
}

#[derive(Error, Diagnostic, Debug)]
pub enum ParseError 
{
  #[error("Unexpected token: expected {expected:?}, found {found:?}")]
  #[diagnostic(code(parser::unexpected_token))]
  UnexpectedToken {
    expected: Vec<TokenKind>,
    found: TokenKind,
    #[label("here")]
    span: SourceSpan,
  },
  
  #[error("Missing token: expected {expected:?}")]
  #[diagnostic(code(parser::missing_token))]
  MissingToken 
  {
    expected: TokenKind,
    #[label("Expected here")]
    span: SourceSpan,
  },
  
  #[error("Invalid literal: {kind:?}")]
  #[diagnostic(code(parser::invalid_literal))]
  InvalidLiteral 
  {
    kind: TokenKind,
    #[label("This literal is invalid")]
    span: SourceSpan,
  },
  
  #[error("Unexpected end of file: expected {expected:?}")]
  #[diagnostic(code(parser::unexpected_eof))]
  UnexpectedEof 
  {
    expected: &'static str,
    #[label("End of file here")]
    span: SourceSpan,
  },

  #[error("missing mandatory entry directive")]
  #[diagnostic(code(parser::missing_entry))]
  MissingEntry {
    #[label("entry must appear at the top of the file")]
    span: SourceSpan,
  },

  #[error("entry directive must be the first top-level directive")]
  #[diagnostic(code(parser::entry_not_first))]
  EntryNotFirst {
    #[label("entry is only valid at the top of the file")]
    span: SourceSpan,
  },

  #[error("include directive is only allowed after entry and before normal code")]
  #[diagnostic(code(parser::include_not_in_header))]
  IncludeNotInHeader {
    #[label("include is only valid in the header section")]
    span: SourceSpan,
  },
}

type ParseResult<T> = Result<T, ParseError>;

impl Parser 
{
  fn current(&self) -> Option<&Token> 
  {
    self.tokens.get(self.position)
  }
  
  fn eof_span(&self) -> SourceSpan 
  {
    match self.tokens.last() 
    {
      Some(token) => token.span,
      None => SourceSpan::new(0usize.into(), 0usize.into()),
    }
  }
  
  fn current_span_or_eof(&self) -> SourceSpan 
  {
    match self.current() 
    {
      Some(token) => token.span,
      None => self.eof_span(),
    }
  }
  
  fn unexpected_eof<T>(&self, expected: &'static str) -> ParseResult<T> 
  {
    Err(ParseError::UnexpectedEof 
    {
      expected,
      span: self.current_span_or_eof(),
    })
  }
  
  fn advance_or_eof(&mut self, expected: &'static str) -> ParseResult<Token> {
    match self.advance() 
    {
      Some(token) => Ok(token),
      None => Err(ParseError::UnexpectedEof 
      {
        expected,
        span: self.current_span_or_eof(),
      }),
    }
  }
  
  fn insertion_before_current(&self) -> SourceSpan 
  {
    match self.current() 
    {
      Some(token) => SourceSpan::new(token.span.offset().into(), 0usize.into()),
      None => self.eof_span(),
    }
  }
  
  fn insertion_after_previus(&self) -> SourceSpan 
  {
    if self.position > 0 
    {
      if let Some(prev) = self.tokens.get(self.position - 1)
      {
        let end = prev.span.offset() + prev.span.len();
        return SourceSpan::new(end.into(), 0usize.into());
      }
    }
    
    self.insertion_before_current()
  }
  
  fn expect(&mut self, expected: TokenKind) -> ParseResult<Token> 
  {
    let token = match self.current() 
    {
      Some(token) => token.clone(),
      None => {
        return Err(ParseError::UnexpectedEof 
        {
          expected: "token",
          span: self.eof_span(),
        })
      }
    };
    
    if token.token_kind == expected 
    {
      self.position += 1;
      Ok(token)
    } else {
      match expected 
      {
        TokenKind::Semicolon => Err(ParseError::MissingToken 
        {
          expected,
          span: self.insertion_after_previus(),
        }),
        
        TokenKind::RightBrace
        | TokenKind::RightParen
        | TokenKind::RightBracket
        | TokenKind::Colon => Err(ParseError::MissingToken 
        {
          expected,
          span: self.insertion_before_current(),
        }),
        
        _ => Err(ParseError::UnexpectedToken 
        {
          expected: vec![expected],
          found: token.token_kind,
          span: token.span,
        }),
      }
    }
  }
  
  fn expect_any(&mut self, expected: &[TokenKind]) -> ParseResult<Token> 
  {
    let token = match self.current() 
    {
      Some(token) => token.clone(),
      None => {
        return Err(ParseError::UnexpectedEof 
        {
          expected: "token",
          span: self.eof_span(),
        })
      }
    };
    
    if expected.iter().any(|kind| kind == &token.token_kind) 
    {
      self.position += 1;
      Ok(token)
    } else 
    {
      Err(ParseError::UnexpectedToken 
      {
        expected: expected.to_vec(),
        found: token.token_kind,
        span: token.span,
      })
    }
  }
  
  fn token_to_binary_op(token: &TokenKind) -> Option<BinaryOperator> 
  {
    match token 
    {
      // Assignment & arithmetic assignment
      TokenKind::Assign => Some(BinaryOperator::Assign),
      TokenKind::Plus => Some(BinaryOperator::Plus),
      TokenKind::Minus => Some(BinaryOperator::Minus),
      TokenKind::Star => Some(BinaryOperator::Star),
      TokenKind::Slash => Some(BinaryOperator::Slash),
      TokenKind::Percent => Some(BinaryOperator::Percent),
      TokenKind::Caret => Some(BinaryOperator::Caret),
      TokenKind::PlusEqual => Some(BinaryOperator::PlusEqual),
      TokenKind::MinusEqual => Some(BinaryOperator::MinusEqual),
      TokenKind::StarEqual => Some(BinaryOperator::StarEqual),
      TokenKind::SlashEqual => Some(BinaryOperator::SlashEqual),
      TokenKind::PercentEqual => Some(BinaryOperator::PercentEqual),
      TokenKind::CaretEqual => Some(BinaryOperator::CaretEqual),
      
      // Comparisons
      TokenKind::Equal => Some(BinaryOperator::Equal),
      TokenKind::NotEqual => Some(BinaryOperator::NotEqual),
      TokenKind::Greater => Some(BinaryOperator::Greater),
      TokenKind::GreaterEqual => Some(BinaryOperator::GreaterEqual),
      TokenKind::Less => Some(BinaryOperator::Less),
      TokenKind::LessEqual => Some(BinaryOperator::LessEqual),
      
      // Logical
      TokenKind::And => Some(BinaryOperator::And),
      TokenKind::Or => Some(BinaryOperator::Or),
      TokenKind::KeywordAnd => Some(BinaryOperator::KeywordAnd),
      TokenKind::KeywordOr => Some(BinaryOperator::KeywordOr),
      
      // Ranges
      TokenKind::RangeExclusive => Some(BinaryOperator::RangeExclusive),
      TokenKind::RangeInclusive => Some(BinaryOperator::RangeInclusive),
      
      _ => None,
    }
  }
  
  fn token_to_unary_op(token: &TokenKind) -> Option<UnaryOperator> 
  {
    match token 
    {
      TokenKind::Minus => Some(UnaryOperator::Negative),
      TokenKind::Not => Some(UnaryOperator::Not),
      TokenKind::KeywordNot => Some(UnaryOperator::KeywordNot),
      _ => None,
    }
  }
  
  pub fn new(tokens: Vec<Token>) -> Self 
  {
    Self 
    {
      tokens,
      position: 0,
    }
  }
  
  pub fn peek(&self) -> Option<&Token> 
  {
    self.tokens.get(self.position)
  }
  
  pub fn peek_or_eof(&self, expected: &'static str) -> ParseResult<&Token> 
  {
    self.peek().ok_or_else(|| ParseError::UnexpectedEof 
    {
      expected,
      span: self.current_span_or_eof(),
    })
  }
  
  pub fn advance(&mut self) -> Option<Token> 
  {
    let token: Option<Token> = self.tokens.get(self.position).cloned();
    
    if token.is_some() 
    {
      self.position += 1;
    }
    
    token
  }

  fn parse_postfix(&mut self, base: Expr) -> ParseResult<Expr> 
  {
    let mut expr = base;

    loop 
    {
      let token_kind = match self.peek() 
      {
        Some(token) => token.token_kind.clone(),
        None => break,
      };

      match token_kind 
      {
        TokenKind::LeftParen =>
        {
          expr = self.finish_call(expr)?;
        },

        TokenKind::Dot =>
        {
          self.expect(TokenKind::Dot)?;
          let identifier = self.expect(TokenKind::Identifier)?.lexed_value;
          expr = Expr::Member 
          { 
            object: Box::new(expr), 
            property: identifier,
          }
        },

        TokenKind::LeftBracket =>
        {
          self.expect(TokenKind::LeftBracket)?;
          let index = self.parse_expr(0)?;
          self.expect(TokenKind::RightBracket)?;

          expr = Expr::Index 
          { 
            object: Box::new(expr), 
            index: Box::new(index),
          }
        }

        _ => break,
      }

      
    }

    Ok(expr)
  }
  
  // pub fn is_end_of_file(&self) -> bool
  // {
  //   match self.peek()
  //   {
  //     Some(token) => token.token_kind == TokenKind::EndOfFile,
  //     None => true,
  //   }
  // }
  
  pub fn get_binding_power(op: &TokenKind) -> Option<(u8, u8)> {
    match op 
    {
      // Assignment (right-associative)
      TokenKind::Assign
      | TokenKind::PlusEqual
      | TokenKind::MinusEqual
      | TokenKind::StarEqual
      | TokenKind::SlashEqual
      | TokenKind::PercentEqual
      | TokenKind::CaretEqual => Some((1, 0)),
      
      // Logical OR
      TokenKind::Or | TokenKind::KeywordOr => Some((2, 3)),
      
      // Logical AND
      TokenKind::And | TokenKind::KeywordAnd => Some((4, 5)),
      
      // Equality
      TokenKind::Equal | TokenKind::NotEqual => Some((6, 7)),
      
      // Comparisons
      TokenKind::Greater
      | TokenKind::GreaterEqual
      | TokenKind::Less
      | TokenKind::LessEqual => Some((8, 9)),
      
      // Addition & Subtraction
      TokenKind::Plus | TokenKind::Minus => Some((10, 11)),
      
      // Multiplication, Division, Modulo
      TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Some((12, 13)),
      
      // Exponentiation (right-associative)
      TokenKind::Caret => Some((14, 13)),
      
      // Ranges
      TokenKind::RangeExclusive | TokenKind::RangeInclusive => Some((15, 16)),
      
      _ => None,
    }
  }

  fn parse_break_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Break)?;
    self.expect(TokenKind::Semicolon)?;

    Ok(Stmt::Break)
  }

  fn parse_continue_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Continue)?;
    self.expect(TokenKind::Semicolon)?;

    Ok(Stmt::Continue)
  }
  
  pub fn parse_expr(&mut self, min_binding_power: u8) -> ParseResult<Expr> 
  {
    let mut left = self.parse_primary()?;
    left = self.parse_postfix(left)?;

    loop 
    {
      let op_token = match self.peek() 
      {
        Some(tok) => tok,
        None => break,
      };
      
      let (left_binding_power, right_binding_power) =
      match Self::get_binding_power(&op_token.token_kind) 
      {
        Some(binding_power) => binding_power,
        None => break,
      };
      
      if left_binding_power < min_binding_power 
      {
        break;
      }
      
      let op = Self::token_to_binary_op(&op_token.token_kind).unwrap();
      self.advance();
      
      let right = self.parse_expr(right_binding_power)?;
      
      left = Expr::Binary 
      {
        left: Box::new(left),
        op,
        right: Box::new(right),
      }
    }
    
    Ok(left)
  }

  fn parse_try_stmt(&mut self) -> ParseResult<Stmt> 
  {

    self.expect(TokenKind::Try)?;
    self.expect(TokenKind::LeftBrace)?;
    let try_body = self.parse_block()?;

    self.expect(TokenKind::On)?;
    self.expect(TokenKind::LeftBrace)?;
    let on_body = self.parse_block()?;
    
    Ok(Stmt::Try 
    { 
      try_body, 
      on_body 
    })
  }

  fn parse_pattern(&mut self) -> ParseResult<Pattern> 
  {
    let token = self.advance_or_eof("match pattern")?;
    match token.token_kind 
    {
      TokenKind::Underscore => Ok(Pattern::Wildcard),

      TokenKind::IntegerLiteral => 
      {
        let value = token.lexed_value.parse::<i64>().map_err(|_| ParseError::InvalidLiteral {
          kind: token.token_kind.clone(),
          span: token.span,
        })?;

        Ok(Pattern::Int(value))
      }

      TokenKind::FloatLiteral => 
      {
        let value = token.lexed_value.parse::<f64>().map_err(|_| ParseError::InvalidLiteral {
          kind: token.token_kind.clone(),
          span: token.span,
        })?;

        Ok(Pattern::Float(value))
      }

      TokenKind::BoolLiteral => 
      {
        let value = token.lexed_value.parse::<bool>().map_err(|_| ParseError::InvalidLiteral {
          kind: token.token_kind.clone(),
          span: token.span,
        })?;

        Ok(Pattern::Bool(value))
      }

      TokenKind::StringLiteral => Ok(Pattern::String(token.lexed_value)),

      _ => Err(ParseError::UnexpectedToken 
      {
        expected: vec![
          TokenKind::Underscore,
          TokenKind::IntegerLiteral,
          TokenKind::FloatLiteral,
          TokenKind::BoolLiteral,
          TokenKind::StringLiteral,
        ],

        found: token.token_kind,
        span: token.span,
      }),
    }
  }

  fn parse_match_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Match)?;
    let value = self.parse_expr(0)?;
    self.expect(TokenKind::LeftBrace)?;

    let mut arms: Vec<(Pattern, Vec<Stmt>)> = Vec::new();

    while self.peek_or_eof("'}' or pattern")?.token_kind != TokenKind::RightBrace 
    {
      let pattern = self.parse_pattern()?;
      self.expect(TokenKind::Arrow)?;
      self.expect(TokenKind::LeftBrace)?;

      let body = self.parse_block()?;
      arms.push((pattern, body));
    }

    self.expect(TokenKind::RightBrace)?;
    Ok(Stmt::Match 
    { 
      value, 
      arms
    })
  }
  
  fn parse_while_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::While)?;
    
    let condition;
    
    if self.peek_or_eof("'('")?.token_kind == TokenKind::LeftParen 
    {
      self.expect(TokenKind::LeftParen)?;
      condition = self.parse_expr(0)?;
      self.expect(TokenKind::RightParen)?;
      self.expect(TokenKind::LeftBrace)?;
    } else 
    {
      self.expect(TokenKind::LeftBrace)?;
      condition = Expr::Bool(true);
    }
    
    let body = self.parse_block()?;
    
    Ok(Stmt::While { condition, body })
  }
  
  fn finish_call(&mut self, callee: Expr) -> ParseResult<Expr> 
  {
    self.expect(TokenKind::LeftParen)?;
    let mut args = Vec::new();
    
    if self.peek_or_eof("')' or expression")?.token_kind != TokenKind::RightParen 
    {
      loop 
      {
        args.push(self.parse_expr(0)?);
        
        if self.peek_or_eof("',' or ')'")?.token_kind == TokenKind::Comma 
        {
          self.expect(TokenKind::Comma)?;
        } else 
        {
          break;
        }
      }
    }
    
    self.expect(TokenKind::RightParen)?;
    Ok(Expr::Call 
    {
      callee: Box::new(callee),
      args,
    })
  }
  
  pub fn parse_primary(&mut self) -> ParseResult<Expr> 
  {
    let token = self.advance_or_eof("expression")?;
    
    match &token.token_kind 
    {
      TokenKind::IntegerLiteral => 
      {
        let value: i64 =
        token
        .lexed_value
        .parse::<i64>()
        .map_err(|_| ParseError::InvalidLiteral 
        {
          kind: token.token_kind.clone(),
          span: token.span,
        })?;
        
        Ok(Expr::Integer(value))
      }
      
      TokenKind::FloatLiteral => 
      {
        let value: f64 =
        token
        .lexed_value
        .parse::<f64>()
        .map_err(|_| ParseError::InvalidLiteral 
        {
          kind: token.token_kind.clone(),
          span: token.span,
        })?;
        
        Ok(Expr::Float(value))
      }
      
      TokenKind::BoolLiteral => 
      {
        let value: bool =
        token
        .lexed_value
        .parse::<bool>()
        .map_err(|_| ParseError::InvalidLiteral 
        {
          kind: token.token_kind.clone(),
          span: token.span,
        })?;
        
        Ok(Expr::Bool(value))
      }
      
      TokenKind::StringLiteral => 
      {
        let value: String = token.lexed_value.parse::<String>().map_err(|_| 
        {
          ParseError::InvalidLiteral 
          {
            kind: token.token_kind.clone(),
            span: token.span,
          }
        })?;
        
        Ok(Expr::String(value))
      }
      
      TokenKind::Identifier => Ok(Expr::Identifier(token.lexed_value)),
      
      TokenKind::LeftParen => 
      {
        let expression = self.parse_expr(0)?;
        self.expect(TokenKind::RightParen)?;
        Ok(expression)
      }

      TokenKind::LeftBracket => 
      {
        let mut expressions: Vec<Expr> = Vec::new();

        while self.peek_or_eof("']'")?.token_kind != TokenKind::RightBracket
        {
          let expression = self.parse_expr(0)?;
          expressions.push(expression);

          if self.peek_or_eof("']'")?.token_kind == TokenKind::RightBracket
          {
            break;
          }

          self.expect(TokenKind::Comma)?;

          if self.peek_or_eof("'expression'")?.token_kind == TokenKind::RightBracket 
          {
            let token = self.peek_or_eof("'expression'")?;
            
            return Err(ParseError::UnexpectedToken 
            { 
              expected: vec![
                TokenKind::IntegerLiteral,
                TokenKind::FloatLiteral,
                TokenKind::BoolLiteral,
                TokenKind::StringLiteral,
                TokenKind::Identifier,
                TokenKind::LeftParen,
                TokenKind::Minus,
                TokenKind::Not,
                TokenKind::KeywordNot,
              ],
              found: token.token_kind.clone(), 
              span: token.span,
            });
          }
        }

        self.expect(TokenKind::RightBracket)?;

        Ok(Expr::ArrayLiteral(expressions))
      }
      
      TokenKind::Minus | TokenKind::KeywordNot | TokenKind::Not => 
      {
        let op = Self::token_to_unary_op(&token.token_kind).ok_or_else(|| 
        {
          ParseError::UnexpectedToken 
          {
            expected: vec![TokenKind::Minus, TokenKind::KeywordNot, TokenKind::Not],
            found: token.token_kind.clone(),
            span: token.span,
          }
        })?;
        
        let right_expr = self.parse_expr(15)?;
        Ok(Expr::Unary 
        {
          op,
          expr: Box::new(right_expr),
        })
      }
      
      _ => Err(ParseError::UnexpectedToken 
      {
        expected: vec!
        [
          TokenKind::IntegerLiteral,
          TokenKind::FloatLiteral,
          TokenKind::BoolLiteral,
          TokenKind::StringLiteral,
          TokenKind::Identifier,
          TokenKind::LeftParen,
          TokenKind::Minus,
          TokenKind::Not,
          TokenKind::KeywordNot,
        ],
        found: token.token_kind.clone(),
        span: token.span,
      }),
    }
  }

  fn parse_repeat_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Repeat)?;
    let count = self.parse_expr(0)?;

    let index_name = if self.peek_or_eof("'as'")?.token_kind == TokenKind::As 
    {
      self.expect(TokenKind::As)?;
      Some(self.expect(TokenKind::Identifier)?.lexed_value)
    } else 
    {
      None
    };

    self.expect(TokenKind::LeftBrace)?;
    let body = self.parse_block()?;

    Ok(Stmt::Repeat 
    { 
      count, 
      index_name, 
      body, 
    })
    
  }
  
  fn parse_let_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Let)?;
    let name = self.expect(TokenKind::Identifier)?.lexed_value;
    
    self.expect(TokenKind::Colon)?;
    
    let var_type = self.expect_any(&
    [
      TokenKind::IntType,
      TokenKind::FloatType,
      TokenKind::StringType,
      TokenKind::BoolType,
      TokenKind::ArrayType,
    ])?.lexed_value;
      
    let mut value: Option<Expr> = None;
    if self.peek_or_eof("'=' or ';'")?.token_kind == TokenKind::Assign 
    {
      self.expect(TokenKind::Assign)?;
      value = Some(self.parse_expr(0)?);
    }
      
    self.expect(TokenKind::Semicolon)?;
      
    Ok(Stmt::Let 
    {
      name,
      var_type: Some(var_type),
      value,
    })
  }
    
  pub fn produce_ast(&mut self) -> ParseResult<Vec<Stmt>> 
  {
    let mut require_entry = true;
    let mut include_allowed = true;
    let mut ast: Vec<Stmt> = Vec::new();
      
    while let Some(token) = self.peek() 
    {
      if token.token_kind == TokenKind::EndOfFile 
      {
        break;
      }

      match token.token_kind.clone() 
      {
        TokenKind::Entry => 
        {
          if !require_entry 
          {
            return Err(ParseError::EntryNotFirst 
            { 
              span: token.span,
            });
          }

          let stmt = self.parse_entry_stmt()?;

          ast.push(stmt);
          require_entry = false;
        },

        TokenKind::Include => 
        {
          if require_entry 
          {
            return Err(ParseError::MissingEntry 
            { 
              span: token.span,
            });
          }

          if !include_allowed 
          {
            return Err(ParseError::IncludeNotInHeader 
            { 
              span: token.span,
            })
          }

          let stmt = self.parse_include_stmt()?;
          ast.push(stmt);
        },

        _ => 
        {
          if require_entry 
          {
            return Err(ParseError::MissingEntry 
            { 
              span: token.span,
            });
          }

          include_allowed = false;
          
          let stmt = self.parse_stmt()?;
          ast.push(stmt);
        }
      }  
    }
      
    if require_entry 
    {
      return Err(ParseError::MissingEntry 
      { 
        span: self.eof_span(),
      });
    }

    Ok(ast)
  }
    
  fn parse_print_stmt(&mut self) -> ParseResult<Stmt> 
  {
    let new_line = self.advance_or_eof("print or println")?.token_kind == TokenKind::Println;
    self.expect(TokenKind::LeftParen)?;
      
    let mut text_string: Option<String> = None;
    let mut args: Vec<Expr> = Vec::new();
      
    match self.peek_or_eof("string literal, expression, or ')'")?.token_kind
    {
      TokenKind::RightParen => {}
      TokenKind::StringLiteral => 
      {
        text_string = Some(self.expect(TokenKind::StringLiteral)?.lexed_value);
          
        if self.peek_or_eof("',' or ')'")?.token_kind == TokenKind::Comma 
        {
          self.expect(TokenKind::Comma)?;
            
          loop 
          {
            args.push(self.parse_expr(0)?);
              
            match self.peek_or_eof("',' or ')'")?.token_kind 
            {
              TokenKind::Comma => 
              {
                self.expect(TokenKind::Comma)?;
              }

              TokenKind::RightParen => 
              {
                break;
              }

              _ => 
              {
                return Err(ParseError::UnexpectedToken 
                {
                  expected: vec![TokenKind::Comma, TokenKind::RightParen],
                  found: self.peek_or_eof("',' or ')'")?.token_kind.clone(),
                  span: self.peek_or_eof("',' or ')'")?.span,
                });
              }
            }
          }
        }
      }
        
      _ => 
      {
        args.push(self.parse_expr(0)?);
        let next = self.peek_or_eof("')'")?;
          
        if next.token_kind != TokenKind::RightParen 
        {
          return Err(ParseError::UnexpectedToken 
          {
            expected: vec![TokenKind::RightParen],
            found: next.token_kind.clone(),
            span: next.span,
          });
        }
      }
    }
      
    self.expect(TokenKind::RightParen)?;
    self.expect(TokenKind::Semicolon)?;
      
    Ok(Stmt::Print 
    {
      text_string,
      args,
      new_line,
    })
  }
    
  fn parse_function_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Function)?;
      
    let name = self.expect(TokenKind::Identifier)?.lexed_value;
      
    let mut params: Vec<(String, String)> = Vec::new();
      
    self.expect(TokenKind::LeftParen)?;
      
    while self.peek_or_eof(")")?.token_kind != TokenKind::RightParen 
    {
      if !params.is_empty() 
      {
        self.expect(TokenKind::Comma)?;
      }
        
      let param_name = self.expect(TokenKind::Identifier)?.lexed_value;
        
      self.expect(TokenKind::Colon)?;
        
      let param_type = self.expect_any(&
      [
        TokenKind::IntType,
        TokenKind::FloatType,
        TokenKind::StringType,
        TokenKind::BoolType,
        TokenKind::VoidType,
        TokenKind::ArrayType,
      ])?.lexed_value;
          
      params.push((param_name, param_type));
    }
        
    self.expect(TokenKind::RightParen)?;
    self.expect(TokenKind::Colon)?;
        
    let return_type = self.expect_any(&
    [
      TokenKind::IntType,
      TokenKind::FloatType,
      TokenKind::StringType,
      TokenKind::BoolType,
      TokenKind::VoidType,
      TokenKind::ArrayType,
    ])?.lexed_value;
          
    self.expect(TokenKind::LeftBrace)?;
          
    let body = self.parse_block()?;
          
    Ok(Stmt::Function 
    {
      name,
      params,
      return_type: Some(return_type),
      body: body,
    })
  }
        
  fn parse_stmt(&mut self) -> ParseResult<Stmt> 
  {
    let token = self.peek_or_eof("statement")?;
          
    match token.token_kind 
    {
      TokenKind::Let => self.parse_let_stmt(),
      TokenKind::Print | TokenKind::Println => self.parse_print_stmt(),
      TokenKind::Function => self.parse_function_stmt(),
      TokenKind::If => self.parse_if_stmt(),
      TokenKind::While => self.parse_while_stmt(),
      TokenKind::Return => self.parse_return_stmt(),
      TokenKind::Repeat => self.parse_repeat_stmt(),
      TokenKind::Continue => self.parse_continue_stmt(),
      TokenKind::Break => self.parse_break_stmt(),
      TokenKind::Match => self.parse_match_stmt(),
      TokenKind::Try => self.parse_try_stmt(),
      
      _ => 
      {
        let expr = self.parse_expr(0)?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::ExprStmt(expr))
      }
    }
  }

  fn parse_entry_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Entry)?;
    let name = self.expect(TokenKind::StringLiteral)?.lexed_value;
    self.expect(TokenKind::Semicolon)?;

    Ok(Stmt::Entry 
    {
      name,
    })
  }

  fn parse_include_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Include)?;
    let path = self.expect(TokenKind::StringLiteral)?.lexed_value;
    self.expect(TokenKind::Semicolon)?;

    Ok(Stmt::Include
    {
      path,
    })
  }
        
  fn parse_block(&mut self) -> ParseResult<Vec<Stmt>> 
  {
    let mut statements = Vec::new();
          
    while self.peek_or_eof("}")?.token_kind != TokenKind::RightBrace 
    {
      let stmt = self.parse_stmt()?;
      statements.push(stmt);
    }
          
    self.expect(TokenKind::RightBrace)?;
    Ok(statements)
  }
        
  fn parse_return_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::Return)?;
          
    match self.peek_or_eof("return value or ';'")?.token_kind 
    {
      TokenKind::Semicolon => 
      {
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Return { value: None })
      }
            
      _ => 
      {
        let value = self.parse_expr(0)?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Return { value: Some(value) })
      }
    }
  }
        
  fn parse_if_stmt(&mut self) -> ParseResult<Stmt> 
  {
    self.expect(TokenKind::If)?;
    self.expect(TokenKind::LeftParen)?;
          
    let condition = self.parse_expr(0)?;
          
    self.expect(TokenKind::RightParen)?;
    self.expect(TokenKind::LeftBrace)?;
          
    let then_body = self.parse_block()?;
          
    let else_exists = match self.peek() 
    {
      Some(token) => token.token_kind == TokenKind::Else,
      None => false,
    };
          
    let else_body = if else_exists 
    {
      self.expect(TokenKind::Else)?;
            
      if self.peek_or_eof("if or '{'")?.token_kind == TokenKind::If 
      {
        Some(vec![self.parse_if_stmt()?])
      } else 
      {
        self.expect(TokenKind::LeftBrace)?;
        Some(self.parse_block()?)
      }
      } else 
      {
        None
      };
          
    Ok(Stmt::If 
    {
      condition,
      then_body,
      else_body,
    })
  }
}
      