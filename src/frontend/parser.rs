use core::panic;

use super::lexer::{Token, TokenKind, Lexer};
use super::ast::{Expr, BinaryOperator, UnaryOperator, Stmt};

pub struct Parser
{
  tokens: Vec<Token>,
  position: usize,
}

impl Parser
{

  fn token_to_binary_op(token: &TokenKind) -> Option<BinaryOperator>
  {
    match token {
      // Assignment & arithmetic assignment
      TokenKind::Assign         	=> Some(BinaryOperator::Assign),
      TokenKind::Plus          		=> Some(BinaryOperator::Plus),
      TokenKind::Minus         		=> Some(BinaryOperator::Minus),
      TokenKind::Star          		=> Some(BinaryOperator::Star),
      TokenKind::Slash         		=> Some(BinaryOperator::Slash),
      TokenKind::Percent       		=> Some(BinaryOperator::Percent),
      TokenKind::Caret         		=> Some(BinaryOperator::Caret),
      TokenKind::PlusEqual     		=> Some(BinaryOperator::PlusEqual),
      TokenKind::MinusEqual    		=> Some(BinaryOperator::MinusEqual),
      TokenKind::StarEqual     		=> Some(BinaryOperator::StarEqual),
      TokenKind::SlashEqual    		=> Some(BinaryOperator::SlashEqual),
      TokenKind::PercentEqual  		=> Some(BinaryOperator::PercentEqual),
      TokenKind::CaretEqual    		=> Some(BinaryOperator::CaretEqual),

      // Comparisons
      TokenKind::Equal    				=> Some(BinaryOperator::Equal),
      TokenKind::NotEqual     		=> Some(BinaryOperator::NotEqual),
      TokenKind::Greater       		=> Some(BinaryOperator::Greater),
      TokenKind::GreaterEqual  		=> Some(BinaryOperator::GreaterEqual),
      TokenKind::Less          		=> Some(BinaryOperator::Less),
      TokenKind::LessEqual     		=> Some(BinaryOperator::LessEqual),

      // Logical
      TokenKind::And              => Some(BinaryOperator::And),
      TokenKind::Or               => Some(BinaryOperator::Or),
      TokenKind::KeywordAnd       => Some(BinaryOperator::KeywordAnd),
      TokenKind::KeywordOr        => Some(BinaryOperator::KeywordOr),

      // Ranges
      TokenKind::RangeExclusive   => Some(BinaryOperator::RangeExclusive),
      TokenKind::RangeInclusive   => Some(BinaryOperator::RangeInclusive),

      _ => None,
    }
  }

  fn token_to_unary_op(token: &TokenKind) -> Option<UnaryOperator>
  {
    match token {
      TokenKind::Minus      	=> Some(UnaryOperator::Negative),
      TokenKind::Not       		=> Some(UnaryOperator::Not),
      TokenKind::KeywordNot 	=> Some(UnaryOperator::KeywordNot),
      _ => None,
    }
  }

  pub fn new(tokens: Vec<Token>) -> Self
  {
    Self {tokens, position: 0}
  }

  pub fn peek(&self) -> Option<&Token>
  {
    self.tokens.get(self.position)
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

  pub fn is_end_of_file(&self) -> bool
  {
    match self.peek()
    {
      Some(token) => token.token_kind == TokenKind::EndOfFile,
      None => true,
    }
  }

  pub fn get_binding_power(op: &TokenKind) -> Option<(u8, u8)>
  {
    match op {
      // Assignment (right-associative)
      TokenKind::Assign
      | TokenKind::PlusEqual
      | TokenKind::MinusEqual
      | TokenKind::StarEqual
      | TokenKind::SlashEqual
      | TokenKind::PercentEqual
      | TokenKind::CaretEqual => Some((1, 0)),

      // Logical OR
      TokenKind::Or
      | TokenKind::KeywordOr => Some((2, 3)),

      // Logical AND
      TokenKind::And
      | TokenKind::KeywordAnd => Some((4, 5)),

      // Equality
      TokenKind::Equal
      | TokenKind::NotEqual => Some((6, 7)),

      // Comparisons
      TokenKind::Greater
      | TokenKind::GreaterEqual
      | TokenKind::Less
      | TokenKind::LessEqual => Some((8, 9)),

      // Addition & Subtraction
      TokenKind::Plus
      | TokenKind::Minus => Some((10, 11)),

      // Multiplication, Division, Modulo
      TokenKind::Star
      | TokenKind::Slash
      | TokenKind::Percent => Some((12, 13)),

      // Exponentiation (right-associative)
      TokenKind::Caret => Some((14, 13)),

      // Ranges
      TokenKind::RangeExclusive
      | TokenKind::RangeInclusive => Some((15, 16)),

      _ => None,
    }
  }


  pub fn parse_expr(&mut self, min_binding_power: u8) -> Option<Expr>
  {
    let mut left = self.parse_primary()?;

    loop
    {
      let op_token = match self.peek()
      {
        Some(tok) => tok,
        None => break,
      };

      let (left_binding_power, right_binding_power) = match Self::get_binding_power(&op_token.token_kind)
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

      left = Expr::Binary {
        left: Box::new(left),
        op,
        right: Box::new(right),
      }

    }

    Some(left)
  }

  pub fn parse_primary(&mut self) -> Option<Expr>
  {
    let token = self.advance()?;

    match &token.token_kind
    {
      TokenKind::IntegerLiteral => {
        let value: i64 = token.lexed_value.parse::<i64>().expect("Invalid integer literal");
        Some(Expr::Integer(value))
      }

      TokenKind::FloatLiteral => {
        let value: f64 = token.lexed_value.parse::<f64>().expect("Invalid Float literal");
        Some(Expr::Float(value))
      }

      TokenKind::BoolLiteral => {
        let value: bool = token.lexed_value.parse::<bool>().expect("Invalid Bool Literal");
        Some(Expr::Bool(value))
      }

      TokenKind::StringLiteral => {
        let value: String = token.lexed_value.parse::<String>().expect("Invalid String Literal");
        Some(Expr::String(value))
      }

      TokenKind::Identifier => {
        Some(Expr::Identifier(token.lexed_value))
      }

      TokenKind::LeftParen =>
      {
        let expression = self.parse_expr(0)?;
        match self.advance()?.token_kind {
          TokenKind::RightParen => Some(expression),
          _ => panic!("Expected closing paretheses"),
        }
      }

      TokenKind::Minus | TokenKind::KeywordNot | TokenKind::Not =>
      {
        let op = Self::token_to_unary_op(&token.token_kind).expect("invalid unary operator");

        let right_expr = self.parse_expr(15)?;
        Some(Expr::Unary {
          op,
          expr: Box::new(right_expr),
        })
      }

      _ => {
        panic!("Unexpected token {:?} at line {}, col {}", token.token_kind, token.line, token.col);
      }
    }
  }

  fn parse_let_stmt(&mut self) -> Option<Stmt>
  {
    self.advance();

    let identifier_token = self.peek()?;

    let name = match identifier_token.token_kind {
      TokenKind::Identifier => identifier_token.lexed_value.to_string(),
      _ => panic!("Invalide variable identifier after 'let'"),
    };

    self.advance();

    let colon_token = self.peek()?;
    if colon_token.token_kind != TokenKind::Colon
    {
      panic!("Invalid Variable type declaration");
    }

    self.advance();

    let token_type = self.peek()?;
    let var_type = match token_type.token_kind
    {
      TokenKind::IntType
        | TokenKind::FloatType
        | TokenKind::StringType
        | TokenKind::BoolType
        | TokenKind::VoidType
        | TokenKind::ArrayType => token_type.lexed_value.clone(),
      _ => panic!("Expected a valid type after ':' (int, float, bool, string, etc.), got {:?}", token_type.token_kind),
    };

    self.advance();

    let mut value: Option<Expr> = None;
    if let Some(next) = self.peek() 
    {
      if next.token_kind == TokenKind::Assign 
      {
        self.advance();
        value = Some(self.parse_expr(0)?);
      }
    }

    let semi = self.peek()?;
    if semi.token_kind != TokenKind::Semicolon
    {
      panic!("Expected ';' after let statement");
    } 

    self.advance();

    Some(Stmt::Let 
    { 
      name,
      var_type: Some(var_type),
      value, 
    })

  }

  pub fn produce_ast(&mut self) -> Vec<Stmt>
  {
    let mut ast: Vec<Stmt> = Vec::new();

    while self.peek().is_some() && self.peek().unwrap().token_kind != TokenKind::EndOfFile 
    {
      if let Some(stmt) = self.parse_stmt()
      {
        ast.push(stmt);
      }
    }

    ast
  }


  fn parse_print_stmt(&mut self) -> Option<Stmt>
  {

    let new_line = self.peek()?.token_kind == TokenKind::Println;

    self.advance();

    if self.peek()?.token_kind != TokenKind::LeftParen 
    {
      panic!("Expected '(' after print statement");
    }

    self.advance();

    let mut text_string: Option<String> = None;
    let mut args: Vec<Expr> = Vec::new();

    if self.peek()?.token_kind == TokenKind::StringLiteral 
    {
      text_string = Some(self.advance()?.lexed_value.clone());
    }

    match self.peek()?.token_kind 
    {
      TokenKind::RightParen => {}, 

      TokenKind::Comma => 
      {
        self.advance();
        
        while self.peek()?.token_kind != TokenKind::RightParen 
        {
          args.push(self.parse_expr(0)?);

          if self.peek()?.token_kind == TokenKind::Comma 
          {
            self.advance();
          }
        }
      },

      _ => 
      {
        panic!("Expected ',' or ')' after print statement");
      }
    }

    if self.peek()?.token_kind != TokenKind::RightParen
    {
      panic!("Expected ')' after print statement");
    }
    
    self.advance();
    if self.peek()?.token_kind != TokenKind::Semicolon 
    {
      panic!("Expected ';' after print statement");
    }

    self.advance();

    Some(Stmt::Print 
    {
      text_string,
      args,
      new_line,
    })
  }

  fn parse_function_stmt(&mut self) -> Option<Stmt>
  {
    self.advance();

    let name: String = self.advance()?.lexed_value;

    let mut params: Vec<(String, String)> = Vec::new();

    if self.peek()?.token_kind != TokenKind::LeftParen
    {
      panic!("Expected '(' after function name"); 
    }

    self.advance();

    while self.peek()?.token_kind != TokenKind::RightParen 
    {
      if self.peek()?.token_kind != TokenKind::Identifier 
      {
        panic!("Expected identifier after '('");
      }

      let param_name = self.advance()?.lexed_value;

      if self.peek()?.token_kind != TokenKind::Colon 
      {
        panic!("Expected ':' after parameter name");
      }

      self.advance();

      let token_type = self.advance()?;
      let param_type = match token_type.token_kind
      {
        TokenKind::IntType
        | TokenKind::FloatType
        | TokenKind::StringType
        | TokenKind::BoolType
        | TokenKind::VoidType
        | TokenKind::ArrayType => token_type.lexed_value.clone(),
        _ => panic!("Expected a valid type after ':' (int, float, bool, string, etc.), got {:?}", token_type.token_kind),
      };

      params.push((param_name, param_type));
    }

    if self.peek()?.token_kind != TokenKind::RightParen
    {
      panic!("Expected ')' after parameters");
    }

    self.advance();

    if self.peek()?.token_kind != TokenKind::Colon 
    {
      panic!("Expected ':' to declare function return type");
    }

    self.advance();

    let return_type = self.advance()?;
    let return_type = match return_type.token_kind
    {
      TokenKind::IntType
      | TokenKind::FloatType
      | TokenKind::StringType
      | TokenKind::BoolType
      | TokenKind::VoidType
      | TokenKind::ArrayType => return_type.lexed_value.clone(),
      _ => panic!("Expected a valid type after ':' (int, float, bool, string, etc.), got {:?}", return_type.token_kind),
    };

    if self.peek()?.token_kind != TokenKind::LeftBrace 
    {
      panic!("Expected '{{' after function return type");
    }

    self.advance();

    //TODO: parse parameters, return type and body
    Some(Stmt::Function 
    { 
      name, 
      params, 
      return_type: Some(return_type), 
      body: vec![], 
    })
  }

  fn parse_stmt(&mut self) -> Option<Stmt>
  {
    let token = self.peek()?;

    match token.token_kind 
    {
      TokenKind::Let => self.parse_let_stmt(),
      TokenKind::Print | TokenKind::Println => self.parse_print_stmt(),
      _ =>
      {
        let expr = self.parse_expr(0)?;
        let semi = self.peek()?;

        if semi.token_kind != TokenKind::Semicolon
        {
          panic!("Expected ';' after statement");
        }

        self.advance();
        Some(Stmt::ExprStmt(expr))
      }
    }
  }

  fn parse_block(&mut self) -> Option<Vec<Stmt>> 
  {
    let mut statements = Vec::new();

    while self.peek()?.token_kind != TokenKind::RightBrace 
    {
      if let Some(stmt) = self.parse_stmt() 
      {
        statements.push(stmt);
      }
    } 

    self.advance();
    Some(statements)
  }

  fn parse_if_stmt(&mut self) -> Option<Stmt>
  {
    self.advance();

    if self.peek()?.token_kind != TokenKind::LeftParen 
    {
      panic!("Expected '(' after 'if'");
    }

    self.advance();

    let condition = self.parse_expr(0)?;

    if self.peek()?.token_kind != TokenKind::RightParen 
    {
      panic!("Expected ')' after if condition");
    }

    self.advance();

    if self.peek()?.token_kind != TokenKind::LeftBrace
    {
      panic!("Expected '{{' after if condition");
    }

    self.advance();

    let then_body = self.parse_block()?;

    let else_exists = match self.peek() 
    {
      Some(token) => token.token_kind == TokenKind::Else,
      None => false,
    };

    let else_body = if else_exists 
    {
      self.advance();

      if self.peek()?.token_kind == TokenKind::If 
      {
        Some(vec![self.parse_if_stmt()?])
      } else 
      {
        if self.peek()?.token_kind != TokenKind::LeftBrace
        {
        panic!("Expected '{{' after 'else'");
        }

        self.advance();
        Some(self.parse_block()?)
      }
      
    } else
    {
      None
    };

    Some(Stmt::If 
    { 
      condition,
      then_body, 
      else_body, 
    })

  }
}

