use super::lexer::{Token, TokenKind, Lexer};
use super::ast::{Expr, BinaryOperator, UnaryOperator};

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

      _ => {
        panic!("Unexpected token {:?} at line {}, col {}", token.token_kind, token.line, token.col);
      }
    }
  }

}

