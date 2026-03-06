use std::collections::HashMap;
use std::f32::consts::E;
use std::fmt::format;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::frontend::ast::{Expr, Stmt, BinaryOperator, UnaryOperator, Pattern};
use crate::frontend::lexer::TokenKind;
use crate::runtime::value::{self, Value};


#[derive(Error, Diagnostic, Debug)]
pub enum InterpreterError 
{
  #[error("Undefined variable `{name}`")]
  UndefinedVariable { name: String },

  #[error("Type mismatch: expected {expected}, found {found}")]
  TypeMismatch 
  { 
    expected: &'static str,
    found: &'static str, 
  },

  #[error("Cant divide {value} by 0, as that would be a mathematical error")]
  DivideBy0
  { 
    value: f64,
  },

  #[error("Cant Multiply {text} by a negative number, or 0, as that would result in an unknown string size")]
  MultiplyStringByInvalidValue
  { 
    text: String,
  },

  #[error("Invalid binary operation `{op}` for {left} and {right}")]
  InvalidBinaryOp 
  {
    op: &'static str,
    left: &'static str,
    right: &'static str,
  },

  #[error("The operation {op} is not a Valid operation between a value of type {left} and a value of type {right}")]  
  InvalidTypeComparisson
  {
    op: &'static str,
    left: &'static str,
    right: &'static str,
  },

  #[error("This character is not a Valid binary operator")]  
  UnsupportedBinaryOp 
  {
    op: &'static str,
  },

  #[error("The range provided would result in a invalid range, as {left} is smaller or equal to {right}, resulting in a range of 0 or smaller")]  
  InvalidRange 
  {
    left: f64,
    right: f64,
  },

  #[error("The values of {left} and {right} must both be integers")]  
  NonIntegerRange 
  {
    left: f64,
    right: f64,
  },
}

type RuntimeError<T> = Result<T, InterpreterError>;

pub struct Interpreter 
{
  global: HashMap<String, Value>,
  functions: HashMap<String, FunctionDef>,
  call_stack: Vec<CallFrame>
}

#[derive(Debug, Clone)]
struct FunctionDef 
{
  params: Vec<String>,
  body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
struct CallFrame 
{
  locals: HashMap<String, Value>,
}

impl Interpreter 
{
  pub fn new() -> Self 
  {
    Self 
    { 
      global: HashMap::new(),
      functions: HashMap::new(),
      call_stack: vec![],
    }
  }

  fn binary_op_to_str(op: &BinaryOperator) -> &'static str 
  {
    match op 
    {
      BinaryOperator::Plus => "+",
      BinaryOperator::Minus => "-",
      BinaryOperator::Star => "*",
      BinaryOperator::Slash => "/",
      BinaryOperator::Percent => "%",
      BinaryOperator::Caret => "^",
      BinaryOperator::Equal => "==",
      BinaryOperator::NotEqual => "!=",
      BinaryOperator::Greater => ">",
      BinaryOperator::GreaterEqual => ">=",
      BinaryOperator::Less => "<",
      BinaryOperator::LessEqual => "<=",
      BinaryOperator::And => "&&",
      BinaryOperator::Or => "||",
      BinaryOperator::KeywordAnd => "AND",
      BinaryOperator::KeywordOr => "OR",
      BinaryOperator::Assign => "=",
      BinaryOperator::PlusEqual => "+=",
      BinaryOperator::MinusEqual => "-=",
      BinaryOperator::StarEqual => "*=",
      BinaryOperator::SlashEqual => "/=",
      BinaryOperator::PercentEqual => "%=",
      BinaryOperator::CaretEqual => "^=",
      BinaryOperator::RangeExclusive => "..",
      BinaryOperator::RangeInclusive => "..=",
      BinaryOperator::Not => "!",
      BinaryOperator::KeywordNot => "NOT",
    }
  }

  pub fn eval_expr(&mut self, expr: &Expr) -> RuntimeError<Value> 
  {
    match expr 
    {
      Expr::Number(value) => Ok(Value::Number(*value)),
      Expr::Bool(value) => Ok(Value::Bool(*value)),
      Expr::String(value) => Ok(Value::String(value.clone())),
      Expr::Identifier(name) => 
      {
        if let Some(frame) = self.call_stack.last() 
        {
          if let Some(value) = frame.locals.get(name) 
          {
            return Ok(value.clone());
          }
        }

        self.global.get(name).cloned().ok_or(InterpreterError::UndefinedVariable { name: name.clone() })
      }, 

      Expr::ArrayLiteral(items) => 
      { 
        let mut values = Vec::new();
        for item in items 
        {
          values.push(self.eval_expr(item)?);
        }

        Ok(Value::Array(values))
      },

      Expr::Unary { op, expr } => 
      {
        let value = self.eval_expr(expr)?;
        match op 
        {
          UnaryOperator::Negative => 
          {
            match value 
            {
              Value::Number(n) => Ok(Value::Number(-n)),
              other => Err(InterpreterError::TypeMismatch 
              { 
                expected: "Number", 
                found: other.type_name(), 
              })
            }
          }, 
          UnaryOperator::Not | UnaryOperator::KeywordNot => 
          {
            Ok(Value::Bool(!value.is_truthy()))
          },
        }
      },

      Expr::Binary { left, op, right } => 
      {
        let left = self.eval_expr(left)?;


        if matches!(op, &BinaryOperator::And) 
          | matches!(op, &BinaryOperator::KeywordAnd)
        {
          if !left.is_truthy() 
          {
            return Ok(Value::Bool(false));
          }
        }

        if matches!(op, &BinaryOperator::Or)
          | matches!(op, &BinaryOperator::KeywordOr) 
        {
          if left.is_truthy() 
          {
            return Ok(Value::Bool(true));
          }
        }

        let right = self.eval_expr(right)?;
        Ok(self.eval_binary(op, left, right)?)
      },

      Expr::Call { callee, args } => 
      { 

      },

      Expr::Member { object, property } => 
      { 

      },

      Expr::Index { object, index } => 
      { 

      },
    }
  }

  fn eval_binary(&self, op: &BinaryOperator, left: Value, right: Value) -> RuntimeError<Value>
  {
    match op 
    {
      BinaryOperator::Plus => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) =>
          {
            return Ok(Value::Number(left + right));
          },

          (Value::String(left), Value::String(right)) =>
          {
            return Ok(Value::String(format!("{}{}", left, right)));
          },

          (left, right) => 
          {
            return Err(InterpreterError::InvalidBinaryOp 
            { 
              op: "+", 
              left: left.type_name(), 
              right: right.type_name(),
            });
          }
        }
      },

      BinaryOperator::Minus => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) =>
          {
            return Ok(Value::Number(left - right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::InvalidBinaryOp 
            { 
              op: "-", 
              left: left.type_name(), 
              right: right.type_name(),
            });
          }
        }
      }, 

      BinaryOperator::Star => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) =>
          {
            
            return Ok(Value::Number(left * right));
          },

          (Value::String(left), Value::Number(right)) =>
          {
            if right <= 0.0 
            {
              return Err(InterpreterError::MultiplyStringByInvalidValue 
              { 
                text: left, 
              })
            }

            if right.fract() == 0.0 
            {
              return Ok(Value::String(left.repeat(right as usize)));
            }

            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "integer", 
              found: "float", 
            });
          },

          (left, right) => 
          {
            return Err(InterpreterError::InvalidBinaryOp 
            { 
              op: "*", 
              left: left.type_name(), 
              right: right.type_name(),
            });
          }
        }
      },

      BinaryOperator::Slash => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) =>
          {
            if right == 0.0 
            {
              return Err(InterpreterError::DivideBy0 
              { 
                value: left, 
              });
            }

            return Ok(Value::Number(left / right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::InvalidBinaryOp 
            { 
              op: "/", 
              left: left.type_name(), 
              right: right.type_name(),
            });
          }
        }
      },

      BinaryOperator::Caret => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) =>
          {
            return Ok(Value::Number(left.powf(right)));
          },

          (left, right) => 
          {
            return Err(InterpreterError::InvalidBinaryOp 
            { 
              op: "^", 
              left: left.type_name(), 
              right: right.type_name(),
            });
          }
        }
      },

      BinaryOperator::Percent => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) =>
          {
            if right == 0.0 
            {
              return Err(InterpreterError::DivideBy0 
              { 
                value: left, 
              });
            }

            return Ok(Value::Number(left % right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::InvalidBinaryOp 
            { 
              op: "%", 
              left: left.type_name(), 
              right: right.type_name(),
            });
          }
        }
      },

      BinaryOperator::Equal  
      | BinaryOperator::NotEqual => 
      {
        let is_equal = left == right;
        let result = if matches!(op, &BinaryOperator::Equal) 
        {
          is_equal
        } else 
        {
          !is_equal
        };

        Ok(Value::Bool(result))
      },

      BinaryOperator::Less 
      | BinaryOperator::LessEqual
      | BinaryOperator::Greater 
      | BinaryOperator::GreaterEqual => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            let result = match op 
            {
              &BinaryOperator::Less => left < right, 
              &BinaryOperator::LessEqual => left <= right,
              &BinaryOperator::Greater => left > right, 
              &BinaryOperator::GreaterEqual => left >= right,
              _ => unreachable!(),
            };

            return Ok(Value::Bool(result));
          },

          (left, right) => 
          {
            return Err(InterpreterError::InvalidTypeComparisson 
            { 
              op: Self::binary_op_to_str(op), 
              left: left.type_name(), 
              right: right.type_name(), 
            })
          }
        } 
      },

      BinaryOperator::Assign => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            return Ok(Value::Number(right));
          },

          (Value::Bool(left), Value::Bool(right)) => 
          {
            return Ok(Value::Bool(right));
          },

          (Value::String(left), Value::String(right)) => 
          {
            return Ok(Value::String(right));
          },

          (Value::Array(left), Value::Array(right)) => 
          {
            return Ok(Value::Array(right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: left.type_name(), 
              found:  right.type_name(),
            });
          }
        }
      },

      BinaryOperator::MinusEqual => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            return Ok(Value::Number(left - right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number", 
              found: right.type_name(), 
            });
          },
        }
      },

      BinaryOperator::PlusEqual => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            return Ok(Value::Number(left + right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number", 
              found: right.type_name(), 
            });
          },
        }
      }, 

      BinaryOperator::SlashEqual => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            if right == 0.0 
            {
              return Err(InterpreterError::DivideBy0 
              { 
                value: left, 
              })
            }

            return Ok(Value::Number(left / right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number", 
              found: right.type_name(), 
            });
          },
        }
      },
      
      BinaryOperator::StarEqual => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            return Ok(Value::Number(left * right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number", 
              found: right.type_name(), 
            });
          },
        }
      },
      
      BinaryOperator::PercentEqual => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            return Ok(Value::Number(left % right));
          },

          (left, right) => 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number", 
              found: right.type_name(), 
            });
          },
        }
      },

      BinaryOperator::CaretEqual => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            return Ok(Value::Number(left.powf(right)));
          },

          (left, right) => 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number", 
              found: right.type_name(), 
            });
          },
        }
      },

      BinaryOperator::RangeExclusive 
      | BinaryOperator::RangeInclusive => 
      {
        match (left, right) 
        {
          (Value::Number(left), Value::Number(right)) => 
          {
            if left.fract() != 0.0 || right.fract() != 0.0 
            {
              return Err(InterpreterError::NonIntegerRange 
              { 
                left, 
                right 
              });
            }

            if left > right 
            {
              return Err(InterpreterError::InvalidRange 
              { 
                left,
                right,
              });
            }

            let inclusive = matches!(op, &BinaryOperator::RangeInclusive);
            let mut range = Vec::new();
            
            for value in left as i32..=right as i32 
            {
              if value == right as i32 && !inclusive 
              {
                break;
              }

              range.push(Value::Number(value as f64));
            }  

            return Ok(Value::Array(range));
          }

          (left, right) => 
          {
            if matches!(left, Value::Number(_))
            {
              return Err(InterpreterError::TypeMismatch 
              { 
                expected: "Number", 
                found: right.type_name(), 
              });
            }

            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number", 
              found: left.type_name(), 
            });
          }
        }
      },
      
      _ => 
      {
        Err(InterpreterError::UnsupportedBinaryOp
        { 
          op: Self::binary_op_to_str(op), 
        })
      }
    }
  }
}