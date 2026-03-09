use std::clone;
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
    expected: String,
    found: String, 
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

  #[error("The value {value} needs to be a number withouth a fractional part, and it needs to be positive to be usable in a loop")]  
  NonValidIntegerCount
  {
    value: f64,
  },
}

type RuntimeError<T> = Result<T, InterpreterError>;

enum ControlFlow 
{
  None,
  Break,
  Continue,
  Return(Value),
}

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
        Ok(self.eval_unary(op, value)?)
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
        Ok(self.eval_expr(expr)?)
      },

      Expr::Member { object, property } => 
      { 
        Ok(self.eval_expr(expr)?)
      },

      Expr::Index { object, index } => 
      { 
        Ok(self.eval_expr(expr)?)
      },
    }
  }

  fn eval_unary(&self, op: &UnaryOperator, value: Value) -> RuntimeError<Value>
  {
    match op 
    {
      UnaryOperator::Negative => 
      {
        match value 
        {
          Value::Number(n) => Ok(Value::Number(-n)),
          other => Err(InterpreterError::TypeMismatch 
          { 
            expected: "Number".to_string(), 
            found: other.type_name().to_string(), 
          })
        }
      },

      UnaryOperator::Not | UnaryOperator::KeywordNot => 
      {
          Ok(Value::Bool(!value.is_truthy()))
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
              expected: "integer".to_string(), 
              found: "float".to_string(), 
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
              expected: left.type_name().to_string(), 
              found:  right.type_name().to_string(),
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
              expected: "Number".to_string(), 
              found: right.type_name().to_string(), 
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
              expected: "Number".to_string(), 
              found: right.type_name().to_string(), 
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
              expected: "Number".to_string(), 
              found: right.type_name().to_string(), 
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
              expected: "Number".to_string(), 
              found: right.type_name().to_string(), 
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
              expected: "Number".to_string(), 
              found: right.type_name().to_string(), 
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
              expected: "Number".to_string(), 
              found: right.type_name().to_string(), 
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
                expected: "Number".to_string(), 
                found: right.type_name().to_string(), 
              });
            }

            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number".to_string(), 
              found: left.type_name().to_string(), 
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

  fn exec_stmt(&mut self, stmt: &Stmt) -> RuntimeError<ControlFlow> 
  {
    match stmt 
    {
      Stmt::Let { name, var_type, value } => 
      {
        let evaluated = if let Some(value) = value 
        {
          self.eval_expr(value)?
        } else 
        {
          Value::Void
        };

        if let Some(declared_type) = var_type 
        {
          let actual_type = evaluated.type_name();

          if declared_type != actual_type 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: declared_type.to_string(), 
              found: actual_type.to_string(), 
            });
          }
        }

        self.set_var(name.clone(), evaluated);

        Ok(ControlFlow::None)
      },

      Stmt::ExprStmt(expr) => 
      {
        self.eval_expr(expr)?;
        Ok(ControlFlow::None)
      },

      Stmt::Return { value } => 
      {
        let value = if let Some(value) = value 
        {
          self.eval_expr(value)?
        } else 
        {
          Value::Void
        };

        Ok(ControlFlow::Return(value))
      },

      Stmt::Break => 
      {
        Ok(ControlFlow::Break)
      },

      Stmt::Continue => 
      {
        Ok(ControlFlow::Continue)
      },

      Stmt::If { condition, then_body, else_body } => 
      {
        let condition = self.eval_expr(condition)?.is_truthy();

        Ok(if condition
        {
          self.exec_block(then_body)?
        } else if let Some(else_body) = else_body
        {
          self.exec_block(else_body)?
        } else 
        {
          ControlFlow::None
        })
      },

      Stmt::While { condition, body } => 
      {
        while self.eval_expr(condition)?.is_truthy()
        {
          let flow = self.exec_block(body)?;
          match flow 
          {
            ControlFlow::Break => break,
            ControlFlow::Continue => continue,
            ControlFlow::Return(value) => {return Ok(ControlFlow::Return(value))},
            ControlFlow::None => continue,
          }
        }

        Ok(ControlFlow::None)
      },

      Stmt::Repeat { count, index_name, body } => 
      {
        let count = self.eval_expr(count)?;

        match count 
        {
          Value::Number(value) => 
          {
            if value.fract() != 0.0 || value < 0.0
            {
              return Err(InterpreterError::NonValidIntegerCount 
              { 
                value 
              })
            }

            let repeat_count = value as usize;

            for index in 0..repeat_count 
            {
              if let Some(name) = index_name 
              {
                self.set_var(name.clone(), Value::Number(repeat_count as f64));
              }

              let flow = self.exec_block(body)?;
              match flow 
              {
                ControlFlow::Break => break,
                ControlFlow::Continue => continue,
                ControlFlow::Return(value) => {return Ok(ControlFlow::Return(value))},
                ControlFlow::None => continue,
              }
            }
          }

          _ => 
          {
            return Err(InterpreterError::TypeMismatch 
            { 
              expected: "Number".to_string(), 
              found: count.type_name().to_string(),
            });
          }
        }

        Ok(ControlFlow::None)
      },

      Stmt::Print { text_string, args, new_line } => 
      {
        Ok(ControlFlow::None)
      },

      Stmt::Function { name, params, return_type, body } => 
      {
        Ok(ControlFlow::None)
      },

      Stmt::Match { value, arms } => 
      {
        Ok(ControlFlow::None)
      },

      Stmt::Entry { name } => 
      {
        Ok(ControlFlow::None)
      },

      Stmt::Include { path } => 
      {
        Ok(ControlFlow::None)
      },

      Stmt::Try { try_body, on_body } => 
      {
        Ok(ControlFlow::None)
      },
    }
  }

  fn exec_block(&mut self, body: &[Stmt]) -> RuntimeError<ControlFlow> 
  {
    for stmt in body 
    {
      let result = self.exec_stmt(stmt)?;
      match result 
      {
        ControlFlow::None => continue,
        ControlFlow::Break  => return Ok(ControlFlow::Break),
        ControlFlow::Continue => return Ok(ControlFlow::Continue),
        ControlFlow::Return(Value) => return Ok(ControlFlow::Return(Value)),
      }  
    }

    Ok(ControlFlow::None)
  }

  fn set_var(&mut self, name: String, value: Value) 
  {
    if let Some(frame) = self.call_stack.last_mut() 
    {
      frame.locals.insert(name, value);
    } else 
    {
      self.global.insert(name, value);
    }
  }
}