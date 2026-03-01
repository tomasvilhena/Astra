use std::collections::HashMap;
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

  #[error("Invalid binary operation `{op}` for {left} and {right}")]
  InvalidBinaryOp 
  {
    op: &'static str,
    left: &'static str,
    right: &'static str,
  }
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
                expected: "number", 
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
        let right = self.eval_expr(right)?;
        match op 
        {
          BinaryOperator::Plus 
          | BinaryOperator::Minus 
          | BinaryOperator::Star
          | BinaryOperator::Slash
          | BinaryOperator::Caret
          | BinaryOperator::Percent => 
          {

          },
        }

        return a;
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
      } 
      _ => 
      {
        Err(InterpreterError::InvalidBinaryOp 
        { 
          op: (), 
          left: (), 
          right: () 
        })
      }
    }
  }
}