use std::collections::HashMap;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::frontend::ast::{Expr, Stmt, BinaryOperator, UnaryOperator, Pattern};
use crate::runtime::value::Value;


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
      Expr::Identifier(name) => { }
      Expr::ArrayLiteral(items) => { }

      Expr::Unary { op, expr } => { }
      Expr::Binary { left, op, right } => { }

      Expr::Call { callee, args } => { }
      Expr::Member { object, property } => { }
      Expr::Index { object, index } => { }
    }
  }
}