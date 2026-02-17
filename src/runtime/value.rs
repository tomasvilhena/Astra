use std::collections::HashMap;
use crate::frontend::ast::{Expr, Stmt, BinaryOperator, UnaryOperator, Pattern};

#[derive(Debug, Clone)]
pub enum Value 
{
  Number(f64),
  Bool(bool),
  String(String),
  Array(Vec<Value>),
  Void,
}

impl Value 
{
  pub fn type_name(&self) -> &'static str 
  {
    match self 
    {
      Value::Number(_) => "number",
      Value::Bool(_) => "bool",
      Value::String(_) => "string",
      Value::Array(_) => "array",
      Value::Void => "void",
    }
  }

  pub fn is_truthy(&self) -> bool 
  {

  }
}