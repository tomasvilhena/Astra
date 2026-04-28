#[derive(Debug, Clone,  PartialEq)]
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
    match self
    {
      Value::Bool(bool) => *bool,
      Value::Number(number) => *number != 0.0,
      Value::String(string) => !string.is_empty(),
      Value::Array(array) => !array.is_empty(),
      Value::Void => false,
    }
  }
}

impl std::fmt::Display for Value
{
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
  {
    match self
    {
      Value::Number(number) => write!(formatter, "{}", number),
      Value::Bool(boolean) => write!(formatter, "{}", boolean),
      Value::String(string) => write!(formatter, "{}", string),

      Value::Array(items) =>
      {
        write!(formatter, "[")?;
        for (i, item) in items.iter().enumerate()
        {
          if i > 0 { write!(formatter, ", ")?; }
          match item
          {
            Value::String(s) => write!(formatter, "\"{s}\"")?,
            other             => write!(formatter, "{other}")?,
          }
        }
        
        write!(formatter, "]")
      },

      Value::Void => write!(formatter, ""),
    }
  }
}
