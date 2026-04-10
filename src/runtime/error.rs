use thiserror::Error;
use miette::Diagnostic;

use crate::runtime::value::Value;

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
  InvalidTypeComparison
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

  #[error("The value {value} needs to be a number without a fractional part, and it needs to be positive to be usable in a loop")]
  NonValidIntegerCount
  {
    value: f64,
  },

  #[error("The operator {op} is not allowed between type {left} and type {right}")]
  NonAllowedAssignOp
  {
    op: &'static str,
    left: &'static str,
    right: &'static str,
  },

  #[error("The operator {op} does not work for the type {left}")]
  UnsupportedAssignOp
  {
    op: &'static str,
    left: &'static str,
  },

  #[error("The operator {op} does not allow the multiplication of a string by a number with a fractional part")]
  InvalidMultiplyAssignValue
  {
    op: &'static str,
  },

  #[error("The target is invalid in an assignment operation")]
  InvalidAssignmentTarget,

  #[error("The provided index is out of bounds or not a valid index")]
  NonValidIndex,

  #[error("The called function {function} does not exist")]
  UndefinedCallTarget
  {
    function: String,
  },

  #[error("Invalid call target, expected function identifier")]
  InvalidCallTarget,

  #[error("The called function {function} does not accept more then {amount_of_args} arguments")]
  TooManyArguments
  {
    function: String,
    amount_of_args: usize,
  },

  #[error("The called function {function} does not accept less then {amount_of_args} arguments")]
  TooFewArguments
  {
    function: String,
    amount_of_args: usize,
  },

  #[error("The function {function} cannot be exited with the use of break or continue statements")]
  InvalidEscapeFunctionCall
  {
    function: String,
  },

  #[error("The provided index value {index} cannot be used as index because it {reason}")]
  InvalidIndexValue
  {
    index: f64,
    reason: &'static str,
  },

  #[error("The provided index was of value {index},  expected index of type 'Number'")]
  NonNumberIndexValue
  {
    index: &'static str,
  },

  #[error("Value of type {target_type} is not indexable. Supported indexable types: Number, Array and String")]
  NonIndexableValue
  {
    target_type: &'static str,
  },

  #[error("Character at position {position} in numeric value {value} is not a digit")]
  InvalidNumericIndexSource
  {
    position: usize,
    value: f64,
  },

  #[error("Invalid member access: the property {property} on the type {target_type} must be called as a method (e.g .{property}())")]
  MethodCallRequired
  {
    property: String,
    target_type: &'static str,
  },

  #[error("The type {target_type} does not support methods")]
  NoMethodForType
  {
    target_type: &'static str,
  },

  #[error("The function {function} expected no arguments, but got {amount_of_args}")]
  ArgumentsInNoArgumentFunction
  {
    function: String,
    amount_of_args: usize,
  },

  #[error("The function {function} expected at least 1 argument, but got {amount_of_args}")]
  MissingArguments
  {
    function: String,
    amount_of_args: usize,
  },

  #[error("A method with the name {function} was not found for type {type_name}")]
  UnknownMethod
  {
    function: String,
    type_name: String,
  },

  #[error("The array method {function} requires the array to contain at least 1 item")]
  InvalidOperationOnEmptyArray
  {
    function: String,
  },

  #[error("The method {function} expected an argument of type 'String', but got an argument of type {type_name}")]
  IncorrectParameterType
  {
    function: String,
    type_name: String,
  },

  #[error("`print!` or `println!` requires a format string with `@` to include arguments")]
  #[diagnostic(
    code(print::no_formatter),
    help("Use a format string, e.g., println!(\"@\", {value}) instead of println!({value})")
  )]
  NoValidFormatterFound
  {
    value: String
  },

  #[error("The function {function} can only be called on a number with no fractional share, but was called on {value}")]
  NonIntegerArgument
  {
    function: String,
    value: f64,
  },

  #[error("The function {function} can only be called on finite numbers, but was called on {value}")]
  NonFiniteIntegerArgument
  {
    function: String,
    value: f64,
  },

  #[error("The Square root function can only be called on positive values, but only the value {value} was found")]
  NegativeSquareRootUsage
  {
    value: f64,
  }
}

pub type RuntimeError<T> = Result<T, InterpreterError>;
