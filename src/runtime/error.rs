use thiserror::Error;
use miette::Diagnostic;

#[derive(Error, Diagnostic, Debug)]
pub enum InterpreterError
{
  #[error("Undefined variable `{name}`")]
  #[diagnostic(
    code(runtime::undefined_variable),
    help("Variables must be declared with `let` before they can be used")
  )]
  UndefinedVariable { name: String },

  #[error("Type mismatch: expected `{expected}`, found `{found}`")]
  #[diagnostic(
    code(runtime::type_mismatch),
    help("Make sure the value you are assigning matches the declared type")
  )]
  TypeMismatch
  {
    expected: String,
    found: String,
  },

  #[error("Cannot divide {value} by zero")]
  #[diagnostic(
    code(runtime::divide_by_zero),
    help("Check that the divisor is not zero before performing the division")
  )]
  DivideBy0
  {
    value: f64,
  },

  #[error("Cannot multiply string `{text}` by zero or a negative number")]
  #[diagnostic(
    code(runtime::multiply_string_invalid),
    help("The multiplier must be a positive whole number (e.g. `\"ha\" * 3`)")
  )]
  MultiplyStringByInvalidValue
  {
    text: String,
  },

  #[error("Operator `{op}` cannot be applied to types `{left}` and `{right}`")]
  #[diagnostic(
    code(runtime::invalid_binary_op),
    help("Check that both operands are of a type that supports this operator")
  )]
  InvalidBinaryOp
  {
    op: &'static str,
    left: &'static str,
    right: &'static str,
  },

  #[error("Cannot compare `{left}` with `{right}` using `{op}`")]
  #[diagnostic(
    code(runtime::invalid_type_comparison),
    help("Comparison operators only work between two values of the same type")
  )]
  InvalidTypeComparison
  {
    op: &'static str,
    left: &'static str,
    right: &'static str,
  },

  #[error("Invalid range: {left}..{right} produces an empty range because {left} is greater than {right}")]
  #[diagnostic(
    code(runtime::invalid_range),
    help("The left side of a range must be less than or equal to the right side")
  )]
  InvalidRange
  {
    left: f64,
    right: f64,
  },

  #[error("Range bounds must be whole integers, but got {left} and {right}")]
  #[diagnostic(
    code(runtime::non_integer_range),
    help("Range values cannot have a fractional part (e.g. use `1..5`, not `1.5..4.5`)")
  )]
  NonIntegerRange
  {
    left: f64,
    right: f64,
  },

  #[error("Repeat count must be a positive whole number, but got {value}")]
  #[diagnostic(
    code(runtime::invalid_repeat_count),
    help("The repeat count cannot be negative or a decimal (e.g. use `repeat 5 as i`, not `repeat 3.5 as i`)")
  )]
  NonValidIntegerCount
  {
    value: f64,
  },

  #[error("Cannot use `{op}` to assign a `{right}` value to a `{left}` variable")]
  #[diagnostic(
    code(runtime::invalid_assign_types),
    help("The type of the value being assigned must match the type of the variable")
  )]
  NonAllowedAssignOp
  {
    op: &'static str,
    left: &'static str,
    right: &'static str,
  },

  #[error("Operator `{op}` is not supported for type `{left}`")]
  #[diagnostic(
    code(runtime::unsupported_assign_op),
    help("Not all compound assignment operators work on every type (e.g. `-=` does not work on strings)")
  )]
  UnsupportedAssignOp
  {
    op: &'static str,
    left: &'static str,
  },

  #[error("`{op}` requires a whole positive number as the multiplier, not a decimal")]
  #[diagnostic(
    code(runtime::invalid_multiply_assign),
    help("Use a whole number when multiplying a string (e.g. `x *= 3`, not `x *= 2.5`)")
  )]
  InvalidMultiplyAssignValue
  {
    op: &'static str,
  },

  #[error("Invalid assignment target")]
  #[diagnostic(
    code(runtime::invalid_assignment_target),
    help("Only variables and array index expressions can be assigned to (e.g. `x = 5` or `arr[0] = 5`)")
  )]
  InvalidAssignmentTarget,

  #[error("Array index is out of bounds")]
  #[diagnostic(
    code(runtime::invalid_index),
    help("Make sure the index is within the bounds of the array")
  )]
  NonValidIndex,

  #[error("Function `{function}` is not defined")]
  #[diagnostic(
    code(runtime::undefined_function),
    help("Make sure the function is declared before it is called")
  )]
  UndefinedCallTarget
  {
    function: String,
  },

  #[error("Invalid call target: expected a function name")]
  #[diagnostic(
    code(runtime::invalid_call_target)
  )]
  InvalidCallTarget,

  #[error("Function `{function}` expects at most {amount_of_args} argument(s), but received more")]
  #[diagnostic(
    code(runtime::too_many_arguments),
    help("Remove the extra arguments from the function call")
  )]
  TooManyArguments
  {
    function: String,
    amount_of_args: usize,
  },

  #[error("Function `{function}` expects at least {amount_of_args} argument(s), but received fewer")]
  #[diagnostic(
    code(runtime::too_few_arguments),
    help("Make sure you are passing all required arguments to the function")
  )]
  TooFewArguments
  {
    function: String,
    amount_of_args: usize,
  },

  #[error("`break` and `continue` cannot be used outside of a loop in function `{function}`")]
  #[diagnostic(
    code(runtime::invalid_loop_escape),
    help("`break` and `continue` are only valid inside `while` or `repeat` loops")
  )]
  InvalidEscapeFunctionCall
  {
    function: String,
  },

  #[error("Index value {index} is not valid because it {reason}")]
  #[diagnostic(
    code(runtime::invalid_index_value),
    help("Indices must be positive whole numbers that are within the length of the value being indexed")
  )]
  InvalidIndexValue
  {
    index: f64,
    reason: &'static str,
  },

  #[error("Index must be of type `number`, but got type `{index}`")]
  #[diagnostic(
    code(runtime::non_number_index),
    help("Only numbers can be used as indices (e.g. `arr[0]`, not `arr[\"key\"]`)")
  )]
  NonNumberIndexValue
  {
    index: &'static str,
  },

  #[error("Type `{target_type}` cannot be indexed")]
  #[diagnostic(
    code(runtime::non_indexable_value),
    help("Only `number`, `array`, and `string` values support index access")
  )]
  NonIndexableValue
  {
    target_type: &'static str,
  },

  #[error("Character at position {position} in numeric value {value} is not a valid digit")]
  #[diagnostic(
    code(runtime::invalid_numeric_index_source)
  )]
  InvalidNumericIndexSource
  {
    position: usize,
    value: f64,
  },

  #[error("`{property}` is a method on `{target_type}` and must be called with parentheses")]
  #[diagnostic(
    code(runtime::method_call_required),
    help("Add parentheses to call the method (e.g. `.{property}()` instead of `.{property}`)")
  )]
  MethodCallRequired
  {
    property: String,
    target_type: &'static str,
  },

  #[error("Type `{target_type}` does not have any methods")]
  #[diagnostic(
    code(runtime::no_method_for_type),
    help("Methods are only available on `number`, `string`, and `array` values")
  )]
  NoMethodForType
  {
    target_type: &'static str,
  },

  #[error("`{function}()` takes no arguments, but {amount_of_args} were provided")]
  #[diagnostic(
    code(runtime::unexpected_arguments),
    help("Remove the arguments from the call")
  )]
  ArgumentsInNoArgumentFunction
  {
    function: String,
    amount_of_args: usize,
  },

  #[error("`{function}()` requires at least one argument, but none were provided")]
  #[diagnostic(
    code(runtime::missing_arguments),
    help("Pass the required argument(s) to the method")
  )]
  MissingArguments
  {
    function: String,
    amount_of_args: usize,
  },

  #[error("`{function}` is not a known method on type `{type_name}`")]
  #[diagnostic(
    code(runtime::unknown_method),
    help("Check the spelling of the method name or consult the documentation for available methods")
  )]
  UnknownMethod
  {
    function: String,
    type_name: String,
  },

  #[error("`{function}()` cannot be called on an empty array")]
  #[diagnostic(
    code(runtime::empty_array_operation),
    help("Make sure the array contains at least one element before calling this method")
  )]
  InvalidOperationOnEmptyArray
  {
    function: String,
  },

  #[error("`{function}()` expected a `string` argument, but got `{type_name}`")]
  #[diagnostic(
    code(runtime::incorrect_parameter_type),
    help("Pass a string value as the argument to this method")
  )]
  IncorrectParameterType
  {
    function: String,
    type_name: String,
  },

  #[error("`print` and `println` require a format string when passing arguments")]
  #[diagnostic(
    code(print::no_formatter),
    help("Use a format string with `@` as a placeholder (e.g. println(\"@\", {value}) instead of println({value}))")
  )]
  NoValidFormatterFound
  {
    value: String
  },

  #[error("`{function}()` requires a whole integer, but was called with {value}")]
  #[diagnostic(
    code(runtime::non_integer_argument),
    help("Call `.floor()` or `.ceil()` on the number first to remove the fractional part")
  )]
  NonIntegerArgument
  {
    function: String,
    value: f64,
  },

  #[error("`{function}()` requires a finite number, but was called with {value}")]
  #[diagnostic(
    code(runtime::non_finite_argument),
    help("The value must not be infinity or NaN")
  )]
  NonFiniteIntegerArgument
  {
    function: String,
    value: f64,
  },

  #[error("Cannot take the square root of a negative number ({value})")]
  #[diagnostic(
    code(runtime::negative_sqrt),
    help("Make sure the value is zero or positive before calling `.sqrt()`")
  )]
  NegativeSquareRootUsage
  {
    value: f64,
  },

  #[error("Missing `entry` directive before {something}")]
  #[diagnostic(
    code(runtime::missing_entry),
    help("Every Astra file must begin with `entry \"functionName\";` to declare the entry point")
  )]
  MissingEntryPoint
  {
    something: String,
  },

  #[error("Only one `entry` directive is allowed per file")]
  #[diagnostic(
    code(runtime::duplicate_entry)
  )]
  DuplicateEntry,

  #[error("'{item}' cannot appear here")]
  #[diagnostic(
    code(runtime::header_order_violation),
    help("The `entry` directive must come before any functions or statements")
  )]
  HeaderOrderViolation
  {
    item: String,
  },

  #[error("Function `{name}` is already defined")]
  #[diagnostic(
    code(runtime::duplicate_function),
    help("Each function must have a unique name — rename or remove one of the definitions")
  )]
  DuplicateFunction
  {
    name: String,
  },

  #[error("Function `{function}` declared return type `{expected}` but returned a value of type `{found}`")]
  #[diagnostic(
    code(runtime::return_type_mismatch),
    help("Make sure the returned value matches the declared return type of the function")
  )]
  ReturnTypeMismatch
  {
    function: String,
    expected: String,
    found: String,
  },

  #[error("Non-exhaustive match: no arm matched the value `{value}`")]
  #[diagnostic(
    code(runtime::non_exhaustive_match),
    help("Add a wildcard arm `_ => {{ ... }}` to handle any value that does not match a specific arm")
  )]
  NonExhaustiveMatch
  {
    value: String,
  },
}

pub type RuntimeError<T> = Result<T, InterpreterError>;
