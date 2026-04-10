use crate::runtime::error::{InterpreterError, RuntimeError};
use crate::runtime::value::Value;

pub fn call_method(receiver: &mut Value, method: &str, args: Vec<Value> ) -> RuntimeError<Value>
{
  match receiver
  {
    Value::Array(items) => call_array_method(items, method, args),
    Value::Number(number) => call_number_method(number, method, args),
    Value::String(text) => call_string_method(text, method, args),
    other =>
    {
      return Err(InterpreterError::NoMethodForType
      {
        target_type: other.type_name(),
      });
    }
  }
}

fn call_array_method(items: &mut Vec<Value>, method: &str, args: Vec<Value>) -> RuntimeError<Value>
{
  match method
  {
    "len" | "length" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      Ok(Value::Number(items.len() as f64))
    },

    "push" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        });
      }

      for arg in args
      {
        items.push(arg);
      }

      Ok(Value::Array(items.clone()))
    },

    "push_start" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        });
      }

      for arg in args.iter().rev()
      {
        items.insert(0, arg.clone());
      }

      Ok(Value::Array(items.clone()))
    },

    "pop" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if items.is_empty()
      {
        return Err(InterpreterError::InvalidOperationOnEmptyArray
        {
          function: method.to_string(),
        });
      }

      items.pop();
      Ok(Value::Array(items.clone()))
    },

    "pop_start" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if items.is_empty()
      {
        return Err(InterpreterError::InvalidOperationOnEmptyArray
        {
          function: method.to_string(),
        });
      }

      items.remove(0);
      Ok(Value::Array(items.clone()))
    },

    "contains" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      } else if args.len() >= 2
      {
        return Err(InterpreterError::TooManyArguments
        {
          function: method.to_string(),
          amount_of_args: 1,
        });
      }

      Ok(Value::Bool(items.contains(&args[0])))
    }

    "to_string" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      let mut string = String::new();
      for item in items
      {
        string.push_str(&item.to_string());
      }

      Ok(Value::String(string))
    },

    "reverse" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      items.reverse();
      Ok(Value::Array(items.clone()))
    }

    "clear" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      items.clear();
      Ok(Value::Array(Vec::new()))
    },

    "flatten" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      let mut flatten_vec = Vec::new();
      for item in items.drain(..)
      {
        flatten(item, &mut flatten_vec);
      }

      *items = flatten_vec;
      Ok(Value::Array(items.clone()))
    },

    _ => return Err(InterpreterError::UnknownMethod
    {
      function: method.to_string(),
      type_name: "Array".to_string(),
    }),
  }
}

fn call_string_method(string: &mut String, method: &str, args: Vec<Value>) -> RuntimeError<Value>
{
  match method
  {
    "len" | "length" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      Ok(Value::Number(string.chars().count() as f64))
    },

    "upper" | "uppercase" | "to_upper" | "to_uppercase" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      Ok(Value::String(string.to_uppercase()))
    },

    "lower" | "lowercase" | "to_lower" | "to_lowercase"=>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      Ok(Value::String(string.to_lowercase()))
    },

    "trim" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      Ok(Value::String(string.trim().to_string()))
    },

    "split" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      } else if args.len() >= 2
      {
        return Err(InterpreterError::TooManyArguments
        {
          function: method.to_string(),
          amount_of_args: 1,
        });
      }

      let delimiter = match &args[0]
      {
        Value::String(string) => string,
        _ =>
        {
          return Err(InterpreterError::IncorrectParameterType
          {
            function: method.to_string(),
            type_name: args[0].type_name().to_string(),
          });
        },
      };

      let mut parts = Vec::new();

      for part in string.split(delimiter)
      {
        parts.push(Value::String(part.to_string()));
      }

      Ok(Value::Array(parts))
    },

    "contains" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      } else if args.len() >= 2
      {
        return Err(InterpreterError::TooManyArguments
        {
          function: method.to_string(),
          amount_of_args: 1,
        });
      }

      let arg = match &args[0]
      {
        Value::String(string_arg) => string_arg,
        _ =>
        {
          return Err(InterpreterError::IncorrectParameterType
          {
            function: method.to_string(),
            type_name: args[0].type_name().to_string(),
          });
        }
      };

      Ok(Value::Bool(string.contains(arg)))
    },

    "starts_with" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      } else if args.len() >= 2
      {
        return Err(InterpreterError::TooManyArguments
        {
          function: method.to_string(),
          amount_of_args: 1,
        });
      }

      let arg = match &args[0]
      {
        Value::String(string_arg) => string_arg,
        _ =>
        {
          return Err(InterpreterError::IncorrectParameterType
          {
            function: method.to_string(),
            type_name: args[0].type_name().to_string(),
          });
        }
      };

      Ok(Value::Bool(string.starts_with(arg)))
    },

    "ends_with" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      } else if args.len() >= 2
      {
        return Err(InterpreterError::TooManyArguments
        {
          function: method.to_string(),
          amount_of_args: 1,
        });
      }

      let arg = match &args[0]
      {
        Value::String(string_arg) => string_arg,
        _ =>
        {
          return Err(InterpreterError::IncorrectParameterType
          {
            function: method.to_string(),
            type_name: args[0].type_name().to_string(),
          });
        }
      };

      Ok(Value::Bool(string.ends_with(arg)))
    },

    "replace" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      } else if args.len() >= 3
      {
        return Err(InterpreterError::TooManyArguments
        {
          function: method.to_string(),
          amount_of_args: 2,
        });
      } else if args.len() < 2
      {
        return Err(InterpreterError::TooFewArguments
        {
          function: method.to_string(),
          amount_of_args: 2,
        });
      }

      let args = match (&args[0], &args[1])
      {
        (Value::String(old), Value::String(new)) => (old, new),

        _ =>
        {
          return Err(InterpreterError::IncorrectParameterType
          {
            function: method.to_string(),
            type_name: if args[0].type_name().to_string() == "string" {args[1].type_name().to_string()} else {args[0].type_name().to_string()},
          });
        },
      };

      Ok(Value::String(string.replacen(args.0, args.1, 1)))
    },

    "replace_all" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      } else if args.len() >= 3
      {
        return Err(InterpreterError::TooManyArguments
        {
          function: method.to_string(),
          amount_of_args: 2,
        });
      } else if args.len() < 2
      {
        return Err(InterpreterError::TooFewArguments
        {
          function: method.to_string(),
          amount_of_args: 2,
        });
      }

      let args = match (&args[0], &args[1])
      {
        (Value::String(old), Value::String(new)) => (old, new),

        _ =>
        {
          return Err(InterpreterError::IncorrectParameterType
          {
            function: method.to_string(),
            type_name: if args[0].type_name().to_string() == "string" {args[1].type_name().to_string()} else {args[0].type_name().to_string()},
          });
        },
      };

      Ok(Value::String(string.replace(args.0, args.1)))
    },

    "is_empty" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      Ok(Value::Bool(string.is_empty()))
    }

    "reverse" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      Ok(Value::String(string.chars().rev().collect()))
    }

    "to_chars" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      let mut characters = Vec::new();
      for character in string.chars()
      {
        characters.push(Value::String(character.to_string()));
      }

      Ok(Value::Array(characters))
    },

    "to_number" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      let text = string.trim();
      match text.parse::<f64>()
      {
        Ok(number) => Ok(Value::Number(number)),
        Err(_) => Err(InterpreterError::TypeMismatch
        {
          expected: "Number".to_string(),
          found: string.trim().to_string(),
        }),
      }
    },

    "is_number" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      let text = string.trim();
      match text.parse::<f64>()
      {
        Ok(_) => Ok(Value::Bool(true)),
        Err(_) => Ok(Value::Bool(false)),
      }
    }

    "to_bool" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      let text = string.trim();
      match text.parse::<bool>()
      {
        Ok(boolean) => Ok(Value::Bool(boolean)),
        Err(_) => Err(InterpreterError::TypeMismatch
        {
          expected: "Boolean".to_string(),
          found: string.trim().to_string(),
        }),
      }
    },

    "is_bool" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      let text = string.trim();
      match text.parse::<bool>()
      {
        Ok(_) => Ok(Value::Bool(true)),
        Err(_) => Ok(Value::Bool(false)),
      }
    }

    _ => return Err(InterpreterError::UnknownMethod
    {
      function: method.to_string(),
      type_name: "String".to_string(),
    }),
  }
}

fn call_number_method(number: &mut f64, method: &str, args: Vec<Value>) -> RuntimeError<Value>
{
  match method
  {
    "abs" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      Ok(Value::Number(number.abs()))
    },

    "floor" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      Ok(Value::Number(number.floor()))
    },

    "ceil" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      Ok(Value::Number(number.ceil()))
    },

    "round" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      Ok(Value::Number(number.round()))
    },

    "sqrt" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      if *number < 0.0
      {
        return Err(InterpreterError::NegativeSquareRootUsage
        {
          value: *number,
        })
      }

      Ok(Value::Number(number.sqrt()))
    },

    "to_string" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      Ok(Value::String(number.to_string()))
    },

    "to_array" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      let number_string = number.to_string();
      let is_negative = *number < 0.0;
      let mut digits: Vec<Value> = Vec::new();
      let mut first_digit = true;
      for character in number_string.chars()
      {
        if character == '.' || character == '-'
        {
          continue;
        }

        if let Some(digit) = character.to_digit(10)
        {
          let value = if is_negative && first_digit && digit != 0 {-(digit as f64)} else {digit as f64};

          digits.push(Value::Number(value));
          first_digit = false;
        } else
        {
          return Err(InterpreterError::InvalidNumericIndexSource
          {
            position: digits.len(),
            value: *number,
          })
        }
      }

      Ok(Value::Array(digits))
    },

    "has_fraction" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      if number.fract() != 0.0
      {
        return Ok(Value::Bool(true));
      }

      Ok(Value::Bool(false))
    },

    "is_even" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      if number.fract() != 0.0
      {
        return Err(InterpreterError::NonIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      if *number % 2.0 as f64 == 0.0
      {
        return Ok(Value::Bool(true));
      }

      Ok(Value::Bool(false))
    },

    "is_odd" =>
    {
      if !args.is_empty()
      {
        return Err(InterpreterError::ArgumentsInNoArgumentFunction
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      if number.fract() != 0.0
      {
        return Err(InterpreterError::NonIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      if *number % 2.0 as f64 == 0.0
      {
        return Ok(Value::Bool(false));
      }

      Ok(Value::Bool(true))
    },

    "is_divisible" | "is_multiple" =>
    {
      if args.is_empty()
      {
        return Err(InterpreterError::MissingArguments
        {
          function: method.to_string(),
          amount_of_args: args.len(),
        })
      }

      if !number.is_finite()
      {
        return Err(InterpreterError::NonFiniteIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      if number.fract() != 0.0
      {
        return Err(InterpreterError::NonIntegerArgument
        {
          function: method.to_string(),
          value: number.clone(),
        })
      }

      let mut numbers = Vec::new();

      for arg in args
      {
        match arg
        {
          Value::Number(number) =>
          {
            if number.fract() != 0.0
            {
              return Err(InterpreterError::NonIntegerArgument
              {
                function: method.to_string(),
                value: number.clone(),
              })
            }

            numbers.push(number)
          },

          other =>
          {
            return Err(InterpreterError::TypeMismatch
            {
              expected: "number".to_string(),
              found: other.type_name().to_string(),
            });
          }
        }
      }

      for parameter_number in numbers
      {
        if parameter_number == 0.0
        {
          return Err(InterpreterError::DivideBy0
          {
            value: *number,
          })
        }

        if *number % parameter_number != 0.0
        {
          return Ok(Value::Bool(false))
        }
      }

      Ok(Value::Bool(true))
    },

    _ => return Err(InterpreterError::UnknownMethod
    {
      function: method.to_string(),
      type_name: "Number".to_string(),
    }),
  }
}

fn flatten(value: Value, result: &mut Vec<Value>)
{
  match value
  {
    Value::Array(inner_items) =>
    {
      for inner_item in inner_items
      {
        flatten(inner_item, result);
      }
    }

    _ => result.push(value),
  }
}
