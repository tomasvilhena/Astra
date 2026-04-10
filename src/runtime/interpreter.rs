use std::any::Any;
use std::clone;
use std::collections::HashMap;
use std::fmt::format;
use std::sync::mpsc::Receiver;
use std::thread::current;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;
use crate::frontend::ast::{AssignOperator, AssignTarget, BinaryOperator, Expr, Pattern, Stmt, UnaryOperator};
use crate::frontend::lexer::TokenKind;
use crate::runtime::value::{self, Value};
use crate::runtime::methods;
use crate::runtime::error::{InterpreterError, RuntimeError};

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

  pub fn run(&mut self, program: &[Stmt]) -> RuntimeError<()>
  {
    let mut entry_name: Option<String> = None;

    for stmt in program
    {
      match stmt
      {
        Stmt::Function { name, params, return_type, body } =>
        {
          self.functions.insert(name.clone(), FunctionDef { params, body});
        },
        Stmt::Entry { name } => {},
        _ => {},
      }
    }

    self.exec_block(program)?;
    Ok(())
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
        match callee.as_ref()
        {
          Expr::Identifier(name) =>
          {
            let (params, body)  = if let Some(function) = self.functions.get(name)
            {
              (function.params.clone(), function.body.clone())
            } else
            {
              return Err(InterpreterError::UndefinedCallTarget
              {
                function: name.clone(),
              })
            };

            let mut evaluated_args: Vec<Value> = Vec::new();
            for arg in args
            {
              evaluated_args.push(self.eval_expr(arg)?);
            }

            if evaluated_args.len() > params.len()
            {
              return Err(InterpreterError::TooManyArguments
              {
                function: name.clone(),
                amount_of_args: params.len()
              });
            } else if evaluated_args.len() < params.len()
            {
              return Err(InterpreterError::TooFewArguments
              {
                function: name.clone(),
                amount_of_args: params.len()
              });
            }

            let mut frame = CallFrame {locals: HashMap::new()};

            for (index, param) in params.iter().enumerate()
            {
              frame.locals.insert(param.clone(), evaluated_args[index].clone());
            }

            self.call_stack.push(frame);
            let flow = self.exec_block(&body);
            self.call_stack.pop();
            let flow = flow?;

            match flow
            {
              ControlFlow::Return(value) => Ok(value),

              ControlFlow::None => Ok(Value::Void),

              ControlFlow::Break
              | ControlFlow::Continue => return Err(InterpreterError::InvalidEscapeFunctionCall
              {
                function: name.clone(),
              }),
            }
          },

          Expr::Member { object, property } =>
          {
            let mut evaluated_args: Vec<Value> = Vec::new();
            for arg in args
            {
              evaluated_args.push(self.eval_expr(arg)?);
            }

            match object.as_ref()
            {
              Expr::Identifier(name) =>
              {
                let mut receiver = self.eval_expr(object)?;
                let result = methods::call_method(&mut receiver, property, evaluated_args)?;
                self.assign_var(name, receiver)?;
                return Ok(result)
              },

              _ =>
              {
                let mut receiver = self.eval_expr(object)?;
                let result = methods::call_method(&mut receiver, property, evaluated_args)?;
                Ok(result)
              }
            }
          },

          _ =>
          {
            return Err(InterpreterError::InvalidCallTarget);
          }
        }
      },

      Expr::Member { object, property } =>
      {
        let receiver = self.eval_expr(object)?;
        Err(InterpreterError::MethodCallRequired
        {
          property: property.clone(),
          target_type: receiver.type_name(),
        })
      },

      Expr::Index { object, index } =>
      {
        let object  = self.eval_expr(object)?;
        let index  = self.eval_expr(index)?;

        let index = match index
        {
          Value::Number(index) =>
          {
            if index.fract() != 0.0
            {
              return Err(InterpreterError::InvalidIndexValue
              {
                index,
                reason: " contains a fractional part",
              });
            } else if index < 0.0
            {
              return Err(InterpreterError::InvalidIndexValue
              {
                index,
                reason: " is out of bounds, therefore it does not correspond with any position of the element it indexes"
              });
            }

            index as usize
          },

          index =>
          {
            return Err(InterpreterError::NonNumberIndexValue
            {
              index: index.type_name(),
            });
          }
        };

        match object
        {
          Value::Array(object) =>
          {
            if index >= object.len()
            {
              return Err(InterpreterError::InvalidIndexValue
              {
                index: index as f64,
                reason: " is out of bounds, therefore it does not correspond with any position of the element it indexes"
              });
            }

            return Ok(object[index].clone());
          },

          Value::String(object) =>
          {
            match object.chars().nth(index)
            {
              Some(character) =>
              {
                return Ok(Value::String(character.to_string()));
              }

              None =>
              {
                return Err(InterpreterError::InvalidIndexValue
                {
                  index: index as f64,
                  reason: " is out of bounds",
                });
              }
            }
          }

          Value::Number(object) =>
          {
            let mut number: String = object.abs().to_string();
            number.retain(| character | character != '.' && character != '-');

            match number.chars().nth(index)
            {
              Some(character) =>
              {
                if let Some(digit) = character.to_digit(10)
                {
                  return Ok(Value::Number(digit as f64));
                } else
                {
                  return Err(InterpreterError::InvalidNumericIndexSource
                  {
                    position: index,
                    value: object,
                  })
                }
              }

              None =>
              {
                return Err(InterpreterError::InvalidIndexValue
                {
                  index: index as f64,
                  reason: " is out of bounds",
                });
              }
            }
          }

          object =>
          {
            return Err(InterpreterError::NonIndexableValue
            {
              target_type: object.type_name(),
            });
          }
        }
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
            return Err(InterpreterError::InvalidTypeComparison
            {
              op: Self::binary_op_to_str(op),
              left: left.type_name(),
              right: right.type_name(),
            })
          }
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

      Stmt::Assign { target, op, value } =>
      {
        let right = self.eval_expr(value)?;

        match target
        {
          AssignTarget::Variable(name) =>
          {
            let current = self.get_var(name)?;
            let new_value= self.apply_assign_op(op, current.clone(), right)?;
            self.assign_var(name, new_value)?;
          },

          AssignTarget::Index { object, index } =>
          {
            let array_name = match object
            {
              Expr::Identifier(name) => name,
              _ => return Err(InterpreterError::InvalidAssignmentTarget),
            };

            let mut array_items = match self.get_var(array_name)?
            {
              Value::Array(value) => value,
              other => return Err(InterpreterError::TypeMismatch
              {
                expected: "array".to_string(),
                found: other.type_name().to_string(),
              }),
            };

            let index = match self.eval_expr(index)?
            {
              Value::Number(number) if number.fract() == 0.0 && number >= 0.0 => number as usize,
              _ => return Err(InterpreterError::NonValidIndex),
            };

            if index >= array_items.len()
            {
              return Err(InterpreterError::NonValidIndex);
            }

            let current_value = array_items[index].clone();
            let new_value = self.apply_assign_op(op, current_value, right)?;
            array_items[index] = new_value;

            self.assign_var(array_name, Value::Array(array_items))?;
          },
        }

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

            for index  in 0..repeat_count
            {
              if let Some(name) = index_name
              {
                self.set_var(name.clone(), Value::Number(index as f64));
              }

              let flow = self.exec_block(body)?;
              match flow
              {
                ControlFlow::Break => break,
                ControlFlow::Continue => continue,
                ControlFlow::    Return(value) => {return Ok(ControlFlow::Return(value))},
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
        let mut  expressions = Vec::new();
        for arg in args
        {
          expressions.push(self.eval_expr(arg)?.to_string());
        }

        if text_string.is_none() && !expressions.is_empty()
        {
          return Err(InterpreterError::NoValidFormatterFound
          {
            value: expressions[0].to_string(),
          })
        }

        let text_string = text_string.as_deref().unwrap_or("");
        let string_placeholder_amount = Self::count_placeholders_in_string(text_string);

        if expressions.len() > string_placeholder_amount
        {
          return Err(InterpreterError::TooManyArguments
          {
            function: if *new_line {"println".to_string()} else {"print".to_string()},
            amount_of_args: string_placeholder_amount,
          })
        } else if expressions.len() < string_placeholder_amount
        {
          return Err(InterpreterError::TooFewArguments
          {
            function: if *new_line {"println".to_string()} else {"print".to_string()},
            amount_of_args: string_placeholder_amount as usize,
          })
        }

        let output = Self::format_output(text_string, &expressions);

        if *new_line
        {
          println!("{output}");
        } else
        {
          print!("{output}");
        }

        Ok(ControlFlow::None)
      },

      Stmt::Function { name, params, return_type, body } =>
      {
        let mut param_names: Vec<String> = Vec::new();

        for (param_name, _param_type) in params
        {
          param_names.push(param_name.to_string());
        }

        self.functions.insert(
          name.clone(),
          FunctionDef
          {
            params: param_names,
            body: body.clone(),
          }
        );

        Ok(ControlFlow::None)
      },

      Stmt::Match { value, arms } =>
      {
        let matched_value = self.eval_expr(value)?;

        for (pattern, body) in arms
        {
          if Self::pattern_matches(pattern, &matched_value)
          {
            let flow = self.exec_block(body)?;
            return Ok(flow);
          }
        }

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
        ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
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

  fn get_var(&mut self, name: &str) -> RuntimeError<Value>
  {
    if let Some(frame) = self.call_stack.last()
    {
      if let Some(value) = frame.locals.get(name)
      {
        return Ok(value.clone());
      }
    }

    self.global
      .get(name)
      .cloned()
      .ok_or
      (
        InterpreterError::UndefinedVariable
        {
          name: name.to_string(),
        }
      )
  }

  fn assign_var(&mut self, name: &str, value: Value) -> RuntimeError<()>
  {
    if let Some(frame) = self.call_stack.last_mut()
    {
      if frame.locals.contains_key(name)
      {
        frame.locals.insert(name.to_string(), value);
        return Ok(());
      }
    }

    if self.global.contains_key(name)
    {
      self.global.insert(name.to_string(), value);
      return Ok(());
    }

    Err(InterpreterError::UndefinedVariable
    {
      name: name.to_string(),
    })
  }

  fn apply_assign_op(&self, op: &AssignOperator, current: Value, right: Value) -> RuntimeError<Value>
  {
    match op
    {
      AssignOperator::Assign =>
      {
        match (current, right)
        {
          (Value::Number(current), Value::Number(right)) =>
          {
            Ok(Value::Number(right))
          },

          (Value::String(current), Value::String(right)) =>
          {
            Ok(Value::String(right))
          },

          (Value::Bool(current), Value::Bool(right)) =>
          {
            Ok(Value::Bool(right))
          },

          (Value::Array(current), Value::Array(right)) =>
          {
            Ok(Value::Array(right))
          },

          (current, right) =>
          {
            Err(InterpreterError::NonAllowedAssignOp
            {
              op: "=",
              left: current.type_name(),
              right: right.type_name(),
            })
          },

        }
      },

      AssignOperator::CaretAssign =>
      {
        match (current, right)
        {
          (Value::Number(current), Value::Number(right)) =>
          {
            Ok(Value::Number(current.powf(right)))
          },

          (current, right) =>
          {
            Err(InterpreterError::UnsupportedAssignOp
            {
              op: "^=",
              left: current.type_name(),
            })
          },
        }
      },

      AssignOperator::MinusAssign =>
      {
        match (current, right)
        {
          (Value::Number(current), Value::Number(right)) =>
          {
            Ok(Value::Number(current - right))
          },

          (current, right) =>
          {
            Err(InterpreterError::UnsupportedAssignOp
            {
              op: "-=",
              left: current.type_name(),
            })
          },
        }
      },

      AssignOperator::PercentAssign =>
      {
        match (current, right)
        {
          (Value::Number(current), Value::Number(right)) =>
          {
            if right == 0.0
            {
              return Err(InterpreterError::DivideBy0
              {
                value: current,
              });
            }

            Ok(Value::Number(current % right))
          },

          (current, right) =>
          {
            Err(InterpreterError::UnsupportedAssignOp
            {
              op: "%=",
              left: current.type_name(),
            })
          },
        }
      },

      AssignOperator::PlusAssign =>
      {
        match (current, right)
        {
          (Value::Number(current), Value::Number(right)) =>
          {
            Ok(Value::Number(current + right))
          },

          (Value::String(current), Value::String(right)) =>
          {
            Ok(Value::String(current + &right))
          },

          (current, right) =>
          {
            Err(InterpreterError::UnsupportedAssignOp
            {
              op: "+=",
              left: current.type_name(),
            })
          },
        }
      },

      AssignOperator::SlashAssign =>
      {
        match (current, right)
        {
          (Value::Number(current), Value::Number(right)) =>
          {
            if right == 0.0
            {
              return Err(InterpreterError::DivideBy0
              {
                value: current,
              });
            }

            Ok(Value::Number(current / right))
          },

          (current, right) =>
          {
            Err(InterpreterError::UnsupportedAssignOp
            {
              op: "/=",
              left: current.type_name(),
            })
          },
        }
      },

      AssignOperator::StarAssign =>
      {
        match (current, right)
        {
          (Value::Number(current), Value::Number(right)) =>
          {
            Ok(Value::Number(current * right))
          },

          (Value::String(current), Value::Number(right)) =>
          {
            if right.fract() != 0.0 || right < 0.0
            {
              return Err(InterpreterError::InvalidMultiplyAssignValue
              {
                op: "*="
              });
            }

            Ok(Value::String(current.repeat(right as usize)))
          },

          (current, right) =>
          {
            Err(InterpreterError::UnsupportedAssignOp
            {
              op: "*=",
              left: current.type_name(),
            })
          },
        }
      },
    }
  }

  fn count_placeholders_in_string(text_string: &str) -> usize
  {
    let mut count = 0;
    let mut chars = text_string.chars().peekable();

    while let Some(char) = chars.next()
    {
      if char == '@'
      {
        if let Some('@') = chars.peek()
        {
          chars.next();
        } else
        {
          count += 1;
        }
      }
    }

    count
  }

  fn format_output(text_string: &str, expressions: &[String]) -> String
  {
    let mut output = String::with_capacity(text_string.len() + expressions.iter().map(|e| e.len()).sum::<usize>());
    let mut chars = text_string.chars().peekable();
    let mut index = 0;

    while let Some(char) = chars.next()
    {
      if char == '@'
      {
        match chars.peek()
        {
          Some('@') =>
          {
            output.push('@');
            chars.next();
          },

          _ =>
          {
            if index < expressions.len()
            {
              output.push_str(&expressions[index]);
              index += 1;
            }
          },
        }
      } else
      {
        output.push(char);
      }
    }

    output
  }

  fn pattern_matches(pattern: &Pattern, value: &Value) -> bool
  {
    return match pattern
    {
      Pattern::Wildcard => true,
      Pattern::Number(number) => if *value == Value::Number(*number) {true} else {false},
      Pattern::String(string) => if *value == Value::String(string.clone()) {true} else {false},
      Pattern::Bool(boolean) => if *value == Value::Bool(*boolean) {true} else {false},
      Pattern::Identifier(_) => false,
    };
  }
}
