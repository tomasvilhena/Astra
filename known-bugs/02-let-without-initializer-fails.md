# Bug 02 — `let x: T;` without an initializer always errors

- [X] **Fixed**
- **Severity:** 🔴 Critical
- **Component:** Interpreter (statement execution)
- **File:** `src/runtime/interpreter.rs`
- **Lines:** ~862–888 (`exec_stmt` / `Stmt::Let` arm)

## Summary

The parser allows `let x: number;` with no `= value` part (see `parse_let_stmt`), but the interpreter always runs a type check that compares the declared type against the evaluated value. When no initializer is present, the evaluated value is `Value::Void`, which never matches the declared type, so every uninitialized typed declaration errors out.

## Reproduction

```astra
entry "main";

fn main(): void {
  let x: number;
  println("@", x);
}
```

Output:

```
Error:   × Type mismatch: expected `number`, found `void`
```

## Offending code

```rust
// src/runtime/interpreter.rs
let evaluated = if let Some(value) = value {
  self.eval_expr(value)?
} else {
  Value::Void
};

if let Some(declared_type) = var_type {
  let actual_type = evaluated.type_name();
  if declared_type != actual_type {
    return Err(InterpreterError::TypeMismatch { /* ... */ });
  }
}
```

The type check runs unconditionally — even for the `Value::Void` fallback.

## Suggested fix

Either:

1. Skip the type check when no initializer was provided, and treat the binding as "declared but unassigned" (require a subsequent assignment before read), or
2. Insert a type-appropriate default (`Value::Number(0.0)`, `Value::String(String::new())`, `Value::Bool(false)`, `Value::Array(Vec::new())`) when no initializer is present, and only type-check when `value.is_some()`.

Option 1 is more principled but requires a new runtime state ("uninitialized"); option 2 matches how many scripting languages behave.

## How to verify

Running the reproduction should either:

- succeed silently (option 1, with a read-before-assign error if `x` is subsequently used), or
- succeed and print a sensible default (option 2).
