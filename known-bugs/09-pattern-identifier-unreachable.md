# Bug 09 — `Pattern::Identifier` is defined but unreachable

- [X] **Fixed**
- **Severity:** 🟡 Minor (dead code)
- **Component:** AST / Parser / Interpreter
- **Files:**
  - `src/frontend/ast.rs` (definition)
  - `src/frontend/parser.rs` (`parse_pattern`)
  - `src/runtime/interpreter.rs` (`pattern_matches`)

## Summary

`Pattern::Identifier(String)` is declared in the AST but `parse_pattern` never constructs it — it only accepts `Underscore`, `NumberLiteral`, `BoolLiteral`, and `StringLiteral`. Meanwhile `pattern_matches` returns `false` for `Pattern::Identifier`, so even if it were somehow produced, no arm would ever match.

This is either a planned feature (binding pattern like Rust's `x => { ... }` binding the scrutinee to `x`) that was never implemented, or leftover code that should be removed.

## Offending code

```rust
// src/frontend/ast.rs
pub enum Pattern {
  Number(f64),
  Bool(bool),
  String(String),
  Identifier(String),   // ← never produced
  Wildcard,
}
```

```rust
// src/runtime/interpreter.rs
fn pattern_matches(pattern: &Pattern, value: &Value) -> bool {
  return match pattern {
    Pattern::Wildcard            => true,
    Pattern::Number(number)      => *value == Value::Number(*number),
    Pattern::String(string)      => *value == Value::String(string.clone()),
    Pattern::Bool(boolean)       => *value == Value::Bool(*boolean),
    Pattern::Identifier(_)       => false,   // ← always false
  };
}
```

## Suggested fix

Pick one:

1. **Implement it.** Accept an `Identifier` token in `parse_pattern`, and in `pattern_matches` always match (return `true`) while also binding the scrutinee value to the named variable inside the arm body. This gives you catch-all arms with access to the matched value.
2. **Remove it.** Delete the variant from `Pattern`, the `Identifier(_)` arm in `pattern_matches`, and any other references. Keeps the code honest.

## How to verify

- Option 1: `match x { y => { println("@", y); } }` compiles and prints the value of `x`.
- Option 2: the variant is gone from `ast.rs` and `cargo check` passes.
