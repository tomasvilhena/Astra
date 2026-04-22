# Bug 11 — Bitwise `|` used on `bool` instead of logical `||`

- [ ] **Fixed**
- **Severity:** 🟡 Minor (style / readability)
- **Component:** Interpreter
- **File:** `src/runtime/interpreter.rs`
- **Lines:** ~223–233 (`eval_expr` / `Expr::Binary` short-circuit section)

## Summary

Short-circuit evaluation for `And` / `Or` checks the operator with two `matches!` calls combined using a bitwise `|`:

```rust
if matches!(op, &BinaryOperator::And) | matches!(op, &BinaryOperator::KeywordAnd) { /* ... */ }
if matches!(op, &BinaryOperator::Or)  | matches!(op, &BinaryOperator::KeywordOr)  { /* ... */ }
```

`|` on `bool` is valid Rust and produces the same boolean result here (both sides are already evaluated — there is no short-circuit, but `matches!` has no side effects). So this works, but it reads incorrectly and will eventually mislead someone.

## Suggested fix

Use `||`:

```rust
if matches!(op, BinaryOperator::And | BinaryOperator::KeywordAnd) { /* ... */ }
if matches!(op, BinaryOperator::Or  | BinaryOperator::KeywordOr)  { /* ... */ }
```

Note: the `|` inside `matches!` is a **pattern alternative**, not a bitwise op, and is idiomatic. Both changes make the code both more concise and more correct-looking.

## How to verify

No behavior change expected. `cargo clippy -- -W clippy::needless_bitwise_bool` should no longer warn (if you enable it), and the rewrite should produce identical output for all existing tests.
