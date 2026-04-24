# Bug 12 — Typo: `insertion_after_previus` (and one other)

- [X] **Fixed**
- **Severity:** 🟡 Minor (cosmetic)
- **Component:** Parser
- **File:** `src/frontend/parser.rs`

## Summary

Two small typos that should be cleaned up:

- `insertion_after_previus` → `insertion_after_previous` (`parser.rs:70`).
- `"assignemnt Operator"` → `"assignment operator"` (`parser.rs:923`, inside the string passed to `peek_or_eof`). This string surfaces in the error message shown to the user, so the typo is user-visible.

## Offending code

```rust
// src/frontend/parser.rs
fn insertion_after_previus(&self) -> SourceSpan { /* ... */ }
```

```rust
let left_span = self.peek_or_eof("assignemnt Operator")?.span;
```

## Suggested fix

Rename the function to `insertion_after_previous` and update every call site. Correct the user-facing string to `"assignment operator"`.

## How to verify

`cargo check` passes; grepping for `previus` and `assignemnt` (and the capitalized `Operator`) should return nothing in `src/`.
