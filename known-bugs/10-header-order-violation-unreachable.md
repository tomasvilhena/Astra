# Bug 10 — `HeaderOrderViolation` for a second `entry` is unreachable

- [X] **Fixed**
- **Severity:** 🟡 Minor (dead branch)
- **Component:** Interpreter (program pre-pass)
- **File:** `src/runtime/interpreter.rs`
- **Lines:** ~63–80 (`run` / `Stmt::Entry` arm)

## Summary

Inside `run`, the `Stmt::Entry` branch checks `if seen_entry` first and returns `DuplicateEntry`, then checks `if header_closed` to return `HeaderOrderViolation`. But `header_closed` is only ever set to `true` **after** `seen_entry` has also been set (you can only close the header by executing a function declaration or a top-level statement, both of which require `seen_entry`). So any second `Entry` encountered will always trip `DuplicateEntry` first, and the `HeaderOrderViolation` branch is unreachable for `Entry`.

## Offending code

```rust
// src/runtime/interpreter.rs
Stmt::Entry { name } => {
  if seen_entry {
    return Err(InterpreterError::DuplicateEntry);
  }

  if header_closed {
    return Err(InterpreterError::HeaderOrderViolation {
      item: "entry".to_string(),
    });
  }

  seen_entry = true;
  entry_name = Some(name.clone());
},
```

## Suggested fix

Two options:

1. **Reorder the checks** so `HeaderOrderViolation` actually fires (swap the two `if`s). Unlikely to matter since `seen_entry` is always true when `header_closed` is true, but at least the branch becomes live for edge cases in future refactors.
2. **Delete the dead branch** — remove the `header_closed` check from the `Entry` arm entirely. Simpler code, same behavior.

Option 2 is preferable unless you plan to change the ordering invariants.

## How to verify

Run a file with two `entry` directives:

```astra
entry "main";
entry "again";
fn main(): void {}
```

The error should still be `DuplicateEntry`, which it already is — but the code path leading to `HeaderOrderViolation` should be gone (or demonstrably reachable).
