# Bug 03 — Nested `fn` declarations silently overwrite global functions

- [ ] **Fixed**
- **Severity:** 🔴 Critical
- **Component:** Interpreter (function registry / scoping)
- **File:** `src/runtime/interpreter.rs`
- **Lines:** ~1109–1129 (`exec_stmt` / `Stmt::Function` arm)

## Summary

`Interpreter::run` registers top-level functions into a single shared `FxHashMap<String, FunctionDef>` and errors on duplicates. But when a `Stmt::Function` is executed inside another function's body, `exec_stmt` inserts into the **same** map with no duplicate check, so:

1. Nested function declarations are global, not lexically scoped.
2. A nested `fn foo` silently replaces an outer `fn foo`.
3. The replacement persists after the outer function returns.

## Reproduction

```astra
entry "main";

fn outer(): void {
  println("outer");
}

fn main(): void {
  fn outer(): void {
    println("shadow");
  }
  outer();
}
```

Output: `shadow` (the inner definition permanently replaces the outer one).

## Offending code

```rust
// src/runtime/interpreter.rs, Stmt::Function arm
self.functions.insert(
  name.clone(),
  FunctionDef { /* ... */ },
);
```

No `contains_key` check, no scope stack.

Compare with the top-level registration in `run`:

```rust
if self.functions.contains_key(name) {
  return Err(InterpreterError::DuplicateFunction { name: name.clone() });
}
```

## Suggested fix

Two options:

1. **Forbid nested `fn`** — error at parse time (or at `exec_stmt`) if a function declaration appears anywhere except the top level.
2. **Proper lexical scoping** — introduce a function-scope stack (push on call, pop on return) and resolve names against it. More work, but it is the only way to let users actually define helper functions inside functions.

At minimum, add the same `contains_key` check that `run` has, so nested redefinitions error loudly instead of silently shadowing.

## How to verify

The reproduction should either fail at parse/exec time with a clear error, or — if lexical scoping is added — the outer `outer()` should remain callable after `main` finishes redefining a local `outer`.
