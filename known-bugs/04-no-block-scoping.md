# Bug 04 — `let` bindings have no block scoping

- [ ] **Fixed**
- **Severity:** 🟠 Major
- **Component:** Interpreter (variable scope)
- **File:** `src/runtime/interpreter.rs`
- **Lines:** ~1180–1189 (`set_var`), plus every `exec_block` call site

## Summary

`set_var` only distinguishes between "inside a call frame" and "global". There is no notion of a block scope, so any `let x` inside `if`, `while`, `repeat`, `match`, or `try` bodies goes into the outer call frame's `locals` and persists after the block ends.

The same issue makes `repeat N as i { ... }` leak `i` into the enclosing scope after the loop finishes (at the value `N - 1`).

## Reproduction

```astra
entry "main";

fn main(): void {
  if (true) {
    let x: number = 5;
  }
  println("@", x);   // prints 5 — x should not be in scope here
}
```

## Offending code

```rust
// src/runtime/interpreter.rs
fn set_var(&mut self, name: String, value: Value) {
  if let Some(frame) = self.call_stack.last_mut() {
    frame.locals.insert(name, value);
  } else {
    self.global.insert(name, value);
  }
}
```

Blocks never push or pop a scope, so `locals` accumulates across every nested block in the current function.

## Suggested fix

Replace `CallFrame { locals: FxHashMap<String, Value> }` with a stack of scopes (e.g. `Vec<FxHashMap<String, Value>>`). Entering a block pushes an empty scope; leaving pops it. `get_var` / `assign_var` walk the stack from innermost to outermost; `set_var` (for `let`) always inserts into the top scope.

Apply in every place that calls `exec_block` for a body: `If`, `While`, `Repeat`, `Match` arms, `Try`/`On`, function body.

## How to verify

The reproduction should error with `Undefined variable 'x'` at the `println` line. `repeat 3 as i { }` followed by `println("@", i);` should likewise error.
