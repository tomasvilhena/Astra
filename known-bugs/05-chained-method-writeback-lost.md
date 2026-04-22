# Bug 05 — Chained method calls drop the writeback of intermediate mutations

- [ ] **Fixed**
- **Severity:** 🟠 Major
- **Component:** Interpreter (method dispatch)
- **File:** `src/runtime/interpreter.rs`
- **Lines:** ~375–400 (`Expr::Call` with `Expr::Member` callee)

## Summary

Methods like `push`, `pop`, `reverse`, `clear`, `flatten` are implemented as in-place mutations on a `&mut Value`. The caller clones the value out of the variable, runs the method, and writes the mutated value back — **but only when the receiver is a direct `Expr::Identifier`**. If the receiver is any other expression (another call, an index, etc.), the `_` arm is taken and the mutation is thrown away.

That means `arr.reverse().push(99)` reverses `arr` in place (identifier path) but the subsequent `push` runs on a temporary and its result is discarded.

## Reproduction

```astra
entry "main";

fn main(): void {
  let arr: array = [1, 2, 3];
  arr.reverse().push(99);
  println("after @", arr.length());   // prints: after 3   (push was lost)
}
```

Expected: `after 4`. Actual: `after 3`.

## Offending code

```rust
// src/runtime/interpreter.rs
match object.as_ref() {
  Expr::Identifier(name) => {
    let mut receiver = self.eval_expr(object)?;
    let result = methods::call_method(&mut receiver, property, evaluated_args)?;
    self.assign_var(name, receiver)?;  // ← writeback
    Ok(result)
  },
  _ => {
    let mut receiver = self.eval_expr(object)?;
    let result = methods::call_method(&mut receiver, property, evaluated_args)?;
    Ok(result)                          // ← no writeback
  }
}
```

## Suggested fix

Two approaches:

1. **Make mutating methods return a fresh value** instead of mutating in place. Then the chain works naturally as long as the user reassigns the result (`arr = arr.reverse().push(99);`).
2. **Propagate writeback for the root receiver** of a chain: walk the callee's object chain to find the leftmost identifier/index, and after all calls finish, write the final mutated state back to that root. More complex; matches JS-style semantics.

Given the language's current style, option 1 is cleaner. Either way, document the rule so users are not surprised.

## How to verify

The reproduction should print `after 4` once the fix is applied (or consistently error / require reassignment for chained mutations, per whichever approach is chosen).
