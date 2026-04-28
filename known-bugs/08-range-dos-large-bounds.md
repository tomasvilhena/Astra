# Bug 08 — Ranges with large bounds can allocate enormous `Vec`s (and silently saturate f64→i32)

- [X] **Fixed**
- **Severity:** 🟠 Major
- **Component:** Interpreter (range evaluation)
- **File:** `src/runtime/interpreter.rs`
- **Lines:** ~783–822 (`eval_binary` / `RangeExclusive` / `RangeInclusive`)

## Summary

Range evaluation does two problematic things:

1. `left as i32 ..= right as i32` — casting an `f64` to `i32` **saturates** in Rust. A value of `1e18` becomes `i32::MAX` silently with no error.
2. The resulting `Vec<Value>` is allocated eagerly. A range like `0..1_000_000_000` tries to allocate ~1B `Value` entries (tens of GB), causing an OOM abort or extreme slowdown.

Both problems share the same root: ranges are materialized as arrays instead of being iterator-like, and there are no size guards.

## Reproduction

```astra
entry "main";

fn main(): void {
  let big: array = 0..1000000000;   // tries to allocate ~1B elements
  println("@", big.length());
}
```

Also:

```astra
let r: array = 0..1e18;   // silently saturates to 0..i32::MAX, no error
```

## Offending code

```rust
// src/runtime/interpreter.rs
for value in left as i32..=right as i32 {
  if value == right as i32 && !inclusive { break; }
  range.push(Value::Number(value as f64));
}
```

- `as i32` saturates without diagnostic.
- The final `Vec` is always fully materialized.

## Suggested fix

Two layers:

1. **Validate bounds before casting**: reject values outside `i32::MIN..=i32::MAX` (or whatever width you pick) with a clear error instead of saturating. Consider widening to `i64` while you are there.
2. **Guard the element count**: compute `count = right - left (+ 1)` and reject if it exceeds a sensible cap (e.g. 10 million) — or introduce a lazy `Range` value kind that iterates without materializing.

Minimal quick fix:

```rust
if !left.is_finite() || !right.is_finite()
   || left  < i64::MIN as f64 || left  > i64::MAX as f64
   || right < i64::MIN as f64 || right > i64::MAX as f64 {
  return Err(/* out-of-range error */);
}

let count = (right - left).abs() as u64 + if inclusive { 1 } else { 0 };
if count > 10_000_000 {
  return Err(/* range-too-large error */);
}
```

## How to verify

The first reproduction should error quickly instead of hanging / OOMing. The second should error on the out-of-range bound rather than silently clamping.
