# Bug 07 — Printing an array shows the literal string `[array]`

- [X] **Fixed**
- **Severity:** 🟠 Major
- **Component:** Runtime value formatting
- **File:** `src/runtime/value.rs`
- **Line:** 47

## Summary

`Value::Array`'s `Display` implementation writes the literal string `[array]` instead of the array contents. Since `print` / `println` rely on `Display`, users cannot print arrays without manually iterating. This makes debugging much harder and contradicts the apparent intent (other value kinds print their content).

## Reproduction

```astra
entry "main";

fn main(): void {
  let a: array = [1, 2, 3];
  println("@", a);   // prints: [array]
}
```

## Offending code

```rust
// src/runtime/value.rs
impl std::fmt::Display for Value {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Value::Number(number)  => write!(formatter, "{}", number),
      Value::Bool(boolean)   => write!(formatter, "{}", boolean),
      Value::String(string)  => write!(formatter, "{}", string),
      Value::Array(_)        => write!(formatter, "[array]"),
      Value::Void            => write!(formatter, ""),
    }
  }
}
```

## Suggested fix

Format each element and wrap in brackets. Strings should be quoted so `[1]` and `["1"]` are distinguishable:

```rust
Value::Array(items) => {
  write!(formatter, "[")?;
  for (i, item) in items.iter().enumerate() {
    if i > 0 { write!(formatter, ", ")?; }
    match item {
      Value::String(s) => write!(formatter, "\"{}\"", s)?,
      other            => write!(formatter, "{}", other)?,
    }
  }
  write!(formatter, "]")
}
```

This recurses naturally for nested arrays.

## How to verify

The reproduction should print `[1, 2, 3]`. `println("@", [[1, 2], [3]]);` should print `[[1, 2], [3]]`.
