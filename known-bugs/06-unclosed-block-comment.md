# Bug 06 — Unclosed `/* */` block comment produces a misleading parser error

- [ ] **Fixed**
- **Severity:** 🟠 Major
- **Component:** Lexer
- **File:** `src/frontend/lexer.rs`
- **Lines:** ~203–219 (`skip_whitespace_and_comments`, block-comment branch)

## Summary

The lexer already has an `UnterminatedString` diagnostic for unclosed `"..."`. There is no equivalent for unclosed `/* ... */` — the block-comment loop simply exits when `peek()` returns `None`. The lexer then emits `EndOfFile` and the parser reports an `UnexpectedToken: EndOfFile` far from the actual opening `/*`, pointing the user at a confusing line.

## Reproduction

```astra
entry "main";
fn main(): void {
  let x: number = 5;
  /* unclosed comment
```

Output:

```
Error: parser::unexpected_token
  × Unexpected token: expected [NumberLiteral, ...], found EndOfFile
```

The real problem — an unclosed block comment — is never mentioned, and the reported span is at the end of the file, not at the `/*`.

## Offending code

```rust
// src/frontend/lexer.rs
Some('*') => {
  // block comment
  self.advance();
  self.advance();
  while let Some(c) = self.peek() {
    if c == '*' && self.next_char(1) == Some('/') {
      self.advance();
      self.advance();
      break;
    }
    self.advance();
  }
  continue;
},
```

The inner loop silently completes when the file ends.

## Suggested fix

Remember the position of the opening `/*`, and when the inner loop exits without finding `*/`, emit a new `LexError::UnterminatedBlockComment { span }` pointing at the opening token — mirroring how `UnterminatedString` works.

```rust
let open_span = start_pos; // capture before the inner loop
let mut closed = false;
while let Some(c) = self.peek() {
  if c == '*' && self.next_char(1) == Some('/') {
    self.advance(); self.advance();
    closed = true;
    break;
  }
  self.advance();
}
if !closed {
  return Err(LexError::UnterminatedBlockComment {
    span: SourceSpan::new(open_span.into(), 2usize.into()),
  });
}
```

## How to verify

The reproduction should produce a diagnostic labelled `lexer::unterminated_block_comment` (or similar), with the span pointing at the opening `/*`.
