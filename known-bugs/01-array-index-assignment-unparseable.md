# Bug 01 — Array element assignment (`arr[i] = v`) is unparseable

- [ ] **Fixed**
- **Severity:** 🔴 Critical
- **Component:** Parser
- **File:** `src/frontend/parser.rs`
- **Lines:** ~980–997 (`parse_stmt`)

## Summary

The AST defines `AssignTarget::Index { object, index }`, the interpreter implements indexed assignment in `apply_assign_op`, and `parse_assign_stmt` correctly builds the target when it is called. But `parse_stmt` only **dispatches** to `parse_assign_stmt` when the current `Identifier` token is **directly followed** by an assignment operator. If a `[` sits between the identifier and the `=`, the statement falls through to `parse_expr`, which produces an `Expr::Index`, and then `expect(TokenKind::Semicolon)` fails when it sees `=`.

Feature is coded but unreachable.

## Reproduction

```astra
entry "main";

fn main(): void {
  let arr: array = [1, 2, 3];
  arr[0] = 99;
  println("@", arr[0]);
}
```

Output:

```
Error: parser::missing_token

  × Missing `Semicolon`
   ╭─[...:4:9]
 4 │   arr[0] = 99;
   ·         ▲
   ·         ╰── insert here
```

The shipped example `docs/syntax/tic_tac_toe.astra:40` (`board[i] = player;`) cannot run because of this.

## Offending dispatch

```rust
// src/frontend/parser.rs (parse_stmt, simplified)
if token.token_kind == TokenKind::Identifier {
  if let Some(next) = self.tokens.get(self.position + 1) {
    match next.token_kind {
      TokenKind::Assign
      | TokenKind::PlusEqual
      | /* ... */ => return self.parse_assign_stmt(),
      _ => {}
    }
  }
}
```

This only fires for `name = ...`. For `name[i] = ...` the lookahead sees `[` and bails out.

## Suggested fix

Speculatively parse a primary + postfix, then check whether the next token is an assignment operator. If yes, build the `AssignTarget` from the parsed expression; if no, treat it as an expression statement.

Sketch:

```rust
// In parse_stmt, for the fallback branch:
let save = self.position;
let expr = self.parse_expr(0)?;
if let Some(tok) = self.peek() {
  if matches!(tok.token_kind,
    TokenKind::Assign | TokenKind::PlusEqual | TokenKind::MinusEqual
    | TokenKind::StarEqual | TokenKind::SlashEqual
    | TokenKind::PercentEqual | TokenKind::CaretEqual)
  {
    return self.parse_assign_tail(expr); // uses the already-parsed LHS
  }
}
self.expect(TokenKind::Semicolon)?;
Ok(Stmt::ExprStmt(expr))
```

## How to verify

Running the reproduction should succeed and print `99`.
