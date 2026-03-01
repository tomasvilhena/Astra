# Astra
Programming Language

## Remaining Implementation Checklist (With File Targets)

### Already done

- [x] Runtime module exported  
  - File: `src/runtime/mod.rs`

- [x] `Value` enum migrated to `Number` model (`Number`, `Bool`, `String`, `Array`, `Void`)  
  - File: `src/runtime/value.rs`

- [x] `Value::type_name()` helper  
  - File: `src/runtime/value.rs`

- [x] `Value::is_truthy()` helper  
  - File: `src/runtime/value.rs`

- [x] Interpreter skeleton (`Interpreter` struct, error enum, constructor)  
  - File: `src/runtime/interpreter.rs`

- [x] `eval_expr` basic literal arms (`Number`, `Bool`, `String`)  
  - File: `src/runtime/interpreter.rs`

- [x] `eval_expr` identifier lookup (local frame -> global -> undefined error)  
  - File: `src/runtime/interpreter.rs`

- [x] `eval_expr` array literal evaluation  
  - File: `src/runtime/interpreter.rs`

- [x] `eval_expr` unary evaluation (`Negative`, `Not`, `KeywordNot`)  
  - File: `src/runtime/interpreter.rs`

- [x] Frontend numeric migration (`NumberType`, `NumberLiteral`, `Expr::Number`, parser updates)  
  - Files: `src/frontend/lexer.rs`, `src/frontend/ast.rs`, `src/frontend/parser.rs`

- [x] Runtime module wired in main (`mod runtime;`)  
  - File: `src/main.rs`

---

### To implement

#### Compile blockers first
- [ ] Remove `return a;` from binary expression arm and return proper `RuntimeResult<Value>`  
  - File: `src/runtime/interpreter.rs`
- [ ] Ensure `Expr::Call`, `Expr::Member`, `Expr::Index` arms return `RuntimeResult<Value>` (temporary explicit error is fine)  
  - File: `src/runtime/interpreter.rs`
- [ ] Fix fallback arm in `eval_binary` (`op/left/right` placeholders must be valid string values)  
  - File: `src/runtime/interpreter.rs`
- [ ] Clean unused imports in runtime files  
  - Files: `src/runtime/interpreter.rs`, `src/runtime/value.rs`

#### Binary evaluator completion
- [ ] Route `Expr::Binary` to `self.eval_binary(op, left, right)` directly  
  - File: `src/runtime/interpreter.rs`
- [ ] Complete arithmetic behavior in `eval_binary` (`+ - * / % ^`) with type checks  
  - File: `src/runtime/interpreter.rs`
- [ ] Add divide/modulo-by-zero runtime errors  
  - File: `src/runtime/interpreter.rs`
- [ ] Add comparisons (`== != < <= > >=`)  
  - File: `src/runtime/interpreter.rs`
- [ ] Add logical operators (`&& || AND OR`)  
  - File: `src/runtime/interpreter.rs`
- [ ] Decide/implement assignment-family handling (`= += -= *= /= %= ^=`)  
  - File: `src/runtime/interpreter.rs`
- [ ] Decide/implement ranges (`..`, `..=`)  
  - File: `src/runtime/interpreter.rs`

#### Remaining expression arms
- [ ] Implement `Expr::Index` runtime behavior (array/string indexing + bounds checks)  
  - File: `src/runtime/interpreter.rs`
- [ ] Implement `Expr::Member` behavior (or explicit unsupported error for now)  
  - File: `src/runtime/interpreter.rs`
- [ ] Implement `Expr::Call` behavior (built-ins first, then user functions)  
  - File: `src/runtime/interpreter.rs`

#### Statement executor
- [ ] Add `exec_stmt(&Stmt)` skeleton  
  - File: `src/runtime/interpreter.rs`
- [ ] Implement `Let`, `ExprStmt`, `Print`/`Println` first  
  - File: `src/runtime/interpreter.rs`
- [ ] Implement control flow statements (`If`, `While`, `Repeat`, `Match`, `Try`)  
  - File: `src/runtime/interpreter.rs`
- [ ] Implement `Break`, `Continue`, `Return` propagation via internal control-flow enum  
  - File: `src/runtime/interpreter.rs`

#### Function system
- [ ] Register function declarations from AST  
  - File: `src/runtime/interpreter.rs`
- [ ] Push/pop call frames on call  
  - File: `src/runtime/interpreter.rs`
- [ ] Bind arguments to parameters + arity checks  
  - File: `src/runtime/interpreter.rs`

#### Main integration
- [ ] Execute interpreter after parse success (instead of only printing AST)  
  - File: `src/main.rs`
- [ ] Wrap runtime errors with `miette::Report` in main flow  
  - File: `src/main.rs`

#### Docs/sample consistency
- [ ] Keep demo samples aligned with currently implemented runtime features (avoid showcasing unsupported runtime behavior yet)  
  - Files: `src/main.rs`, `docs/SYNTAX/example1.astra`