# Known Bugs

Index of known bugs in the Astra interpreter. Each bug lives in its own file with a checkbox at the top — toggle `[ ]` → `[x]` when fixed (GitHub, VS Code, and most Markdown viewers render these as clickable checkboxes).

> Folder was named `known-bugs` (with a dash) instead of `known bugs` to avoid spaces in paths — makes scripts, CI, and shell tooling safer. Rename if you prefer the literal name.

## Severity Legend

- 🔴 **Critical** — core feature broken or unusable
- 🟠 **Major** — feature works but with surprising behavior
- 🟡 **Minor** — cosmetic, style, or dead code

## Bug Index

| # | Severity | Bug | Status |
|---|----------|-----|--------|
| 01 | 🔴 | [Array element assignment unparseable](01-array-index-assignment-unparseable.md) | ✅ Closed |
| 02 | 🔴 | [`let x: T;` without initializer always fails](02-let-without-initializer-fails.md) | ✅ Closed |
| 03 | 🔴 | [Nested `fn` overwrites global function silently](03-nested-fn-overwrites-global.md) | ✅ Closed |
| 04 | 🟠 | [No block scoping for `let`](04-no-block-scoping.md) | ✅ Closed |
| 05 | 🟠 | [Chained method call drops writeback](05-chained-method-writeback-lost.md) | ✅ Closed |
| 06 | 🟠 | [Unclosed block comment produces misleading error](06-unclosed-block-comment.md) | ✅ Closed |
| 07 | 🟠 | [Array display is `[array]` placeholder](07-array-display-placeholder.md) | ✅ Closed |
| 08 | 🟠 | [Range with large bounds can allocate huge `Vec`](08-range-dos-large-bounds.md) | ✅ Closed|
| 09 | 🟡 | [`Pattern::Identifier` unreachable](09-pattern-identifier-unreachable.md) | ✅ Closed |
| 10 | 🟡 | [`HeaderOrderViolation` for Entry unreachable](10-header-order-violation-unreachable.md) | ✅ Closed |
| 12 | 🟡 | [Typo `insertion_after_previus`](12-typo-insertion-after-previus.md) | ✅  Closed |
