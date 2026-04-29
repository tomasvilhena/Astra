<div align="center">

<img src="assets/astra.jpeg" alt="Astra" width="240" />

# Astra

*A small, friendly programming language — written in Rust.*

</div>

```rust
// TODO: insert funny quote here
```

---

## About

Astra is a tiny interpreted language I built as a school project. I made it intentionally **basic**: a handful of types, a few control-flow constructs, and a clean syntax that should feel familiar if you have ever read C, Rust, or JavaScript. I wrote the interpreter in Rust and it ships as a single CLI binary.

This isn't a production language. It's a learning project for me — small enough to read end-to-end, useful enough to write little programs in.

---

## Install

I distribute Astra through Cargo. If you have a Rust toolchain, one command is all you need:

```bash
cargo install --git https://github.com/tomasvilhena/Astra
```

The binary lands in `~/.cargo/bin/astra` (already on your `PATH` after a normal Rust install). After it finishes, run:

```bash
astra --help
```

…and you should see the CLI.

---

## Hello, world

```astra
entry "main";

fn main(): void {
  println("Hello, world!");
}
```

Save it as `hello.astra`, then:

```bash
astra run hello.astra
```

Every Astra program starts with an `entry "<function_name>";` directive. The interpreter looks for that function and runs it.

---

## A flavor of the syntax

```astra
entry "main";

fn main(): void {
  let nums: array = [3, 1, 4, 1, 5, 9, 2, 6];
  nums.reverse().push(0);

  let total: number = 0;
  repeat nums.length() as i {
    total += nums[i];
  }

  println("sum is @", total);

  match total {
    0     => { println("nothing"); }
    31    => { println("nice"); }
    _     => { println("something else"); }
  }
}
```

A few notes on what I picked:
- Variables are declared with `let name: type = value;` — types are explicit.
- Functions use `fn name(args): return_type { ... }`.
- `print` has no formatter; `println` uses `@` as the placeholder for any value.
- Mutating array methods like `push`, `pop`, `reverse` chain naturally and write back to the variable.
- `match` arms support number, string, and bool literal patterns plus `_` as a wildcard.

---

## FizzBuzz, the canonical example

```astra
entry "fizz_buzz";

fn fizz_buzz(): void {
  let value: number = read("Insert a number: ").to_number();

  repeat value as i {
    if ((i + 1) % 3 == 0 && (i + 1) % 5 == 0) {
      print("FizzBuzz ");
      continue;
    } else if ((i + 1) % 3 == 0) {
      print("Fizz ");
      continue;
    } else if ((i + 1) % 5 == 0) {
      print("Buzz ");
      continue;
    }

    print("@ ", (i + 1));
  }
}
```

More examples live in [`docs/SYNTAX/`](docs/SYNTAX/).

---

## Types

| Type    | Example                       |
|---------|-------------------------------|
| `number`| `42`, `3.14`, `-7`            |
| `bool`  | `true`, `false`               |
| `string`| `"hello"`                     |
| `array` | `[1, 2, 3]`, `["a", "b"]`     |
| `void`  | functions that return nothing |

---

## Keywords

| Category            | Keywords                                                  |
|---------------------|-----------------------------------------------------------|
| Program             | `entry`, `fn`, `return`                                   |
| Declaration         | `let`                                                     |
| Types               | `number`, `bool`, `string`, `array`, `void`               |
| Control flow        | `if`, `else`, `while`, `repeat`, `as`, `break`, `continue`|
| Pattern matching    | `match`, `_`                                              |
| Error handling      | `try`, `on`                                               |
| Logical (symbols)   | `&&`, `\|\|`, `!`                                          |
| Logical (words)     | `AND`, `OR`, `NOT`                                        |
| I/O                 | `print`, `println`, `read`                                |
| Literals            | `true`, `false`                                           |

---

## Operators

| Kind         | Operators                                              |
|--------------|--------------------------------------------------------|
| Arithmetic   | `+`  `-`  `*`  `/`  `%`  `^`                           |
| Comparison   | `==`  `!=`  `<`  `<=`  `>`  `>=`                       |
| Assignment   | `=`  `+=`  `-=`  `*=`  `/=`  `%=`  `^=`                |
| Logical      | `&&`  `\|\|`  `!`  `AND`  `OR`  `NOT`                  |
| Range        | `..` (exclusive)  `..=` (inclusive)                    |
| Member/Index | `obj.method()`  `arr[i]`                               |

---

## CLI

```bash
astra run <file>          # run a program
astra tokens <file>       # dump the token stream (for debugging the lexer)
astra <command> --time    # also print how long each phase took
```

---

## Building from source

If you'd rather not install globally:

```bash
git clone https://github.com/tomasvilhena/Astra
cd Astra
cargo build --release
./target/release/astra run docs/SYNTAX/hello_world.astra
```

---

## Project status

Astra is my school project. I hand-wrote the interpreter, parser, and lexer in safe Rust. There's no JIT, no module system, and I don't plan to add one — I want it to stay small on purpose. I track bug fixes in [`known-bugs/`](known-bugs/) and follow my own issue → fix → verify cycle.
