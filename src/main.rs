mod frontend; // or `pub mod frontend;` in lib.rs
use frontend::parser::Parser;
use frontend::lexer::Lexer;

fn main()
{
  let source = r#"
        entry main;
// ==============================
// Astra Programming Language Syntax Demo
// ==============================

// Single-line comment

/* Multi-line comment */
// ------------------------------
// #include
// ------------------------------
// Allows importing libraries.
// Libraries are written in Astra syntax as well.
// A library cannot define an entry point (no main). #include "std.astra" #include "math.astra"

// ------------------------------
// Functions
// ------------------------------
// Functions are declared with the fn keyword.
// Syntax: fn name(params): return_type { ... }
//
// Return type can be any type (int, string, etc.) or void if nothing is returned.

fn add(a: int, b: int): int
{
  return a + b;
  // explicit return
}

// Default parameter values are supported using = inside the parameter list.
fn greet(name: string = "World"): void
{
  println("Hello, {}", name);
}
// ------------------------------
// Arrays
// ------------------------------
fn array_demo(): void
{
  let nums: array<int> = [1, 2];

  nums.push_end(3); // adds value to the end of an array

  nums.push_start(0); // adds value to the beginning of the array

  nums.remove(0); // removes the number at the specified position

  let more: array<int> = [4, 5];

  nums.merge(more); // appends the "more" array to the end of the nums array

  println("Array size: {}", nums.length()); // provides the length of the array
}

// ------------------------------
// Strings
// ------------------------------
fn string_demo(): void
{
  let text: string = "hello";
  let more: string = " world";

  text.uppercase(); // makes text string uppercase
  text.lowercase(); // makes text string lowercase

  text.expand(more); // adds the "more" string to the "text" string

  text = text.trim(); // removes whitespace at beginning and end

  if (text.contains("world")) // returns true if the substring exists
  {
    println("Text: {}", text);
  }
}

// ------------------------------
// Input and Matching
// ------------------------------
fn input_demo(): void
{
  println("Enter your age: ");

  let input: string = read(); // reads input from the user
  let age: int = input.parse_int(); // converts the string to an integer

  // Match is used for branching on values
  match age
  {
    18 => {
      println("You are 18!");
    }

    21 => {
      println("You are 21!");
    }

    _ => {
      println("Age: {}", age);
    } // "_" or "default" matches anything else

  }
}

// ------------------------------
// Loops
// ------------------------------
fn loops_demo(): void
{
  // for loop with ranges
  for i: int in 0..5 // 0..5 → 0,1,2,3,4 (end exclusive)
  {
    print("{}", i);
  }

  for i: int in 0..=5 // 0..=5 → 0,1,2,3,4,5 (end inclusive)
  {
    print("{}", i);
  }

  // Iterating arrays by index
  let book_collection: array<string> = ["lord of the rings", "hobbit"];
  for i: int in book_collection
  {
    print("{}", book_collection[i]);
  }

  // Iterating arrays by value
  for book: string in book_collection
  {
    print("{}", book);
  }

  // while loop
  let x: int = 0;

  while (x < 3)
  {
    println("While: {}", x);
    x += 1;

    if (x == 2)
    {
      continue;
    } // skip rest of this iteration
  }

  // do-while loop
  let y: int = 0;
  do
  {
    println("Do-while: {}", y);
    y += 1;

    if (y == 1)
    {
       break; // exit loop early
    }
  } while (y < 2);
}

// ------------------------------
// Operators
// ------------------------------
fn operators_demo(): void
{
  let a: int = 10;
  let b: int = 3;

  // Arithmetic
  println("Sum: {}", a + b);
  println("Sub: {}", a - b);
  println("Mul: {}", a * b);
  println("Div: {}", a / b);
  println("Mod: {}", a % b);
  println("Exponentiation: {}", a*);

  // example custom operator
  // Boolean logic
  let flag1: bool = (a > b) && (b < 5);
  let flag2: bool = (a > b) AND (b < 5);

  // keyword alternative
  let flag3: bool = (a > b) || (b < 5);
  let flag4: bool = (a > b) OR (b < 5);
  println("Flag: {}", flag1); }

  // ------------------------------
  // Error Handling (TRY/ON)
  // ------------------------------
  // try { ... } on { ... } executes the try block,
  // and if an error occurs, runs the on block.
  // It can be used for statements (no return value required).

  fn error_handling_demo(): void
  {
    try
    {
      println("Trying risky code...");
      let x: int = "oops".parse_int();
      // will fail
    } on
    {
      println("Recovering from error");
    }
  }

  // ------------------------------
  // Debugging
  // ------------------------------
  // The debug function prints variable names and values.
  // It can also evaluate expressions and show the result.

  fn debug_demo(): void
  {
    let number: int = 42;
    let text: string = "astra";

    debug(number, text);
    // prints both variable name and value debug(number == 42);
    // prints "number == 42: true"
  }

  // ------------------------------
  // Entry Point
  // ------------------------------

  fn main(): void
  {
    println("=== Astra Language Syntax Demo ===");

    greet(); // default parameter ("World")

    greet("Astra"); // explicit argument

    println("Add: {}", add(5, 7));
    array_demo();

    string_demo();

    input_demo();

    loops_demo();

    operators_demo();

    error_handling_demo();

    debug_demo();

    clear(); // clears the screen

    println("Done.");
  }

    "#;

  let mut lexer = Lexer::new(source);
  let tokens = lexer.tokenize();

  // for token in tokens
  // {
  //   println!("{:?} => {}", token.token_kind, token.lexed_value);
  // }

  let source = "not !true";
  let mut lexer = Lexer::new(source);
  let tokens = lexer.tokenize();

  let mut parser = Parser::new(tokens);
  let ast = parser.parse_expr(0);

  println!("{:?}", ast);

}
