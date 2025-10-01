#[derive(Debug, Clone)]
pub enum Expr
{
    Integer(i64),
    Float(f64),
    Bool(bool),
    Identifier(String),

    Unary {
      op: UnaryOperator,
      expr: Box<Expr>,
    },

    Binary {
      left: Box<Expr>,
      op: BinaryOperator,
      right: Box<Expr>,
    },
    // add more later
}

#[derive(Debug, Clone)]
pub enum BinaryOperator
{
  Assign,         // =
  Plus,           // +
  Minus,          // -
  Star,           // *
  Slash,          // /
  Percent,        // %
  Caret,          // ^
  PlusEqual,      // +=
  MinusEqual,     // -=
  StarEqual,      // *=
  SlashEqual,     // /=
  PercentEqual,   // %=
  CaretEqual,     // ^=

  // Comparison
  Equal,          // ==
  NotEqual,       // !=
  Greater,        // >
  GreaterEqual,   // >=
  Less,           // <
  LessEqual,      // <=

  // Logical
  And,            // &&
  Or,             // ||
  Not,            // !
  KeywordAnd,     // AND
  KeywordOr,      // OR
  KeywordNot,     // NOT

  // Ranges
  RangeExclusive, // ..
  RangeInclusive, // ..=      let names: array<int> = 1..5 [1, 2, 3, 4]

}

#[derive(Debug, Clone)]
pub enum UnaryOperator
{
  Negative,       // -
  Not,            // !
  KeywordNot,     // NOT
}

