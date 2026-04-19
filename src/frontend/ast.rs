#[derive(Debug, Clone)]
pub enum Expr
{
  Number(f64),
  Bool(bool),
  Identifier(String),
  String(String),
  ArrayLiteral(Vec<Expr>),

  Unary
  {
    op: UnaryOperator,
    expr: Box<Expr>,
  },

  Binary
  {
    left: Box<Expr>,
    op: BinaryOperator,
    right: Box<Expr>,
  },

  Call
  {
    callee: Box<Expr>,
    args: Vec<Expr>,
  },

  Member
  {
    object: Box<Expr>,
    property: String,
  },

  Index
  {
    object: Box<Expr>,
    index: Box<Expr>,
  },
}

#[derive(Debug, Clone)]
pub enum Pattern
{
  Number(f64),
  Bool(bool),
  String(String),
  Identifier(String),
  Wildcard,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator
{
  Plus,         // +
  Minus,        // -
  Star,         // *
  Slash,        // /
  Percent,      // %
  Caret,        // ^

  // Comparison
  Equal,        // ==
  NotEqual,     // !=
  Greater,      // >
  GreaterEqual, // >=
  Less,         // <
  LessEqual,    // <=

  // Logical
  And,        // &&
  Or,         // ||
  KeywordAnd, // AND
  KeywordOr,  // OR

  // Ranges
  RangeExclusive, // ..
  RangeInclusive, // ..=
}

#[derive(Debug, Clone)]
pub enum UnaryOperator
{
  Negative,   // -
  Not,        // !
  KeywordNot, // NOT
}

#[derive(Debug, Clone)]
pub enum AssignOperator
{
  Assign,       // =
  PlusAssign,   // +=
  MinusAssign,  // -=
  StarAssign,   // *=
  SlashAssign,  // /=
  PercentAssign,// %=
  CaretAssign,  // ^=
}

#[derive(Debug, Clone)]
pub enum AssignTarget
{
  Variable(String),
  Index {object: Expr, index: Expr},
}

#[derive(Debug, Clone)]
pub enum Stmt
{
  Let
  {
    name: String,
    var_type: Option<String>,
    value: Option<Expr>,
  },

  Function
  {
    name: String,
    params: Vec<(String, String)>,
    return_type: Option<String>,
    body: Vec<Stmt>,
  },

  ExprStmt(Expr),

  Print
  {
    text_string: Option<String>,
    args: Vec<Expr>,
    new_line: bool,
  },

  If
  {
    condition: Expr,
    then_body: Vec<Stmt>,
    else_body: Option<Vec<Stmt>>,
  },

  Return
  {
    value: Option<Expr>,
  },

  While
  {
    condition: Expr,
    body: Vec<Stmt>,
  },

  Repeat
  {
    count: Expr,
    index_name: Option<String>,
    body: Vec<Stmt>
  },

  Break,
  Continue,

  Match
  {
    value: Expr,
    arms: Vec<(Pattern, Vec<Stmt>)>,
  },

  Entry
  {
    name: String,
  },

  Try
  {
    try_body: Vec<Stmt>,
    on_body: Vec<Stmt>,
  },

  Assign
  {
    target: AssignTarget,
    op: AssignOperator,
    value: Expr
  }
}
