#[derive(Debug, Clone)]
pub enum Expr
{
    Integer(i64),
    Float(f64),
    Bool(bool),
    Identifier(String),
    String(String),

    Unary {
      op: UnaryOperator,
      expr: Box<Expr>,
    },

    Binary {
      left: Box<Expr>,
      op: BinaryOperator,
      right: Box<Expr>,
    },
    
    Call {
      callee: Box<Expr>,
      args: Vec<Expr>,
    }
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
  RangeInclusive, // ..=
}

#[derive(Debug, Clone)]
pub enum UnaryOperator
{
  Negative,       // -
  Not,            // !
  KeywordNot,     // NOT
}

#[derive(Debug, Clone)]
pub enum Stmt //statments 
{
  Let {
    name: String,
    var_type: Option<String>, 
    value: Option<Expr>
  },

  Function {
    name: String,
    params: Vec<(String, String)>,
    return_type: Option<String>,
    body: Vec<Stmt>,
  },

  ExprStmt(Expr),

  Print {
    text_string: Option<String>,
    args: Vec<Expr>,
    new_line: bool,
  },

  If {
    condition: Expr,
    then_body: Vec<Stmt>, 
    else_body: Option<Vec<Stmt>>, 
  },

  Return {
    value: Option<Expr>,
  },

  While
  {
    condition: Expr,
    body: Vec<Stmt>, 
  },
}

