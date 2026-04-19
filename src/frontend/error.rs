use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use super::lexer::TokenKind;

#[derive(Debug, Error, Diagnostic)]
pub enum LexError
{
  #[error("Unexpected character `{ch}`")]
  #[diagnostic(
    code(lexer::unexpected_character),
    help("Remove or replace this character — it is not part of the Astra language")
  )]
  UnexpectedCharacter
  {
    ch: char,
    #[label("this character is not valid here")]
    span: SourceSpan,
  },

  #[error("Unterminated string literal")]
  #[diagnostic(
    code(lexer::unterminated_string),
    help("Add a closing `\"` to end the string")
  )]
  UnterminatedString
  {
    #[label("string opens here but is never closed")]
    span: SourceSpan,
  },
}

pub type LexResult<T> = Result<T, LexError>;

#[derive(Error, Diagnostic, Debug)]
pub enum ParseError
{
  #[error("Unexpected token: expected {expected:?}, found {found:?}")]
  #[diagnostic(
    code(parser::unexpected_token),
    help("Check the syntax around this location")
  )]
  UnexpectedToken
  {
    expected: Vec<TokenKind>,
    found: TokenKind,
    #[label("unexpected token here")]
    span: SourceSpan,
  },

  #[error("Missing `{expected:?}`")]
  #[diagnostic(
    code(parser::missing_token),
    help("Insert the missing token at the indicated position")
  )]
  MissingToken
  {
    expected: TokenKind,
    #[label("insert here")]
    span: SourceSpan,
  },

  #[error("Invalid literal: `{kind:?}` could not be parsed")]
  #[diagnostic(
    code(parser::invalid_literal)
  )]
  InvalidLiteral
  {
    kind: TokenKind,
    #[label("this literal value is not valid")]
    span: SourceSpan,
  },

  #[error("Unexpected end of file while looking for {expected}")]
  #[diagnostic(
    code(parser::unexpected_eof),
    help("The file ended before the program was complete — check for unclosed blocks or missing tokens")
  )]
  UnexpectedEof
  {
    expected: &'static str,
    #[label("file ends here")]
    span: SourceSpan,
  },

  #[error("Every Astra file must begin with an `entry` directive")]
  #[diagnostic(
    code(parser::missing_entry),
    help("Add `entry \"functionName\";` as the very first line of the file")
  )]
  MissingEntry
  {
    #[label("`entry` must appear here, before anything else")]
    span: SourceSpan,
  },

  #[error("`entry` must be the first statement in the file")]
  #[diagnostic(
    code(parser::entry_not_first),
    help("Move the `entry` directive to the very top of the file, before any functions or statements")
  )]
  EntryNotFirst
  {
    #[label("`entry` cannot appear here")]
    span: SourceSpan,
  },

  #[error("Invalid assignment target")]
  #[diagnostic(
    code(parser::invalid_assignment_target),
    help("Only a variable name or an array index can appear on the left side of an assignment (e.g. `x = 5` or `arr[0] = 5`)")
  )]
  InvalidAssignmentTarget
  {
    #[label("this expression cannot be assigned to")]
    span: SourceSpan,
  },
}

pub type ParseResult<T> = Result<T, ParseError>;
