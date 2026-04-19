use miette::{IntoDiagnostic, NamedSource, Result};
use std::{path::{Path, PathBuf}, time::{Instant}};
use clap::{Parser as clap_parser3};

mod frontend;
use frontend::lexer::{Lexer};
use frontend::parser::Parser;

mod runtime;
use runtime::interpreter::Interpreter;

pub mod args;
use args::{Cli, Commands};

use tabled::{
    Tabled, Table,
    settings::{Style, Alignment, object::Columns},
};


#[derive(Tabled)]
struct TokenRow
{
  token_kind: String,
  lexed_value: String,
  span: String,
}

fn validate_input_file(path: &Path) -> miette::Result<PathBuf>
{
  if !path.exists()
  {
    return Err(miette::miette!(
      "A file with this name does not exist: {}",
      path.display(),
    ));
  }

  if !path.is_file()
  {
    return Err(miette::miette!(
      "Path is not a file: {}",
      path.display(),
    ));
  }

  if path.extension().and_then(|string| string.to_str()) != Some("astra")
  {
    return Err(miette::miette!(
      "Invalid extension, Expected .astra, got: .{}",
      path.extension().and_then(|string| string.to_str()).unwrap_or("Null"),
    ));
  }

  Ok(path.to_path_buf())
}

fn main() -> Result<()>
{
  let cli = Cli::parse();

  match cli.commands
  {
    Commands::Tokens { file, time } =>
    {
      let path = Path::new(&file);
      match validate_input_file(path)
      {
        Ok(_) => {}
        Err(err) => return Err(err),
      }

      let time_taken_to_lex = if time {Some(Instant::now())} else {None};
      let code = std::fs::read_to_string(path).unwrap();

      let tokens = match Lexer::new(&code).tokenize()
      {
        Ok(tokens) => {tokens},
        Err(error) =>
        {
          let source = NamedSource::new(&file, code.clone());
          return Err(miette::Report::new(error).with_source_code(source))
        }
      };

      let mut token_rows: Vec<TokenRow> = Vec::new();

      for token in tokens
      {
        token_rows.push(TokenRow
        {
          token_kind: format!("{:?}", token.token_kind),
          lexed_value: token.lexed_value,
          span: format!("{:?}", token.span),
        });
      }

      let mut table = Table::new(token_rows);
      table.with(Style::modern_rounded());
      table.modify(Columns::first(), Alignment::right());

      println!("{table}");
      if let Some(time) = time_taken_to_lex
      {
        println!("\nTook: {:?} to lex the code", time.elapsed());
      }

      return Ok(());
    },

    Commands::Run { file, time } =>
    {
      let path = Path::new(&file);

      match validate_input_file(path)
      {
        Ok(_) => {}
        Err(err) => return Err(err),
      }

      let time_taken_to_execute = if time {Some(Instant::now())} else {None};

      let code = std::fs::read_to_string(path).unwrap();

      let tokens = match Lexer::new(&code).tokenize()
      {
        Ok(tokens) => {tokens},
        Err(error) =>
        {
          let source = NamedSource::new(&file, code.clone());
          return Err(miette::Report::new(error).with_source_code(source))
        }
      };

      let ast = match Parser::new(tokens).produce_ast()
      {
        Ok(ast) => {ast},
        Err(error) =>
        {
          let source = NamedSource::new(&file, code.clone());
          return Err(miette::Report::new(error).with_source_code(source))
        }
      };

      let mut interpreter = Interpreter::new();
      interpreter.run(&ast).into_diagnostic()?;

      if let Some(time) = time_taken_to_execute
      {
        println!("\nTook: {:?} to execute the Program", time.elapsed());
      }

      return Ok(());
    }
  }
}
