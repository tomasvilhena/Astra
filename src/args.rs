use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "Astra", about = "Astra is an interpreted programming language, written in rust as a school project")]
pub struct Cli
{
  #[command(subcommand)]
  pub commands: Commands,
}

#[derive(Subcommand)]
pub enum Commands
{
  Run
  {
    file: String,

    #[arg(short, long)]
    time: bool,
  },

  Tokens
  {
    file: String,

    #[arg(short, long)]
    time: bool,
  },
}


