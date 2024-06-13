mod lexer;

use anyhow::anyhow;
use std::io::{BufReader, Read};
use std::fs::File;
use crate::lexer::tokenize;

pub fn run(src: &str) -> anyhow::Result<()> {
    tokenize(&read_file(src)?);
    todo!()
}

fn read_file(src: &str) -> anyhow::Result<String> {
    let mut text = String::new();
    let mut reader = BufReader::new(File::open(src).map_err(|e| anyhow!("failed to open file - {e}"))?);
    reader.read_to_string(&mut text).map_err(|e| anyhow!("failed to open file - {e}"))?;
    Ok(text)
}

