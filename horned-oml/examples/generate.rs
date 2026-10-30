//! Write a synthetic vocabulary and description: `generate DIR N M`.

#[path = "../benches/generate.rs"]
mod generate;

use std::fs;
use std::path::Path;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let dir = Path::new(&args[1]);
    let n: usize = args[2].parse().unwrap();
    let m: usize = args[3].parse().unwrap();
    let vocab = dir.join("example.com/big");
    fs::create_dir_all(&vocab).unwrap();
    fs::write(vocab.join("vocab.oml"), generate::vocabulary(n)).unwrap();
    fs::write(vocab.join("data.oml"), generate::description(n, m)).unwrap();
}
