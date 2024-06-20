use clap::Parser;

fn main() {
    if let Err(e) = sprout::run(&Cli::parse().src) {
        println!("{}", e);
    }
}

#[derive(Parser)]
struct Cli {
    src: String
}
