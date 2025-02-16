use clap::{Arg, Command};
use wat::compile;

fn main() {
    let matches = Command::new("jasper")
        .version("0.1.0")
        .author("Dibash Thapa <dibashthapa55@gmail.com>")
        .about("JS to WASM compiler")
        .subcommand(
            Command::new("run")
                .about("Run the given JS file")
                .arg(Arg::new("file").required(true)),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("run") {
        let file_path: &String = matches.get_one("file").unwrap();
        let output = compile(file_path).unwrap();
        println!("{}", output);
    }
}
