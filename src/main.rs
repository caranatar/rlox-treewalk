use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: {} [script]", args[0]);
        std::process::exit(64);
    } else if args.len() == 2 {
        if rlox::interpreter::run_file(args[1].clone()).is_err() {
            std::process::exit(70);
        }
    } else {
        let _ = rlox::interpreter::run_prompt();
    }
}
