use std::result::Result;

use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::{resolver::ResolveVisitor, eval_visitor::EvalVisitor};

fn run(source: String, eval: &mut EvalVisitor) -> Result<(), ()> {
    let reporter = &mut eval.reporter;
    let mut scanner = Scanner::new(source, reporter);
    let tokens = scanner.scan_tokens();

    if reporter.has_errors() {
        reporter.report();
        return Err(());
    }

    let mut parser = Parser::new(tokens, reporter);
    let mut stmts = parser.parse();

    if reporter.has_errors() {
        reporter.report();
        return Err(());
    }

    let mut resolver = ResolveVisitor::new(reporter);
    resolver.resolve_stmts(&mut stmts);

    if reporter.has_errors() {
        reporter.report();
        return Err(());
    }

    for stmt in stmts {
        if stmt.eval(eval).is_err() {
            // Break on runtime error
            break;
        }
    }

    if eval.reporter.has_errors() {
        eval.reporter.report();
        Err(())
    } else {
        Ok(())
    }
}

pub fn run_file(path: String) -> Result<(), ()> {
    let contents = slurp::read_all_to_string(path).unwrap();
    let mut eval = EvalVisitor::new();
    run(contents, &mut eval)
}

pub fn run_prompt() -> Result<(), ()> {
    use rustyline::error::ReadlineError;
    use rustyline::Editor;
    let mut eval = EvalVisitor::new();
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(line) => {
                let _ = run(line, &mut eval);
            }
            Err(ReadlineError::Interrupted) => {
                println!("ctrl-c");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("ctrl-d");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
