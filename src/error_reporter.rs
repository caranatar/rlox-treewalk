use crate::lox_error::LoxError;

struct Err {
    ty: LoxError,
    line: usize,
}

pub struct ErrorReporter {
    errors: Vec<Err>,
}

impl ErrorReporter {
    pub fn new() -> Self {
        ErrorReporter { errors: Vec::new() }
    }

    pub fn error(&mut self, e: LoxError, line: usize) {
        self.errors.push(Err{ ty: e, line });
    }

    pub fn report(&mut self) {
        for e in self.errors.drain(..) {
            println!("{:?} on line {}", e.ty, e.line);
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

impl Default for ErrorReporter {
    fn default() -> Self {
        ErrorReporter::new()
    }
}
