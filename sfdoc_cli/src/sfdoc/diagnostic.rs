pub trait DiagnosticEmitter {
    fn warning(&mut self, line: usize, msg: String);
    fn error(&mut self, line: usize, msg: String);
}
