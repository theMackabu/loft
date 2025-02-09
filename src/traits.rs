pub trait ResultExt<T, E> {
    fn is_ok(&self) -> bool;
    fn is_err(&self) -> bool;
    fn ok(self) -> Option<T>;
    fn err(self) -> Option<E>;
    fn unwrap(self) -> T;
    fn unwrap_or(self, default: T) -> T;
    fn unwrap_or_else<F: FnOnce(E) -> T>(self, op: F) -> T;
}

pub trait OptionExt<T> {
    fn is_some(&self) -> bool;
    fn is_none(&self) -> bool;
    fn unwrap(self) -> T;
    fn unwrap_or(self, default: T) -> T;
    fn unwrap_or_else<F: FnOnce() -> T>(self, f: F) -> T;
}
