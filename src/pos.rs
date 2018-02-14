use std::fmt;
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone)]
pub struct Pos {
    file: Rc<String>,
    line: usize,
    column: usize,
}

impl Pos {
    pub fn new(file: Rc<String>, line: usize, column: usize) -> Self {
        Pos { file, line, column }
    }

    pub fn advance(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    pub fn same_line(&self, other: &Self) -> bool {
        self.line == other.line
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}:{}:{}:", self.file, self.line, self.column)
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Tag<T> {
    pub value: T,
    pub pos: Pos,
}

impl<T> Tag<T> {
    pub fn new(value: T, pos: Pos) -> Self {
        Tag { value, pos }
    }

    pub fn with_value<U>(&self, value: U) -> Tag<U> {
        Tag { value, pos: self.pos.clone() }
    }

    pub fn map<F, U>(self, fun: F) -> Tag<U> where F: FnOnce(T) -> U {
        Tag { value: fun(self.value), pos: self.pos.clone() }
    }
}

impl<T: fmt::Debug> fmt::Debug for Tag<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?} {:?}", self.pos, self.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pos_display() {
        let mut pos = Pos::new(Rc::new("heyo".to_owned()), 1, 0);
        assert_eq!(
            "heyo:1:0:".to_owned(),
            format!("{}", pos));
        pos.advance('a');
        assert_eq!(
            "heyo:1:1:".to_owned(),
            format!("{}", pos));
        pos.advance('a');
        assert_eq!(
            "heyo:1:2:".to_owned(),
            format!("{}", pos));
        pos.advance('\n');
        assert_eq!(
            "heyo:2:0:".to_owned(),
            format!("{}", pos));
        pos.advance('a');
        assert_eq!(
            "heyo:2:1:".to_owned(),
            format!("{}", pos));
    }
}
