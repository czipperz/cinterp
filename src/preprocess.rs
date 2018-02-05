use std::rc::Rc;
use pos::*;

pub struct PreprocessorIterator<I: Iterator<Item = char>> {
    pos: Pos,
    iter: I,
}

impl<I: Iterator<Item = char>> PreprocessorIterator<I> {
    pub fn new(file_name: &str, iter: I) -> Self {
        PreprocessorIterator {
            pos: Pos::new(Rc::new(file_name.to_owned()), 1, 0),
            iter,
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for PreprocessorIterator<I> {
    type Item = Tag<char>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(c) => {
                let pos = self.pos.clone();
                self.pos.advance(c);
                Some(Tag::new(c, pos))
            },
            None => None,
        }
    }
}
