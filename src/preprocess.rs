use std::rc::Rc;
use pos::*;
use prefix::*;
use std::iter::Fuse;
use std::io;

struct Tagged<I: Iterator<Item = char>> {
    pos: Pos,
    iter: Fuse<I>,
}

impl<I: Iterator<Item = char>> Tagged<I> {
    fn new(file_name: &str, iter: Fuse<I>) -> Self {
        Tagged { pos: Pos::new(Rc::new(file_name.to_owned()), 1, 0), iter }
    }
}

impl<I: Iterator<Item = char>> Iterator for Tagged<I> {
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

pub struct Preprocessor<I: Iterator<Item = char>> {
    iter: PrefixIterator<Tagged<I>>,
    //files: Vec<PrefixIterator<PosIterator<ReaderIterator>>>,
}

impl<I: Iterator<Item = char>> Preprocessor<I> {
    pub fn new(file_name: &str, iter: Fuse<I>) -> Self {
        Preprocessor {
            iter: PrefixIterator::new(Tagged::new(file_name, iter)),
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Preprocessor<I> {
    type Item = io::Result<Tag<char>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(c) => {
                if c.value == '\\' {
                    match self.iter.next() {
                        Some(escaped) => {
                            if escaped.value == '\n' {
                                self.iter.next().map(Ok)
                            } else {
                                self.iter.push(escaped);
                                Some(Ok(c))
                            }
                        },
                        None => {
                            Some(Ok(c))
                        }
                    }
                } else if c.value == '/' {
                    match self.iter.next() {
                        Some(Tag { value: '*', pos: _ }) => {
                            let mut next = self.iter.next();
                            loop {
                                match next {
                                    Some(Tag { value: '*', pos: _ }) => {
                                        match self.iter.next() {
                                            Some(Tag { value: '/', pos: _ }) => return Some(Ok(Tag::new(' ', c.pos))),
                                            Some(c) => next = Some(c),
                                            None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                                   format!("{} Found EOF when expected end of comment started here", c.pos)))),
                                        }
                                    },
                                    Some(_) => next = self.iter.next(),
                                    None => return Some(Err(io::Error::new(io::ErrorKind::InvalidInput,
                                                                           format!("{} Found EOF when expected end of comment started here", c.pos)))),
                                }
                            }
                        },
                        Some(n) => {
                            self.iter.push(n);
                            Some(Ok(c))
                        },
                        None => None
                    }
                } else {
                    Some(Ok(c))
                }
            },
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn preprocess(s: &str) -> io::Result<Vec<Tag<char>>> {
        Preprocessor::new("*stdin*", s.chars().fuse()).collect()
    }

    fn pos(line: usize, column: usize) -> Pos {
        Pos::new(Rc::new("*stdin*".to_owned()), line, column)
    }

    #[test]
    fn preprocess_backslash_newline() {
        assert_eq!(vec![Tag::new('h', pos(1, 0)),
                        Tag::new('e', pos(1, 1)),
                        Tag::new('l', pos(2, 0)),
                        Tag::new('l', pos(2, 1)),
                        Tag::new('o', pos(2, 2))],
                   preprocess("he\\\nllo").unwrap());
    }

    #[test]
    fn preprocess_delete_block_comment() {
        assert_eq!(vec![Tag::new('h', pos(1, 0)),
                        Tag::new(' ', pos(1, 1)),
                        Tag::new('e', pos(1, 13))],
                   preprocess("h/*ello lov*/e").unwrap());
    }

    #[test]
    fn preprocess_delete_multiline_block_comment() {
        assert_eq!(vec![Tag::new('h', pos(1, 0)),
                        Tag::new(' ', pos(1, 1)),
                        Tag::new('e', pos(3, 10))],
                   preprocess("h/*ao  nteuheou\nao hte  nu\toehun\noh\tenueo*/e").unwrap());
    }

    #[test]
    fn preprocess_dont_delete_line_comment() {
        assert_eq!(vec![Tag::new('h', pos(1, 0)),
                        Tag::new('/', pos(1, 1)),
                        Tag::new('/', pos(1, 2)),
                        Tag::new('c', pos(1, 3))],
                   preprocess("h//c").unwrap());
    }

    #[test]
    fn preprocess_unclosed_line_comment() {
        assert_eq!("*stdin*:1:12: Found EOF when expected end of comment started here",
                   format!("{}", preprocess("hello world /*/").unwrap_err()));
    }
}
