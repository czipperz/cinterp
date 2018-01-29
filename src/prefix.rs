pub struct PrefixIterator<I: Iterator> {
    iter: I,
    prefix: Vec<I::Item>,
}

impl<I: Iterator> PrefixIterator<I> {
    pub fn new(iter: I) -> Self {
        PrefixIterator {
            iter,
            prefix: Vec::new(),
        }
    }

    pub fn push(&mut self, item: I::Item) {
        self.prefix.push(item);
    }
}

impl<I: Iterator> Iterator for PrefixIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.prefix.is_empty() {
            self.iter.next()
        } else {
            self.prefix.pop()
        }
    }
}
