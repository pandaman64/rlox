struct Entry {
    line: usize,
    start_position: usize,
}

#[derive(Default)]
pub struct LineMap {
    // sorted list
    entries: Vec<Entry>,
}

impl LineMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, line: usize, start_position: usize) {
        if let Some(last) = self.entries.last() {
            assert_eq!(last.line + 1, line);
            assert!(last.start_position < start_position);
        }

        self.entries.push(Entry {
            line,
            start_position,
        })
    }

    pub fn resolve(&self, position: usize) -> usize {
        match self
            .entries
            .binary_search_by_key(&position, |entry| entry.start_position)
        {
            Ok(index) => self.entries[index].line,
            Err(index) => self.entries[index - 1].line,
        }
    }
}
