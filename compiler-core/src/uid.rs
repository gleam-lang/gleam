use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

/// A generator of unique ids. Only one should be used per compilation run to
/// ensure ids do not get reused.
#[derive(Debug, Clone, Default)]
pub struct UniqueIdGenerator {
    id: Arc<AtomicU64>,
}

impl UniqueIdGenerator {
    pub fn new() -> Self {
        Self::default()
    }

    /// Fork the id generator into a new one that does not share the same
    /// internal id. It will increment independantly. Not to be used in the same
    /// compilation run.
    pub fn fork(&self) -> Self {
        let current = self.id.load(Ordering::Relaxed);
        let id = AtomicU64::new(current);
        Self { id: Arc::new(id) }
    }

    pub fn next(&self) -> u64 {
        self.id.fetch_add(1, Ordering::Relaxed)
    }
}

#[test]
fn id_geneation() {
    let ids = UniqueIdGenerator::new();
    let ids2 = ids.clone();

    assert_eq!(ids.next(), 0);
    assert_eq!(ids.next(), 1);
    assert_eq!(ids.next(), 2);

    // Cloned ones use the same counter
    assert_eq!(ids2.next(), 3);
    assert_eq!(ids2.next(), 4);
    assert_eq!(ids2.next(), 5);

    // The original is updated
    assert_eq!(ids.next(), 6);
    assert_eq!(ids.next(), 7);
}
