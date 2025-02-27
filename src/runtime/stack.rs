use std::collections::VecDeque;

#[derive(Debug)]
pub struct WorkQueue<T> {
    queue: VecDeque<T>,
}

impl<T> WorkQueue<T> {
    /// Creates a new, empty work queue.
    #[inline(always)]
    pub fn new() -> Self { Self { queue: VecDeque::new() } }

    /// Creates a new work queue with the given capacity.
    #[inline(always)]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            queue: VecDeque::with_capacity(capacity),
        }
    }

    /// Enqueues an item at the back.
    #[inline(always)]
    pub fn enqueue(&mut self, item: T) { self.queue.push_back(item); }

    /// Dequeues an item from the front. Returns None when empty.
    #[inline(always)]
    pub fn dequeue(&mut self) -> Option<T> { self.queue.pop_front() }

    /// True if there are no items in the work queue.
    #[inline(always)]
    pub fn is_empty(&self) -> bool { self.queue.is_empty() }

    /// Returns the number of items in the work queue.
    #[inline(always)]
    pub fn len(&self) -> usize { self.queue.len() }
}
