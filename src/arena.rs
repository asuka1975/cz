use std::marker::PhantomData;

/// 型安全な Arena 内インデックス。
/// `T` は Arena の要素型を示す PhantomData で、異なる Arena の ID を混同しない。
#[derive(Debug)]
pub struct Id<T> {
    raw: u32,
    _marker: PhantomData<T>,
}

impl<T> Id<T> {
    pub fn new(index: usize) -> Self {
        Self {
            raw: index as u32,
            _marker: PhantomData,
        }
    }

    pub fn index(self) -> usize {
        self.raw as usize
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<T> Eq for Id<T> {}

impl<T> std::hash::Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

/// 連続メモリ上にノードを格納する Arena アロケータ。
pub struct Arena<T> {
    nodes: Vec<T>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn alloc(&mut self, node: T) -> Id<T> {
        let id = Id::new(self.nodes.len());
        self.nodes.push(node);
        id
    }

    pub fn get(&self, id: Id<T>) -> &T {
        &self.nodes[id.index()]
    }

    #[allow(dead_code)]
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.nodes[id.index()]
    }

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }
}
