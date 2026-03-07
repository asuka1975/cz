use std::collections::HashMap;

/// 変数スコープのスタック管理。
/// Lowering フェーズで名前解決に使用する。
pub struct ScopeStack<V: Copy> {
    scopes: Vec<HashMap<String, V>>,
}

impl<V: Copy> ScopeStack<V> {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    /// 現在のスコープに変数を定義する。
    /// 同一スコープに既に同名の変数がある場合は `true` を返す (重複)。
    pub fn define(&mut self, name: String, value: V) -> bool {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value).is_some()
        } else {
            false
        }
    }

    pub fn lookup(&self, name: &str) -> Option<V> {
        for scope in self.scopes.iter().rev() {
            if let Some(&value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
}
