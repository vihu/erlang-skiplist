use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

type SList = skiplist::SkipList<i64>;

pub struct SkiplistRes(RwLock<SList>);

impl SkiplistRes {
    pub fn read(&self) -> RwLockReadGuard<'_, SList> {
        self.0.read().unwrap()
    }

    pub fn write(&self) -> RwLockWriteGuard<'_, SList> {
        self.0.write().unwrap()
    }
}

impl From<SList> for SkiplistRes {
    fn from(other: SList) -> Self {
        SkiplistRes(RwLock::new(other))
    }
}
