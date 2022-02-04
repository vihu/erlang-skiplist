use crate::{atoms, res::SkiplistRes};
use rustler::{Atom, Error, NifResult, ResourceArc};
use skiplist::SkipList;

#[rustler::nif(name = "new_skiplist")]
pub fn new_skiplist() -> NifResult<(Atom, ResourceArc<SkiplistRes>)> {
    Ok((
        atoms::ok(),
        ResourceArc::new(SkiplistRes::from(SkipList::new())),
    ))
}

#[rustler::nif(name = "with_capacity_skiplist")]
pub fn with_capacity_skiplist(capacity: usize) -> NifResult<(Atom, ResourceArc<SkiplistRes>)> {
    Ok((
        atoms::ok(),
        ResourceArc::new(SkiplistRes::from(SkipList::with_capacity(capacity))),
    ))
}

#[rustler::nif(name = "push_front_skiplist")]
pub fn push_front_skiplist(sl_arc: ResourceArc<SkiplistRes>, value: i64) -> NifResult<Atom> {
    let mut sl = sl_arc.write();
    sl.push_front(value);
    Ok(atoms::ok())
}

#[rustler::nif(name = "pop_front_skiplist")]
pub fn pop_front_skiplist(sl_arc: ResourceArc<SkiplistRes>) -> NifResult<(Atom, i64)> {
    let mut sl = sl_arc.write();
    match sl.pop_front() {
        Some(val) => Ok((atoms::ok(), val)),
        None => Err(Error::Term(Box::new(atoms::undefined()))),
    }
}

#[rustler::nif(name = "push_back_skiplist")]
pub fn push_back_skiplist(sl_arc: ResourceArc<SkiplistRes>, value: i64) -> NifResult<Atom> {
    let mut sl = sl_arc.write();
    sl.push_back(value);
    Ok(atoms::ok())
}

#[rustler::nif(name = "pop_back_skiplist")]
pub fn pop_back_skiplist(sl_arc: ResourceArc<SkiplistRes>) -> NifResult<(Atom, i64)> {
    let mut sl = sl_arc.write();
    match sl.pop_back() {
        Some(val) => Ok((atoms::ok(), val)),
        None => Err(Error::Term(Box::new(atoms::undefined()))),
    }
}

#[rustler::nif(name = "len_skiplist")]
pub fn len_skiplist(sl_arc: ResourceArc<SkiplistRes>) -> NifResult<usize> {
    let sl = sl_arc.read();
    Ok(sl.len())
}

#[rustler::nif(name = "clear_skiplist")]
pub fn clear_skiplist(sl_arc: ResourceArc<SkiplistRes>) -> NifResult<Atom> {
    let mut sl = sl_arc.write();
    sl.clear();
    Ok(atoms::ok())
}

#[rustler::nif(name = "front_skiplist")]
pub fn front_skiplist(sl_arc: ResourceArc<SkiplistRes>) -> NifResult<(Atom, i64)> {
    let sl = sl_arc.read();

    match sl.front() {
        Some(val) => Ok((atoms::ok(), *val)),
        None => Err(Error::Term(Box::new(atoms::undefined()))),
    }
}

#[rustler::nif(name = "back_skiplist")]
pub fn back_skiplist(sl_arc: ResourceArc<SkiplistRes>) -> NifResult<(Atom, i64)> {
    let sl = sl_arc.read();

    match sl.back() {
        Some(val) => Ok((atoms::ok(), *val)),
        None => Err(Error::Term(Box::new(atoms::undefined()))),
    }
}

#[rustler::nif(name = "get_skiplist")]
pub fn get_skiplist(sl_arc: ResourceArc<SkiplistRes>, index: usize) -> NifResult<(Atom, i64)> {
    let sl = sl_arc.read();

    match sl.get(index) {
        Some(val) => Ok((atoms::ok(), *val)),
        None => Err(Error::Term(Box::new(atoms::index_out_of_bounds()))),
    }
}

#[rustler::nif(name = "remove_skiplist")]
pub fn remove_skiplist(sl_arc: ResourceArc<SkiplistRes>, index: usize) -> NifResult<(Atom, i64)> {
    let mut sl = sl_arc.write();
    let to_remove = sl.remove(index);
    Ok((atoms::ok(), to_remove))
}

#[rustler::nif(name = "insert_skiplist")]
pub fn insert_skiplist(
    sl_arc: ResourceArc<SkiplistRes>,
    value: i64,
    index: usize,
) -> NifResult<Atom> {
    let mut sl = sl_arc.write();
    sl.insert(value, index);
    Ok(atoms::ok())
}

#[rustler::nif(name = "contains_skiplist")]
pub fn contains_skiplist(sl_arc: ResourceArc<SkiplistRes>, value: i64) -> NifResult<bool> {
    let sl = sl_arc.read();
    Ok(sl.contains(&value))
}

#[rustler::nif(name = "dedup_skiplist")]
pub fn dedup_skiplist(sl_arc: ResourceArc<SkiplistRes>) -> NifResult<Atom> {
    let mut sl = sl_arc.write();
    sl.dedup();
    Ok(atoms::ok())
}

#[rustler::nif(name = "modify_skiplist")]
pub fn modify_skiplist(sl_arc: ResourceArc<SkiplistRes>, value: i64, index: usize) -> NifResult<Atom> {
    let mut sl = sl_arc.write();

    match sl.get_mut(index) {
        Some(v) => {
            *v = value;
            Ok(atoms::ok())
        }
        None => Err(Error::Term(Box::new(atoms::index_out_of_bounds()))),
    }

}
