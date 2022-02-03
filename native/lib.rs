mod res;
mod atoms;
mod skiplist;

use rustler::{Env, Term};

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(res::SkiplistRes, env);
    true
}

fn unload(_env: Env) {}

fn upgrade(_env: Env, _: Term) -> bool {
    true
}

rustler::init!(
    "skiplist_nif",
    [
        skiplist::new_skiplist,
        skiplist::with_capacity_skiplist,
        skiplist::push_front_skiplist,
        skiplist::pop_front_skiplist,
        skiplist::len_skiplist,
        skiplist::push_back_skiplist,
        skiplist::pop_back_skiplist,
        skiplist::clear_skiplist,
        skiplist::insert_skiplist,
        skiplist::front_skiplist,
        skiplist::back_skiplist,
        skiplist::get_skiplist,
        skiplist::remove_skiplist,
        skiplist::contains_skiplist,
        skiplist::dedup_skiplist,
    ],
    load = Some(load),
    upgrade = Some(upgrade),
    unload = Some(unload)
);
