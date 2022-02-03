-module(skiplist).

-export([
    new/0,
    with_capacity/1,
    push_front/2,
    pop_front/1,
    push_back/2,
    pop_back/1,
    len/1,
    clear/1,
    is_empty/1,
    insert/3,
    front/1,
    back/1,
    get/2,
    remove/2,
    to_list/1,
    contains/2,
    dedup/1
]).

-spec new() -> {ok, reference()} | {error, any()}.
new() ->
    skiplist_nif:new_skiplist().

-spec with_capacity(Capacity :: pos_integer()) -> {ok, reference()} | {error, any()}.
with_capacity(Capacity) ->
    skiplist_nif:with_capacity_skiplist(Capacity).

-spec push_front(Skiplist :: reference(), Value :: integer()) -> ok | {error, any()}.
push_front(Skiplist, Value) ->
    skiplist_nif:push_front_skiplist(Skiplist, Value).

-spec pop_front(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
pop_front(Skiplist) ->
    skiplist_nif:pop_front_skiplist(Skiplist).

-spec push_back(Skiplist :: reference(), Value :: integer()) -> ok | {error, any()}.
push_back(Skiplist, Value) ->
    skiplist_nif:push_back_skiplist(Skiplist, Value).

-spec pop_back(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
pop_back(Skiplist) ->
    skiplist_nif:pop_back_skiplist(Skiplist).

-spec len(Skiplist :: reference()) -> non_neg_integer().
len(Skiplist) ->
    skiplist_nif:len_skiplist(Skiplist).

-spec clear(Skiplist :: reference()) -> ok | {error, any()}.
clear(Skiplist) ->
    skiplist_nif:clear_skiplist(Skiplist).

-spec is_empty(Skiplist :: reference()) -> boolean().
is_empty(Skiplist) ->
    ?MODULE:len(Skiplist) == 0.

-spec front(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
front(Skiplist) ->
    skiplist_nif:front_skiplist(Skiplist).

-spec back(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
back(Skiplist) ->
    skiplist_nif:back_skiplist(Skiplist).

-spec get(Skiplist :: reference(), Index :: non_neg_integer()) -> {ok, integer()} | {error, any()}.
get(Skiplist, Index) ->
    skiplist_nif:get_skiplist(Skiplist, Index).

-spec remove(Skiplist :: reference(), Index :: non_neg_integer()) ->
    {ok, integer()} | {error, any()}.
remove(Skiplist, Index) ->
    case Index >= ?MODULE:len(Skiplist) of
        true -> {error, index_out_of_bounds};
        false -> skiplist_nif:remove_skiplist(Skiplist, Index)
    end.

-spec insert(Skiplist :: reference(), Value :: integer(), Index :: non_neg_integer()) ->
    ok | {error, any()}.
insert(Skiplist, Value, Index) ->
    case Index > ?MODULE:len(Skiplist) of
        true -> {error, index_out_of_bounds};
        false -> skiplist_nif:insert_skiplist(Skiplist, Value, Index)
    end.

-spec to_list(Skiplist :: reference()) -> [integer()].
to_list(Skiplist) ->
    Len = ?MODULE:len(Skiplist),
    lists:reverse(
        lists:foldl(
            fun(I, Acc) ->
                case ?MODULE:get(Skiplist, I) of
                    {ok, Value} -> [Value | Acc];
                    {error, _} -> Acc
                end
            end,
            [],
            lists:seq(1, Len)
        )
    ).

-spec contains(Skiplist :: reference(), Value :: integer()) -> boolean().
contains(Skiplist, Value) ->
    skiplist_nif:contains_skiplist(Skiplist, Value).

-spec dedup(Skiplist :: reference()) -> ok.
dedup(Skiplist) ->
    skiplist_nif:dedup_skiplist(Skiplist).
