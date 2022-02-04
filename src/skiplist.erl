%% @doc API for interacting with a skiplist

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
    nth/2,
    remove/2,
    to_list/1,
    contains/2,
    dedup/1,
    modify/3
]).


%% @doc Create a new empty skiplist.
-spec new() -> {ok, reference()} | {error, any()}.
new() ->
    skiplist_nif:new_skiplist().

%% @doc Create a new skiplist with specific `Capacity'.
-spec with_capacity(Capacity :: pos_integer()) -> {ok, reference()} | {error, any()}.
with_capacity(Capacity) ->
    skiplist_nif:with_capacity_skiplist(Capacity).

%% @doc Push `Value' to the front of the skiplist.
-spec push_front(Skiplist :: reference(), Value :: integer()) -> ok | {error, any()}.
push_front(Skiplist, Value) ->
    skiplist_nif:push_front_skiplist(Skiplist, Value).

%% @doc Pop from front of the skiplist. Mutates the existing skiplist.
-spec pop_front(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
pop_front(Skiplist) ->
    skiplist_nif:pop_front_skiplist(Skiplist).

%% @doc Push `Value' to the back of the skiplist.
-spec push_back(Skiplist :: reference(), Value :: integer()) -> ok | {error, any()}.
push_back(Skiplist, Value) ->
    skiplist_nif:push_back_skiplist(Skiplist, Value).

%% @doc Pop from back of the skiplist. Mutates the existing skiplist.
-spec pop_back(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
pop_back(Skiplist) ->
    skiplist_nif:pop_back_skiplist(Skiplist).

%% @doc Return length of the skiplist.
-spec len(Skiplist :: reference()) -> non_neg_integer().
len(Skiplist) ->
    skiplist_nif:len_skiplist(Skiplist).

%% @doc Empties the skiplist.
-spec clear(Skiplist :: reference()) -> ok | {error, any()}.
clear(Skiplist) ->
    skiplist_nif:clear_skiplist(Skiplist).

%% @doc Returns `true' if the skiplist is empty, `false' otherwise.
-spec is_empty(Skiplist :: reference()) -> boolean().
is_empty(Skiplist) ->
    ?MODULE:len(Skiplist) == 0.

%% @doc Get the value from the front of the skiplist. Does not mutate the skiplist.
-spec front(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
front(Skiplist) ->
    skiplist_nif:front_skiplist(Skiplist).

%% @doc Get the value from the back of the skiplist. Does not mutate the skiplist.
-spec back(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
back(Skiplist) ->
    skiplist_nif:back_skiplist(Skiplist).

%% @doc Get the value at the `nth' index of the skiplist. Does not mutate the skiplist.
-spec nth(Skiplist :: reference(), Index :: non_neg_integer()) -> {ok, integer()} | {error, any()}.
nth(Skiplist, Index) ->
    skiplist_nif:get_skiplist(Skiplist, Index).

%% @doc Remove the value at the specified index of the skiplist. Mutates the skiplist.
-spec remove(Skiplist :: reference(), Index :: non_neg_integer()) ->
    {ok, integer()} | {error, any()}.
remove(Skiplist, Index) ->
    case Index >= ?MODULE:len(Skiplist) of
        true -> {error, index_out_of_bounds};
        false -> skiplist_nif:remove_skiplist(Skiplist, Index)
    end.

%% @doc Insert `Value' at the `Index' of the skiplist.
-spec insert(Skiplist :: reference(), Value :: integer(), Index :: non_neg_integer()) ->
    ok | {error, any()}.
insert(Skiplist, Value, Index) ->
    case Index > ?MODULE:len(Skiplist) of
        true -> {error, index_out_of_bounds};
        false -> skiplist_nif:insert_skiplist(Skiplist, Value, Index)
    end.

%% @doc Converst the skiplist reference to erlang style list.
-spec to_list(Skiplist :: reference()) -> [integer()].
to_list(Skiplist) ->
    Len = ?MODULE:len(Skiplist),
    lists:reverse(
        lists:foldl(
            fun(I, Acc) ->
                case ?MODULE:nth(Skiplist, I) of
                    {ok, Value} -> [Value | Acc];
                    {error, _} -> Acc
                end
            end,
            [],
            lists:seq(1, Len)
        )
    ).

%% @doc Checks whether the skiplist contains the given `Value'.
-spec contains(Skiplist :: reference(), Value :: integer()) -> boolean().
contains(Skiplist, Value) ->
    skiplist_nif:contains_skiplist(Skiplist, Value).

%% @doc Removes consecutive duplicates items from the skiplist. Mutates the skiplist.
-spec dedup(Skiplist :: reference()) -> ok.
dedup(Skiplist) ->
    skiplist_nif:dedup_skiplist(Skiplist).

%% @doc Modify the skiplist and insert `Value' at the given `Index'.
-spec modify(Skiplist :: reference(), Value :: integer(), Index :: non_neg_integer()) ->
    ok | {error, any()}.
modify(Skiplist, Value, Index) ->
    skiplist_nif:modify_skiplist(Skiplist, Value, Index).
