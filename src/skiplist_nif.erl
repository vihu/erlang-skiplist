-module(skiplist_nif).

-export([
    new_skiplist/0,
    with_capacity_skiplist/1,
    push_front_skiplist/2,
    pop_front_skiplist/1,
    len_skiplist/1,
    push_back_skiplist/2,
    pop_back_skiplist/1,
    clear_skiplist/1,
    insert_skiplist/3,
    front_skiplist/1,
    back_skiplist/1,
    get_skiplist/2,
    remove_skiplist/2,
    contains_skiplist/2,
    dedup_skiplist/1
]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-spec new_skiplist() -> {ok, reference()} | {error, any()}.
new_skiplist() ->
    not_loaded(?LINE).

-spec with_capacity_skiplist(Capacity :: pos_integer()) -> {ok, reference()} | {error, any()}.
with_capacity_skiplist(_Capacity) ->
    not_loaded(?LINE).

-spec push_front_skiplist(Skiplist :: reference(), Value :: integer()) ->
    ok | {error, any()}.
push_front_skiplist(_Skiplist, _Value) ->
    not_loaded(?LINE).

-spec pop_front_skiplist(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
pop_front_skiplist(_Skiplist) ->
    not_loaded(?LINE).

-spec push_back_skiplist(Skiplist :: reference(), Value :: integer()) ->
    ok | {error, any()}.
push_back_skiplist(_Skiplist, _Value) ->
    not_loaded(?LINE).

-spec pop_back_skiplist(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
pop_back_skiplist(_Skiplist) ->
    not_loaded(?LINE).

-spec len_skiplist(Skiplist :: reference()) -> non_neg_integer().
len_skiplist(_Skiplist) ->
    not_loaded(?LINE).

-spec clear_skiplist(Skiplist :: reference()) -> ok | {error, any()}.
clear_skiplist(_Skiplist) ->
    not_loaded(?LINE).

-spec front_skiplist(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
front_skiplist(_Skiplist) ->
    not_loaded(?LINE).

-spec back_skiplist(Skiplist :: reference()) -> {ok, integer()} | {error, any()}.
back_skiplist(_Skiplist) ->
    not_loaded(?LINE).

-spec get_skiplist(Skiplist :: reference(), Index :: non_neg_integer()) ->
    {ok, integer()} | {error, any()}.
get_skiplist(_Skiplist, _Index) ->
    not_loaded(?LINE).

-spec remove_skiplist(Skiplist :: reference(), Index :: non_neg_integer()) ->
    {ok, integer()} | {error, any()}.
remove_skiplist(_Skiplist, _Index) ->
    not_loaded(?LINE).

-spec contains_skiplist(Skiplist :: reference(), Value :: integer()) -> boolean().
contains_skiplist(_Skiplist, _Value) ->
    not_loaded(?LINE).

-spec insert_skiplist(Skiplist :: reference(), Value :: integer(), Index :: non_neg_integer()) ->
    ok | {error, any()}.
insert_skiplist(_Skiplist, _Value, _Index) ->
    not_loaded(?LINE).

-spec dedup_skiplist(Skiplist :: reference()) -> ok.
dedup_skiplist(_Skiplist) ->
    not_loaded(?LINE).

%% ==================================================================
%% NIF
%% ==================================================================

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
