-module(skiplist_test).

-include_lib("eunit/include/eunit.hrl").

push_front_test() ->
    {ok, SL} = skiplist:new(),

    ok = lists:foreach(
        fun(I) ->
            ok = skiplist:push_front(SL, I)
        end,
        lists:seq(1, 100)
    ),

    ?assertEqual(100, skiplist:len(SL)),
    ?assertNot(skiplist:is_empty(SL)),
    ok = skiplist:clear(SL),
    ?assertEqual(0, skiplist:len(SL)),
    ?assert(skiplist:is_empty(SL)),

    ok.

push_back_test() ->
    {ok, SL} = skiplist:new(),

    ok = lists:foreach(
        fun(I) ->
            ok = skiplist:push_back(SL, I)
        end,
        lists:seq(1, 100)
    ),

    ?assertEqual(100, skiplist:len(SL)),
    ?assertNot(skiplist:is_empty(SL)),
    ok = skiplist:clear(SL),
    ?assertEqual(0, skiplist:len(SL)),
    ?assert(skiplist:is_empty(SL)),
    ok.

insert_test() ->
    {ok, SL} = skiplist:new(),
    ok = skiplist:insert(SL, 100, 0),
    Inserted = skiplist:front(SL),
    ?assertEqual(1, skiplist:len(SL)),
    ?assertEqual({ok, 100}, Inserted),

    ok = skiplist:insert(SL, 200, 1),
    ?assertEqual({ok, 100}, skiplist:front(SL)),
    ?assertEqual({ok, 200}, skiplist:back(SL)),

    ?assertEqual({ok, 100}, skiplist:get(SL, 0)),
    ?assertEqual({ok, 200}, skiplist:get(SL, 1)),

    {ok, 100} = skiplist:pop_front(SL),
    ?assertEqual({ok, 200}, skiplist:get(SL, 0)),
    ?assertEqual({error, undefined}, skiplist:get(SL, 1)),
    ?assertEqual(1, skiplist:len(SL)),

    ok = skiplist:insert(SL, 100, 1),
    ?assertEqual(2, skiplist:len(SL)),
    {ok, 100} = skiplist:pop_back(SL),
    ?assertEqual(1, skiplist:len(SL)),

    ok = skiplist:clear(SL),
    ?assertEqual(0, skiplist:len(SL)),

    % This should result in out of bounds error
    NotInserted = skiplist:insert(SL, 200, 10),
    ?assertEqual({error, index_out_of_bounds}, NotInserted),

    ok.

rand_test() ->
    {ok, SL} = skiplist:new(),
    Seq = lists:seq(0, 101),
    Vals = [rand:uniform(I + 1) || I <- Seq],

    [_ | Vec] = lists:reverse(
        lists:foldl(
            fun({Index, Value}, Acc) ->
                ok = skiplist:insert(SL, Value, Index),
                [Value | Acc]
            end,
            [],
            lists:zip(Seq, Vals)
        )
    ),

    List = skiplist:to_list(SL),

    ?assertEqual(Vec, List),

    ok.

contains_and_remove_test() ->
    {ok, SL} = skiplist:new(),
    ok = skiplist:insert(SL, 100, 0),
    ok = skiplist:insert(SL, 200, 1),

    true = skiplist:contains(SL, 100),
    true = skiplist:contains(SL, 200),
    false = skiplist:contains(SL, 300),

    {error, index_out_of_bounds} = skiplist:remove(SL, 2),
    {ok, 200} = skiplist:remove(SL, 1),
    {ok, 100} = skiplist:get(SL, 0),
    {ok, 100} = skiplist:remove(SL, 0),
    ?assertEqual(0, skiplist:len(SL)),

    ok.

dedup_test() ->
    {ok, SL} = skiplist:new(),

    Dup = 100,

    ToInsert = lists:seq(1, 10) ++ [Dup || _ <- lists:seq(1, 10)],

    lists:foreach(
        fun(I) ->
            skiplist:push_front(SL, I)
        end,
        ToInsert
    ),

    ?assertEqual(11, length(lists:usort(skiplist:to_list(SL)))),
    ok = skiplist:dedup(SL),
    ?assertEqual(10, length(lists:usort(skiplist:to_list(SL)))),

    ok.
