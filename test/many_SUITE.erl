-module(many_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(ASSERT, true).

-compile([export_all]).

suite() ->
    [
     {timetrap, {seconds, 180}}
    ].

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [], [
                {group, compare}
               ]},
     {compare, [], [
                 compare_with_sort
                ]}
    ].

init_per_suite(Config) ->
    random:seed(now()),
    Local = local_config(Config),
    [{local, Local} | Config].

end_per_suite(_Config) ->
    ok.

compare_with_sort(Config) ->
    Dur = set_timeout(Config),
    {Tests, Items} = compare_with_sort_till_timeout(Config, Dur),
    ct:pal("tests: ~p, items: ~p", [Tests, Items]),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_local_value(Key, Config) ->
    Local = proplists:get_value(local, Config),
    proplists:get_value(Key, Local).

set_timeout(Config) ->
    case get_local_value(duration, Config) of
        undefined ->
            30;
        Val ->
            ct:timetrap({seconds, Val + 5}),
            Val
    end.

local_config(Config) ->
    File = "local.conf",
    Dir = ?config(data_dir, Config),
    Path = filename:join([Dir, File]),
    {ok, [L]} = file:consult(Path),
    L.

compare_with_sort_till_timeout(Config, Dur) ->
    Cur = timestamp(),
    Stop = Cur + Dur - 1,
    compare_with_sort_till_timeout(Config, Cur, Stop, 0, 0).

compare_with_sort_till_timeout(_, Cur, Stop, Tests, Items) when Cur > Stop ->
    {Tests, Items};
compare_with_sort_till_timeout(Config, _, Stop, Tests, Items) ->
    Items2 = compare_with_sort_one_item(Config),
    compare_with_sort_till_timeout(Config, timestamp(), Stop, Tests + 1,
                                   Items + Items2).

compare_with_sort_one_item(Config) ->
    Data = gen_data(Config),
    Sorted = lists:sort(Data),
    Len = length(Sorted),
    Tree = avl_tree:from_list(Data),
    List = avl_tree:to_list(Tree),
    case List of
        Sorted ->
            Len;
        _ ->
            ct:log("data mismatch, len=~p~ndata:~n~p~nafter tree:~n~p~n",
                   [Len, Sorted, List]),
            erlang:error(data_mismatch)
    end.

timestamp() ->
    timestamp(os:timestamp()).

timestamp({MS, S, _}) ->
    MS * 1000000 + S.

gen_data(Config) ->
    Max_len = get_local_value(max_length, Config),
    Len = random:uniform(Max_len),
    Max_val = get_local_value(max_value, Config),
    Keys = [random:uniform(Max_val) || _ <- lists:duplicate(Len, true)],
    Keys2 = sets:to_list(sets:from_list(Keys)),
    [{Key, random:uniform(Max_val)} || Key <- Keys2].

