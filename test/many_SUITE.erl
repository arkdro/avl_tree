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
     {compare, [parallel], [
                 add_and_del,
                 compare_with_sort
                ]}
    ].

init_per_suite(Config) ->
    random:seed(now()),
    Local = local_config(Config),
    New = [{local, Local} | Config],
    set_timeout(New),
    New.

end_per_suite(_Config) ->
    ok.

compare_with_sort(Config) ->
    Dur = get_duration(Config),
    {Tests, Items} = compare_with_sort_till_timeout(Config, Dur),
    ct:pal("tests: ~p, items: ~p", [Tests, Items]),
    ok.

add_and_del(Config) ->
    Dur = get_duration(Config),
    {Tests, Items} = add_and_del_till_timeout(Config, Dur),
    ct:pal("add and del tests: ~p, items: ~p", [Tests, Items]),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_local_value(Key, Config) ->
    Local = proplists:get_value(local, Config),
    proplists:get_value(Key, Local).

set_timeout(Config) ->
    Dur = get_duration(Config),
    T = {seconds, Dur + 300},
    ct:timetrap(T).

get_duration(Config) ->
    case get_local_value(duration, Config) of
        undefined ->
            30;
        Val ->
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

add_and_del_till_timeout(Config, Dur) ->
    Cur = timestamp(),
    Stop = Cur + Dur - 1,
    add_and_del_till_timeout(Config, Cur, Stop, 0, 0).

add_and_del_till_timeout(_, Cur, Stop, Tests, Items) when Cur > Stop ->
    {Tests, Items};
add_and_del_till_timeout(Config, _, Stop, Tests, Items) ->
    Items2 = add_and_del_one_item(Config),
    add_and_del_till_timeout(Config, timestamp(), Stop, Tests + 1,
                                   Items + Items2).

add_and_del_one_item(Config) ->
    {Data, To_del, Deleted} = gen_data_for_add_and_del(Config),
    Tree = avl_tree:from_list(Data),
    Tree2 = delete_items(Tree, To_del),
    List = avl_tree:to_list(Tree2),
    Deleted_sorted = lists:sort(Deleted),
    Len = length(Deleted_sorted),
    Check_result = check_util:check_height(Tree2),
    case List of
        Deleted_sorted when Check_result == ok ->
            Len;
        _ ->
            ct:log("data mismatch, len=~p~ndata:~n~p~nafter tree:~n~p~n"
                   "check_result:~n~p~n",
                   [Len, Deleted_sorted, List, Check_result]),
            erlang:error(data_mismatch)
    end.

delete_items(Tree, To_del) ->
    lists:foldl(fun({Key, _}, Acc) ->
                        avl_tree:delete(Key, Acc)
                end, Tree, To_del).

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

gen_data_for_add_and_del(Config) ->
    L = gen_data(Config),
    All = sets:from_list(L),
    Ndel = random:uniform(sets:size(All)),
    Shuffled = shuffle(L),
    To_del = lists:sublist(Shuffled, Ndel),
    To_del_set = sets:from_list(To_del),
    Del_set = sets:subtract(All, To_del_set),
    Deleted = sets:to_list(Del_set),
    {L, To_del, Deleted}.


shuffle(L) ->
    L2 = [{random:uniform(), X} || X <- L],
    L3 = lists:sort(L2),
    L4 = [X || {_, X} <- L3],
    L4.
