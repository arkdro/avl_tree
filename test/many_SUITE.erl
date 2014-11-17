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
    Local = local_config(Config),
    [{local, Local} | Config].

end_per_suite(_Config) ->
    ok.

compare_with_sort(Config) ->
    set_timeout(Config),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

set_timeout(Config) ->
    Local = proplists:get_value(local, Config),
    case proplists:get_value(timetrap, Local) of
        undefined ->
            ok;
        Val ->
            ct:timetrap(Val)
    end.

local_config(Config) ->
    File = "local.conf",
    Dir = ?config(data_dir, Config),
    Path = filename:join([Dir, File]),
    {ok, [L]} = file:consult(Path),
    L.

