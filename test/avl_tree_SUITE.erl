-module(avl_tree_SUITE).

-include_lib("common_test/include/ct.hrl").

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
                {group, read}
               ]},
     {read, [], [
                 new_tree,
                 lookup3,
                 lookup2,
                 lookup1
                ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

new_tree(_Config) ->
    {0, nil} = avl_tree:new(),
    ok.

lookup1(_Config) ->
    Subtree = {1, {1, a}, nil, nil},
    Tree = {1, Subtree},
    {value, a} = avl_tree:lookup(1, Tree),
    ok.

lookup2(_Config) ->
    Tree = {0, nil},
    none = avl_tree:lookup(2, Tree),
    ok.

lookup3(_Config) ->
    Subtree1 = {1, {1, a}, nil, nil},
    Subtree2 = {2, {2, b}, Subtree1, nil},
    Tree = {2, Subtree2},
    {value, a} = avl_tree:lookup(1, Tree),
    {value, b} = avl_tree:lookup(2, Tree),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

