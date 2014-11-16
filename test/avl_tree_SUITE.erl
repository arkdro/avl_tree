-module(avl_tree_SUITE).

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
                {group, write},
                {group, read}
               ]},
     {write, [], [
                  delete1,
                  take_smallest1,
                  insert3,
                  insert2,
                  insert1
                 ]},
     {read, [], [
                 size,
                 new_tree,
                 lookup4,
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

lookup4(_Config) ->
    Subtree1 = {1, {1, a}, nil, nil},
    Subtree2 = {2, {5, b}, Subtree1, nil},
    Subtree3 = {1, {50, c}, nil, nil},
    Subtree4 = {3, {10, d}, Subtree2, Subtree3},
    Tree = {4, Subtree4},
    none = avl_tree:lookup(2, Tree),
    {value, a} = avl_tree:lookup(1, Tree),
    {value, b} = avl_tree:lookup(5, Tree),
    {value, d} = avl_tree:lookup(10, Tree),
    {value, c} = avl_tree:lookup(50, Tree),
    ok.

size(_) ->
    0 = avl_tree:size(avl_tree:new()),
    4 = avl_tree:size({4, stub}),
    ok.

insert1(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert(5, a, T0),
    Exp1 = {1, {0, {5, a}, nil, nil}},
    Exp1 = T1,
    T2 = avl_tree:insert(3, b, T1),
    Exp2 = {2, {
              1, {5, a},
              {0, {3, b}, nil, nil},
              nil
             }},
    Exp2 = T2,
    T3 = avl_tree:insert(4, c, T2),
    Exp3 = {3, {
              1, {4, c},
              {0, {3, b}, nil, nil},
              {0, {5, a}, nil, nil}
             }},
    Exp3 = T3,
    ok.

insert2(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert(3, a, T0),
    Exp1 = {1, {0, {3, a}, nil, nil}},
    Exp1 = T1,
    T2 = avl_tree:insert(5, b, T1),
    Exp2 = {2, {
              1, {3, a},
              nil,
              {0, {5, b}, nil, nil}
             }},
    Exp2 = T2,
    T3 = avl_tree:insert(4, c, T2),
    Exp3 = {3, {
              1, {4, c},
              {0, {3, a}, nil, nil},
              {0, {5, b}, nil, nil}
             }},
    Exp3 = T3,
    ok.

insert3(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert(3, a, T0),
    T2 = avl_tree:insert(5, b, T1),
    ?assertError({key_exists, 5}, avl_tree:insert(5, c, T2)),
    ok.

take_smallest1(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert(3, a, T0),
    T2 = avl_tree:insert(5, b, T1),
    Exp2 ={{3, a},
           {1, {0, {5, b}, nil, nil}}},
    Act2 = avl_tree:take_smallest(T2),
    Exp2 = Act2,
    Exp1 = {{3, a}, T0},
    Act1 = avl_tree:take_smallest(T1),
    Exp1 = Act1,
    ok.

delete1(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert(6, a, T0),
    T2 = avl_tree:insert(3, b, T1),
    T3 = avl_tree:insert(7, c, T2),
    T4 = avl_tree:insert(1, d, T3),
    Act = avl_tree:delete(7, T4),
    Exp = {3,
           {1, {3, b},
            {0, {1, d}, nil, nil},
            {0, {6, a}, nil, nil}
           }},
    Exp = Act,
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

