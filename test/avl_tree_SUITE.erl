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
                  map3,
                  map2,
                  map1,
                  from_list,
                  delete5,
                  delete4,
                  delete3,
                  delete2,
                  delete1,
                  take_largest1,
                  take_smallest1,
                  insert_any1,
                  insert3,
                  insert2,
                  insert1
                 ]},
     {read, [], [
                 keys1,
                 to_list2,
                 to_list1,
                 largest,
                 smallest,
                 member,
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

member(_) ->
    true = avl_tree:member(15, tree1()),
    false = avl_tree:member(99, tree1()),
    ok.

smallest(_) ->
    L = [{12, a},
         {15, b},
         {20, c},
         {5, d},
         {1, e},
         {9, f},
         {7, g},
         {10, h}],
    Tree = tree1(L),
    {1, e} = avl_tree:smallest(Tree),
    ok.

largest(_) ->
    L = [{12, a},
         {15, b},
         {22, c},
         {5, d},
         {1, e},
         {9, f},
         {7, g},
         {10, h}],
    Tree = tree1(L),
    {22, c} = avl_tree:largest(Tree),
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

insert_any1(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert_any(3, a, T0),
    T2 = avl_tree:insert_any(5, b, T1),
    T3 = avl_tree:insert_any(5, c, T2),
    Exp = {2,
           {1, {3, a},
            nil,
            {0, {5, c}, nil, nil}
           }},
    Exp = T3,
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

take_largest1(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert(3, a, T0),
    T2 = avl_tree:insert(5, b, T1),
    Exp2 ={{5, b},
           {1, {0, {3, a}, nil, nil}}},
    Act2 = avl_tree:take_largest(T2),
    Exp2 = Act2,
    Exp1 = {{3, a}, T0},
    Act1 = avl_tree:take_largest(T1),
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

delete2(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert(6, a, T0),
    ?assertError({no_key, 7}, avl_tree:delete(7, T1)),
    ok.

delete3(_) ->
    Tree = tree1(),
    Act = avl_tree:delete(7, Tree),
    Exp = {7,
           {3, {12,a},
            {2, {5,d},
             {0,{2,e},nil,nil},
             {1,{9,f},nil,{0,{10,h},nil,nil}}
            },
            {1,{15,b},nil,{0,{20,c},nil,nil}}
           }
          },
    Exp = Act,
    ok.

delete4(_) ->
    Tree = tree1(),
    Act = avl_tree:delete(9, Tree),
    Exp = {7,
           {3, {12,a},
            {2, {5,d},
             {0,{2,e},nil,nil},
             {1,{10,h},{0,{7,g},nil,nil},nil}
            },
            {1,{15,b},nil,{0,{20,c},nil,nil}}
           }
          },
    Exp = Act,
    ok.

delete5(_) ->
    Tree = tree1(),
    Tree = avl_tree:delete_any(99, Tree),
    ok.

from_list(_) ->
    L = [{12, a},
         {15, b},
         {20, c},
         {5, d},
         {2, e},
         {9, f},
         {7, g},
         {10, h}],
    Tree = tree1(L),
    Tree = avl_tree:from_list(L),
    ok.

to_list1(_) ->
    L = [{12, a},
         {15, b},
         {20, c},
         {5, d},
         {2, e},
         {9, f},
         {7, g},
         {10, h}],
    Tree = tree1(L),
    Sorted = lists:sort(L),
    Sorted = avl_tree:to_list(Tree),
    ok.

to_list2(_) ->
    [] = avl_tree:to_list(avl_tree:new()),
    ok.

map1(_) ->
    Tree = avl_tree:insert(1, a, avl_tree:new()),
    F = fun(_K, V) ->
                V
        end,
    Tree = avl_tree:map(F, Tree),
    ok.

map2(_) ->
    Tree = tree1(),
    F = fun(_K, V) ->
                V
        end,
    trc_util:start([avl_tree]),
    Tree = avl_tree:map(F, Tree),
    trc_util:stop(),
    ok.

map3(_) ->
    T0 = avl_tree:new(),
    T1 = avl_tree:insert(6, 1, T0),
    T2 = avl_tree:insert(3, 2, T1),
    T3 = avl_tree:insert(7, 3, T2),
    T1e = avl_tree:insert_any(6, 11, T0),
    T2e = avl_tree:insert_any(3, 22, T1e),
    T3e = avl_tree:insert_any(7, 33, T2e),
    F = fun(_K, V) ->
                V * 11
        end,
    T3e = avl_tree:map(F, T3),
    ok.

keys1(_) ->
    Keys = [Key || {Key, _} <- items1()],
    Exp = lists:sort(Keys),
    Exp = avl_tree:keys(tree1()),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

items1() ->
    [{12, a},
     {15, b},
     {20, c},
     {5, d},
     {2, e},
     {9, f},
     {7, g},
     {10, h}].

tree1() ->
    L = items1(),
    tree1(L).

tree1(L) ->
    Tree = lists:foldl(fun({K, V}, Acc) ->
                               avl_tree:insert(K, V, Acc)
                       end, avl_tree:new(), L),
    %% ct:pal("tree: ~p", [Tree]),
    Tree.

