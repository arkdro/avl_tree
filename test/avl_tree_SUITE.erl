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
                  delete10,
                  delete9,
                  delete8,
                  delete7,
                  delete6,
                  delete5,
                  delete4,
                  delete3,
                  delete2,
                  delete1,
                  take_largest2,
                  take_largest1,
                  take_smallest2,
                  take_smallest1,
                  insert_any1,
                  insert3,
                  insert2,
                  insert1
                 ]},
     {read, [], [
                 keys1,
                 values1,
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
    ok = check_util:check_height(T3),
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

take_smallest2(_) ->
    Tree = avl_tree:from_list([{5, a}, {3, b}, {10, c}, {15, d}]),
    Act = avl_tree:take_smallest(Tree),
    Exp = {{3,b},
           {3,{1,{10,c},
               {0,{5,a},nil,nil},
               {0,{15,d},nil,nil}}}
          },
    Exp = Act,
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

take_largest2(_) ->
    Tree = avl_tree:from_list([{5, a}, {3, b}, {10, c}, {15, d}]),
    Act = avl_tree:take_largest(Tree),
    Exp = {{15,d},
           {3,{1,{5,a},
               {0,{3,b},nil,nil},
               {0,{10,c},nil,nil}}}
          },
    Exp = Act,
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
    ok = check_util:check_height(Act),
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
    ok = check_util:check_height(Act),
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
    ok = check_util:check_height(Act),
    ok.

delete5(_) ->
    Tree = tree1(),
    Tree = avl_tree:delete_any(99, Tree),
    ok = check_util:check_height(Tree),
    ok.

delete6(_) ->
    Tree = tree2(),
    Act = avl_tree:delete(32, Tree),
    Exp = {8, {3, {62, c},
               {2, {44, a},
                {0, {17, b}, nil, nil},
                {1, {50, e},
                 {0, {48, g}, nil, nil},
                 {0, {54, h}, nil, nil}
                }
               },
               {1, {78, f},
                nil,
                {0, {88, i}, nil, nil}}
            }},
    Exp = Act,
    ok = check_util:check_height(Act),
    ok.

delete7(_) ->
    Tree = tree3(),
    Act = avl_tree:delete(40, Tree),
    Exp = {11,
           {3,
            {50,e},
            {2,
             {44,a},
             {1,{32,d},{0,{17,b},nil,nil},{0,{37,j},nil,nil}},
             {1,{48,g},nil,{0,{49,l},nil,nil}}},
            {2,
             {62,c},
             {0,{54,h},nil,nil},
             {1,{78,f},nil,{0,{88,i},nil,nil}}}}},
    Exp = Act,
    ok = check_util:check_height(Act),
    ok.

delete8(_) ->
    Tree = tree4(),
    Act = avl_tree:delete(40, Tree),
    Exp = {11,
           {3,
            {50,e},
            {2,
             {44,a},
             {1,{32,d},{0,{17,b},nil,nil},{0,{37,j},nil,nil}},
             {0,{48,g},nil,nil}},
            {2,
             {62,c},
             {1,{54,h},nil,{0,{55,l},nil,nil}},
             {1,{78,f},nil,{0,{88,i},nil,nil}}}}},
    Exp = Act,
    ok = check_util:check_height(Act),
    ok.

delete9(_) ->
    Tree = tree5(),
    ok = check_util:check_height(Tree),
    To_del = [
              595, 475, 793, 141, 528, 736, 570, 162, 268, 485,
              52, 922, 603, 148, 365, 279, 636, 334, 442, 416
             ],
    lists:foldl(fun delete_one_key/2, Tree, To_del),
    ok.

delete10(_) ->
    Tree = tree6(),
    ok = check_util:check_height(Tree),
    To_del = [334, 442, 416],
    lists:foldl(fun delete_one_key/2, Tree, To_del),
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
    Tree = avl_tree:map(F, Tree),
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

values1(_) ->
    Exp = [Value || {_, Value} <-  lists:sort(items1())],
    Exp = avl_tree:values(tree1()),
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

items2() ->
    [
     {44, a},
     {17, b},
     {62, c},
     {32, d},
     {50, e},
     {78, f},
     {48, g},
     {54, h},
     {88, i}
    ].

tree2() ->
    avl_tree:from_list(items2()).

items3() ->
    [
     {44, a},
     {17, b},
     {62, c},
     {32, d},
     {50, e},
     {78, f},
     {48, g},
     {54, h},
     {88, i},
     {37, j},
     {40, k},
     {49, l}
    ].

tree3() ->
    avl_tree:from_list(items3()).

items4() ->
    [
     {44, a},
     {17, b},
     {62, c},
     {32, d},
     {50, e},
     {78, f},
     {48, g},
     {54, h},
     {88, i},
     {37, j},
     {40, k},
     {55, l}
    ].

tree4() ->
    avl_tree:from_list(items4()).

tree5() ->
    {30,
     {5, {442,47},
      {4, {268,639},
       {3, {148,108},
        {2, {24,241},
         {0,{4,240},nil,nil},
         {1,
          {52,48}, nil,
          {0, {141,33}, nil,nil}}},
        {1, {162,853},
         {0, {151,648}, nil,nil},
         {0, {183,468}, nil,nil}}},
       {2, {365,467},
        {1, {279,187},
         nil,
         {0, {334,922}, nil,nil}},
        {1, {400,63},
         nil,
         {0, {416,325}, nil,nil}}}},
      {4, {734,164},
       {3, {570,250},
        {2, {528,716},
         {1, {485,753},
          {0, {475,261}, nil,nil},
          nil},
         {0, {537,377}, nil,nil}},
        {1, {603,71},
         {0, {595,382}, nil,nil},
         {0, {636,689}, nil,nil}}},
       {2, {804,980},
        {1, {789,365},
         {0, {736,970}, nil,nil},
         {0, {793,478}, nil,nil}},
        {1, {922,482},
         nil,
         {0, {981,48}, nil,nil}}}}}}
        .

tree6() ->
    {13,
     {4, {442,47},
      {3, {334,922},
       {2, {151,648},
        {1,{24,241},{0,{4,240},nil,nil},nil},
        {0,{183,468},nil,nil}},
       {1,{400,63},nil,{0,{416,325},nil,nil}}},
      {2, {734,164},
       {0,{537,377},nil,nil},
       {1, {804,980},
        {0,{789,365},nil,nil},
        {0,{981,48},nil,nil}}}}}
        .

delete_one_key(Key, Tree) ->
    ok = check_util:check_height(Tree),
    Res = avl_tree:delete(Key, Tree),
    ok = check_util:check_height(Res),
    Res.

