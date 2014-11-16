-module(avl_tree).

-export([
         take_smallest/1,
         insert/3,
         size/1,
         lookup/2,
         new/0
        ]).

-type tree() :: {size(), subtree()}.
-type size() :: non_neg_integer().
-type subtree() :: {height(), root_node(), left_subtree(), right_subtree()}
                 | nil.
-type height() :: non_neg_integer().
-type root_node() :: {key(), value()}.
-type key() :: term().
-type value() :: term().
-type left_subtree() :: subtree().
-type right_subtree() :: subtree().

-spec new() -> tree().

new() ->
    {0, nil}.

-spec lookup(Key, Tree) -> Value when
      Tree :: tree(),
      Key :: key(),
      Value :: none | {value, value()}.

lookup(Key, {_, Tree}) ->
    subtree_lookup(Key, Tree).

subtree_lookup(_, nil) ->
    none;
subtree_lookup(Key, {_, {Stored_key, Value}, _, _}) when Key == Stored_key ->
    {value, Value};
subtree_lookup(Key, {_, {Stored_key, _}, Left, _}) when Key < Stored_key ->
    subtree_lookup(Key, Left);
subtree_lookup(Key, {_, _, _, Right}) ->
    subtree_lookup(Key, Right).

-spec size(Tree) -> Size when
      Tree :: tree(),
      Size :: size().

size({Size, _}) ->
    Size.

%% assume the key is _not_ in the tree
-spec insert(Key, Value, Tree) -> Tree2 when
      Tree :: tree(),
      Tree2 :: tree(),
      Key :: key(),
      Value :: value().

insert(Key, Value, {Size, Subtree}) ->
    Subtree2 = insert1(Key, Value, Subtree),
    {Size + 1, Subtree2}.

insert1(Key, Value, {_, {Key0, _}=Root, Smaller, Bigger}) when Key < Key0 ->
    Smaller2 = insert1(Key, Value, Smaller),
    proceed_with_balance_common(Root, Smaller2, Bigger);
insert1(Key, Value, {_, {Key0, _}=Root, Smaller, Bigger}) when Key > Key0 ->
    Bigger2 = insert1(Key, Value, Bigger),
    proceed_with_balance_common(Root, Smaller, Bigger2);
insert1(Key, Value, nil) ->
    new_subtree(Key, Value);
insert1(Key, _, _) ->
    erlang:error({key_exists, Key}).

proceed_with_balance_common(Root, Smaller, Bigger) ->
    Balance = calc_balance(Smaller, Bigger),
    Subtree = make_subtree(Root, Smaller, Bigger),
    case is_balanced(Balance) of
        true ->
            Subtree;
        false ->
            rebalance(Balance, Subtree)
    end.

rebalance(2, {_, _, Smaller, _} = Subtree) ->
    case find_longer_side(Smaller) of
        left ->
            left_left_rotation(Subtree);
        right ->
            left_right_rotation(Subtree)
    end;
rebalance(-2, {_, _, _, Bigger} = Subtree) ->
    case find_longer_side(Bigger) of
        left ->
            right_left_rotation(Subtree);
        right ->
            right_right_rotation(Subtree)
    end.

new_subtree(Key, Val) ->
    make_subtree(new_root_node(Key, Val), nil, nil).

make_subtree(Root, Smaller, Bigger) ->
    {calc_new_height(Smaller, Bigger), Root, Smaller, Bigger}.

new_root_node(Key, Val) ->
    {Key, Val}.

height(nil) ->
    -1;
height({_, _, nil, nil}) ->
    0;
height({Height, _, _, _}) ->
    Height.

is_balanced(Height) when Height >= -1, Height =< 1 ->
    true;
is_balanced(Height) when Height == -2; Height == 2 ->
    false.

find_longer_side(Subtree) ->
    which_longer(get_smaller_subtree(Subtree), get_bigger_subtree(Subtree)).

get_smaller_subtree({_, _, Smaller, _}) ->
    Smaller.

get_bigger_subtree({_, _, _, Bigger}) ->
    Bigger.

get_root({_, Root, _, _}) ->
    Root.

which_longer(Subtree1, Subtree2) ->
    H1 = height(Subtree1),
    H2 = height(Subtree2),
    if H1 > H2 ->
            left;
       H1 < H2 ->
            right;
       true ->
            equal
    end.

calc_balance(Subtree1, Subtree2) ->
    height(Subtree1) - height(Subtree2).

calc_new_height(Subtree1, Subtree2) ->
    H1 = height(Subtree1),
    H2 = height(Subtree2),
    erlang:max(H1, H2) + 1.

left_left_rotation({_, Root, Smaller, Bigger}) ->
    Smaller_smaller = get_smaller_subtree(Smaller), %% 3(A, B)
    Smaller_bigger = get_bigger_subtree(Smaller), %% C
    Bigger2 = make_subtree(Root, Smaller_bigger, Bigger), %% 5(C, D)
    make_subtree(get_root(Smaller), Smaller_smaller, Bigger2). %% 4(3, 5)

left_right_rotation(Subtree) ->
    New = left_right_rotation_step1(Subtree),
    left_left_rotation(New).

left_right_rotation_step1({_, Root, Smaller, Bigger}) ->
    Smaller_smaller = get_smaller_subtree(Smaller), %% A
    Smaller_bigger = get_bigger_subtree(Smaller), %% 4(B, C)
    Smaller_bigger_smaller = get_smaller_subtree(Smaller_bigger), %% B
    Smaller_bigger_bigger = get_bigger_subtree(Smaller_bigger), %% C
    Smaller_smaller2 = make_subtree(get_root(Smaller),
                                    Smaller_smaller,
                                    Smaller_bigger_smaller), %% 3(A, B)
    Smaller2 = make_subtree(get_root(Smaller_bigger),
                            Smaller_smaller2,
                            Smaller_bigger_bigger), %% 4(3, C)
    make_subtree(Root, Smaller2, Bigger). %% 5(4, D)

right_left_rotation(Subtree) ->
    New = right_left_rotation_step1(Subtree),
    right_right_rotation(New).

right_left_rotation_step1({_, Root, Smaller, Bigger}) ->
    Bigger_bigger = get_bigger_subtree(Bigger), %% D
    Bigger_smaller = get_smaller_subtree(Bigger), %% 4(B, C)
    Bigger_smaller_smaller = get_smaller_subtree(Bigger_smaller), %% B
    Bigger_smaller_bigger = get_bigger_subtree(Bigger_smaller), %% C
    Bigger_bigger2 = make_subtree(get_root(Bigger),
                                  Bigger_smaller_bigger,
                                  Bigger_bigger), %% 5(C, D)
    Bigger2 = make_subtree(get_root(Bigger_smaller),
                           Bigger_smaller_smaller,
                           Bigger_bigger2), %% 4(B, 5)
    make_subtree(Root, Smaller, Bigger2). %% 3(A, 4)

right_right_rotation({_, Root, Smaller, Bigger}) ->
    Bigger_bigger = get_bigger_subtree(Bigger), %% 5(C, D)
    Bigger_smaller = get_smaller_subtree(Bigger), %% B
    Smaller2 = make_subtree(Root, Smaller, Bigger_smaller), %% 3(A, B)
    make_subtree(get_root(Bigger), Smaller2, Bigger_bigger). %% 4(3, 5)


-spec take_smallest(Tree) -> {Node, Tree2} when
      Tree :: tree(),
      Tree2 :: tree(),
      Node :: root_node().

take_smallest({Size, Subtree}) ->
    {Smallest, Subtree2} = take_smallest1(Subtree),
    New = {Size - 1, Subtree2},
    {Smallest, New}.

take_smallest1({_, Root, nil, Bigger}) ->
    {Root, Bigger};
take_smallest1({_, Root, Smaller, Bigger}) ->
    {Smallest, Smaller2} = take_smallest1(Smaller),
    {Smallest, make_subtree(Root, Smaller2, Bigger)}.

