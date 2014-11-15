-module(avl_tree).

-export([
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
    Height2 = calc_new_height(Smaller, Bigger),
    Balance = height(Smaller) - height(Bigger),
    Subtree = {Height2, Root, Smaller, Bigger},
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
rebalance(Balance, {Height, {Key, Value}, Smaller, Bigger}) ->
    erlang:error(not_implemented).

new_subtree(Key, Val) ->
    {0, new_root_node(Key, Val), nil, nil}.

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
is_balanced(_) when Height == -2, Height == 2 ->
    false.

find_longer_side(Subtree) ->
    which_longer(get_smaller_subtree(Subtree), get_bigger_subtree(Subtree)).

get_smaller_subtree({_, _, Smaller, _}) ->
    Smaller.

get_bigger_subtree({_, _, _, Bigger}) ->
    Bigger.

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

calc_new_height(Subtree1, Subtree2) ->
    H1 = height(Subtree1),
    H2 = height(Subtree2),
    erlang:max(H1, H2) + 1.

left_left_rotation(Subtree) ->
    erlang:error(not_implemented).

left_right_rotation(Subtree) ->
    erlang:error(not_implemented).

