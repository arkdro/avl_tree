-module(avl_tree).

-export([
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

