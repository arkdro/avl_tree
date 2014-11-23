-module(check_util).

-compile([export_all]).

check_height({_, Subtree}) ->
    check_height1(Subtree).

check_height1(nil) ->
    ok;
check_height1({H, Root, Smaller, Bigger}) ->
    Res1 = check_height1(Smaller),
    Res2 = check_height1(Bigger),
    case {Res1, Res2} of
        {ok, ok} ->
            Height1 = avl_tree:height(Smaller),
            Height2 = avl_tree:height(Bigger),
            Max_inc = max(Height1, Height2) + 1,
            case check_balance(Height1, Height2) of
                ok when Max_inc =:= H ->
                    ok;
                ok ->
                    {error, {bad_current_height, [{h, H}, {h1, Height1},
                                                  {h2, Height2},
                                                  {root, Root}]}};
                {error, D} ->
                    {error, {bad_balance, [{delta, D}, {h1, Height1},
                                           {h2, Height2}, {root, Root}]}}
            end;
        {ok, {error, Reason} = Error} ->
            Error;
        {{error, Reason} = Error, ok} ->
            Error;
        {{error, R1}, {error, R2}} ->
            {error, {double_error, R1, R2}}
    end.

check_balance(H1, H2) ->
    D = H1 - H2,
    case D of
        -1 ->
            ok;
        0 ->
            ok;
        1 ->
            ok;
        _ ->
            {error, D}
    end.

