-module(foldable_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [test_fold_map_preserves_list_order].

test_fold_map_preserves_list_order(_Config) ->
    Result = foldable:fold_map(fun(A) -> [A] end, [a, b, c], list),
    ?assertEqual([a, b, c], undetermined:run(Result, list)).
