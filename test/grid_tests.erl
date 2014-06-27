-module(grid_tests).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    ets:new(cells, [set, named_table]),
    grid:init(3,3),
    EtsInfoList = ets:info(cells),
    {_, Size} = lists:nth(6, EtsInfoList),
    ?assertEqual(9, Size).

