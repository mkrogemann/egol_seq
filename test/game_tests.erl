-module (game_tests).
-include_lib ("eunit/include/eunit.hrl").

init_test() ->
  Dict = dict:from_list([{{1,1},1},{{1,2},0},{{1,3},0},{{2,1},1},{{2,2},1},{{2,3},0}]),
  [Grid, Width, Height] = game:init([{{1,1},1},{{1,2},0},{{1,3},0},{{2,1},1},{{2,2},1},{{2,3},0}]),
  ?assertEqual(Dict, Grid),
  ?assertEqual(2, Width),
  ?assertEqual(3, Height).


alive_test() ->
  ?assert(game:alive(1)),
  ?assertNot(game:alive(0)).

neighbors_test() ->
  Dict = dict:from_list([ {{1,1},1}, {{1,2},0}, {{1,3},1}, {{1,4},0},
                          {{2,1},0}, {{2,2},1}, {{2,3},1}, {{2,4},0},
                          {{3,1},0}, {{3,2},0}, {{3,3},1}, {{3,4},0},
                          {{4,1},1}, {{4,2},0}, {{4,3},1}, {{4,4},0} ]),

  ?assertEqual(4, game:neighbors({2,2}, Dict)),
  ?assertEqual(3, game:neighbors({2,3}, Dict)),
  ?assertEqual(5, game:neighbors({3,2}, Dict)),
  ?assertEqual(3, game:neighbors({3,3}, Dict)).

state_test() ->
  Dict = dict:from_list([{{1,1},1},{{1,2},0}]),
  ?assertEqual(1, game:state({1,1}, Dict)),
  ?assertEqual(0, game:state({1,2}, Dict)).


