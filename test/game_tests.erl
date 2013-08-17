-module (game_tests).
-include_lib ("eunit/include/eunit.hrl").


init_test() ->
  Dict = dict:from_list([{{1,1},1},{{1,2},0},{{1,3},0},{{2,1},1},{{2,2},1},{{2,3},0}]),
  {Grid, Width, Height} = game:init([{{1,1},1},{{1,2},0},{{1,3},0},{{2,1},1},{{2,2},1},{{2,3},0}]),
  ?assertEqual(Dict, Grid),
  ?assertEqual(2, Width),
  ?assertEqual(3, Height).


alive_test() ->
  ?assert(game:alive(1)),
  ?assertNot(game:alive(0)).


neighbors_test() ->
  Board = game:init([ {{1,1},1}, {{1,2},0}, {{1,3},1}, {{1,4},0},
                          {{2,1},0}, {{2,2},1}, {{2,3},1}, {{2,4},0},
                          {{3,1},0}, {{3,2},0}, {{3,3},1}, {{3,4},0},
                          {{4,1},1}, {{4,2},0}, {{4,3},1}, {{4,4},0} ]),

  ?assertEqual(4, game:neighbors({2,2}, Board)),
  ?assertEqual(3, game:neighbors({2,3}, Board)),
  ?assertEqual(5, game:neighbors({3,2}, Board)),
  ?assertEqual(3, game:neighbors({3,3}, Board)).


state_test() ->
  Board = game:init([{{1,1},1},{{1,2},0},{{2,1},0},{{2,2},1}]),
  ?assertEqual(1, game:state({1,1}, Board)),
  ?assertEqual(0, game:state({1,2}, Board)),
  ?assertEqual(0, game:state({2,1}, Board)),
  ?assertEqual(1, game:state({2,2}, Board)),
  ?assertEqual(1, game:state({3,1}, Board)),
  ?assertEqual(0, game:state({2,3}, Board)).


