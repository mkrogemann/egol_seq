-module (game_tests).
-include_lib ("eunit/include/eunit.hrl").


init_test() ->
  ExpectedGrid = dict:from_list([ {{1,1},1},{{2,1},0},{{3,1},0},
                                  {{1,2},1},{{2,2},1},{{3,2},0} ]),
  {ActualGrid, Width, Height} =
              game:init([ {{1,1},1},{{2,1},0},{{3,1},0},
                          {{1,2},1},{{2,2},1},{{3,2},0} ]),

  ?assertEqual(ExpectedGrid, ActualGrid),
  ?assertEqual(3, Width),
  ?assertEqual(2, Height).


evolve_test() ->
  Board = game:init([ {{1,1},1}, {{2,1},0}, {{3,1},1}, {{4,1},0},
                      {{1,2},0}, {{2,2},1}, {{3,2},1}, {{4,2},0},
                      {{1,3},0}, {{2,3},0}, {{3,3},1}, {{4,3},0},
                      {{1,4},1}, {{2,4},0}, {{3,4},1}, {{4,4},0} ]),

  ExpectedGrid = dict:from_list([ {{1,1},1}, {{2,1},0}, {{3,1},1}, {{4,1},0},
                                  {{1,2},0}, {{2,2},0}, {{3,2},1}, {{4,2},0},
                                  {{1,3},0}, {{2,3},0}, {{3,3},1}, {{4,3},0},
                                  {{1,4},0}, {{2,4},0}, {{3,4},1}, {{4,4},0} ]),
  {ActualGrid,_,_} = game:evolve(Board),
  ?assertEqual(ExpectedGrid, ActualGrid).


neighbors_test() ->
  Board = game:init([ {{1,1},1}, {{2,1},0}, {{3,1},1}, {{4,1},0},
                      {{1,2},0}, {{2,2},1}, {{3,2},1}, {{4,2},0},
                      {{1,3},0}, {{2,3},0}, {{3,3},1}, {{4,3},0},
                      {{1,4},1}, {{2,4},0}, {{3,4},1}, {{4,4},0} ]),

  ?assertEqual(2, game:neighbors({1,1}, Board)),
  ?assertEqual(3, game:neighbors({3,1}, Board)),
  ?assertEqual(5, game:neighbors({4,1}, Board)),
  ?assertEqual(4, game:neighbors({2,2}, Board)),
  ?assertEqual(3, game:neighbors({3,2}, Board)),
  ?assertEqual(5, game:neighbors({2,3}, Board)),
  ?assertEqual(3, game:neighbors({3,3}, Board)),
  ?assertEqual(1, game:neighbors({1,4}, Board)),
  ?assertEqual(5, game:neighbors({4,4}, Board)).


state_test() ->
  Board = game:init([{{1,1},1},{{2,1},0},{{1,2},0},{{2,2},1}]),
  ?assertEqual(1, game:state({1,1}, Board)),
  ?assertEqual(0, game:state({2,1}, Board)),
  ?assertEqual(0, game:state({1,2}, Board)),
  ?assertEqual(1, game:state({2,2}, Board)),
  ?assertEqual(1, game:state({1,3}, Board)),
  ?assertEqual(0, game:state({3,2}, Board)).
