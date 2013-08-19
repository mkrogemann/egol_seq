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


%%----------------------------------------------------------------------------
%% @doc
%% 5-Cell glider
%%
%%                 |     |      |     |      |     |      |     |      |     |
%%                 |  o  |      | o   |      |  o  |      |     |      |     |
%%                 |o o  |      |  oo |      |   o |      | o o |      |   o |
%%                 | oo  |  ->  | oo  |  ->  | ooo |  ->  |  oo |  ->  | o o |
%%                 |     |      |     |      |     |      |  o  |      |  oo |
%%
%% @end
%%----------------------------------------------------------------------------
evolve_glider_test() ->
  FirstGeneration = game:init([ {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0},
                                {{1,2},0}, {{2,2},0}, {{3,2},1}, {{4,2},0}, {{5,2},0},
                                {{1,3},1}, {{2,3},0}, {{3,3},1}, {{4,3},0}, {{5,3},0},
                                {{1,4},0}, {{2,4},1}, {{3,4},1}, {{4,4},0}, {{5,4},0},
                                {{1,5},0}, {{2,5},0}, {{3,5},0}, {{4,5},0}, {{5,5},0} ]),

  SecondGeneration = game:evolve(FirstGeneration),
  ThirdGeneration = game:evolve(SecondGeneration),
  FourthGeneration = game:evolve(ThirdGeneration),
  FifthGeneration = game:evolve(FourthGeneration),

  ExpectedSecondGrid = dict:from_list([ {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0},
                                        {{1,2},0}, {{2,2},1}, {{3,2},0}, {{4,2},0}, {{5,2},0},
                                        {{1,3},0}, {{2,3},0}, {{3,3},1}, {{4,3},1}, {{5,3},0},
                                        {{1,4},0}, {{2,4},1}, {{3,4},1}, {{4,4},0}, {{5,4},0},
                                        {{1,5},0}, {{2,5},0}, {{3,5},0}, {{4,5},0}, {{5,5},0} ]),

  ExpectedThirdGrid = dict:from_list([  {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0},
                                        {{1,2},0}, {{2,2},0}, {{3,2},1}, {{4,2},0}, {{5,2},0},
                                        {{1,3},0}, {{2,3},0}, {{3,3},0}, {{4,3},1}, {{5,3},0},
                                        {{1,4},0}, {{2,4},1}, {{3,4},1}, {{4,4},1}, {{5,4},0},
                                        {{1,5},0}, {{2,5},0}, {{3,5},0}, {{4,5},0}, {{5,5},0} ]),

  ExpectedFourthGrid = dict:from_list([ {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0},
                                        {{1,2},0}, {{2,2},0}, {{3,2},0}, {{4,2},0}, {{5,2},0},
                                        {{1,3},0}, {{2,3},1}, {{3,3},0}, {{4,3},1}, {{5,3},0},
                                        {{1,4},0}, {{2,4},0}, {{3,4},1}, {{4,4},1}, {{5,4},0},
                                        {{1,5},0}, {{2,5},0}, {{3,5},1}, {{4,5},0}, {{5,5},0} ]),

  ExpectedFifthGrid = dict:from_list([  {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0},
                                        {{1,2},0}, {{2,2},0}, {{3,2},0}, {{4,2},0}, {{5,2},0},
                                        {{1,3},0}, {{2,3},0}, {{3,3},0}, {{4,3},1}, {{5,3},0},
                                        {{1,4},0}, {{2,4},1}, {{3,4},0}, {{4,4},1}, {{5,4},0},
                                        {{1,5},0}, {{2,5},0}, {{3,5},1}, {{4,5},1}, {{5,5},0} ]),

  {ActualSecondGrid,_,_} = SecondGeneration,
  {ActualThirdGrid,_,_} = ThirdGeneration,
  {ActualFourthGrid,_,_} = FourthGeneration,
  {ActualFifthGrid,_,_} = FifthGeneration,


  ?assertEqual(ExpectedSecondGrid, ActualSecondGrid),
  ?assertEqual(ExpectedThirdGrid, ActualThirdGrid),
  ?assertEqual(ExpectedFourthGrid, ActualFourthGrid),
  ?assertEqual(ExpectedFifthGrid, ActualFifthGrid).



%%----------------------------------------------------------------------------
%% @doc
%% Beehive (still life)
%%
%%                |      |      |      |
%%                |  oo  |      |  oo  |
%%                | o  o |  ->  | o  o |
%%                |  oo  |      |  oo  |
%%                |      |      |      |
%%
%% @end
%%----------------------------------------------------------------------------
evolve_beehive_test() ->
  FirstGeneration = game:init([ {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0}, {{6,1},0},
                                {{1,2},0}, {{2,2},0}, {{3,2},1}, {{4,2},1}, {{5,2},0}, {{6,2},0},
                                {{1,3},0}, {{2,3},1}, {{3,3},0}, {{4,3},0}, {{5,3},1}, {{6,3},0},
                                {{1,4},0}, {{2,4},0}, {{3,4},1}, {{4,4},1}, {{5,4},0}, {{6,4},0},
                                {{1,5},0}, {{2,5},0}, {{3,5},0}, {{4,5},0}, {{5,5},0}, {{6,5},0} ]),

  ExpectedSecondGrid = dict:from_list([
                                {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0}, {{6,1},0},
                                {{1,2},0}, {{2,2},0}, {{3,2},1}, {{4,2},1}, {{5,2},0}, {{6,2},0},
                                {{1,3},0}, {{2,3},1}, {{3,3},0}, {{4,3},0}, {{5,3},1}, {{6,3},0},
                                {{1,4},0}, {{2,4},0}, {{3,4},1}, {{4,4},1}, {{5,4},0}, {{6,4},0},
                                {{1,5},0}, {{2,5},0}, {{3,5},0}, {{4,5},0}, {{5,5},0}, {{6,5},0} ]),

  {ActualSecondGrid,_,_} = game:evolve(FirstGeneration),
  ?assertEqual(ExpectedSecondGrid, ActualSecondGrid).


%%----------------------------------------------------------------------------
%% @doc
%% Blinker (an oscillator)
%%
%%                |     |      |     |      |     |
%%                |  o  |      |     |      |  o  |
%%                |  o  |  ->  | ooo |  ->  |  o  |
%%                |  o  |      |     |      |  o  |
%%                |     |      |     |      |     |
%%
%% @end
%%----------------------------------------------------------------------------
evolve_blinker_test() ->
  FirstGeneration = game:init([ {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0},
                                {{1,2},0}, {{2,2},0}, {{3,2},1}, {{4,2},0}, {{5,2},0},
                                {{1,3},0}, {{2,3},0}, {{3,3},1}, {{4,3},0}, {{5,3},0},
                                {{1,4},0}, {{2,4},0}, {{3,4},1}, {{4,4},0}, {{5,4},0},
                                {{1,5},0}, {{2,5},0}, {{3,5},0}, {{4,5},0}, {{5,5},0} ]),

  ExpectedSecondGrid = dict:from_list([
                                {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0},
                                {{1,2},0}, {{2,2},0}, {{3,2},0}, {{4,2},0}, {{5,2},0},
                                {{1,3},0}, {{2,3},1}, {{3,3},1}, {{4,3},1}, {{5,3},0},
                                {{1,4},0}, {{2,4},0}, {{3,4},0}, {{4,4},0}, {{5,4},0},
                                {{1,5},0}, {{2,5},0}, {{3,5},0}, {{4,5},0}, {{5,5},0} ]),

  ExpectedThirdGrid = dict:from_list([
                                {{1,1},0}, {{2,1},0}, {{3,1},0}, {{4,1},0}, {{5,1},0},
                                {{1,2},0}, {{2,2},0}, {{3,2},1}, {{4,2},0}, {{5,2},0},
                                {{1,3},0}, {{2,3},0}, {{3,3},1}, {{4,3},0}, {{5,3},0},
                                {{1,4},0}, {{2,4},0}, {{3,4},1}, {{4,4},0}, {{5,4},0},
                                {{1,5},0}, {{2,5},0}, {{3,5},0}, {{4,5},0}, {{5,5},0} ]),

  SecondGeneration = game:evolve(FirstGeneration),
  ThirdGeneration = game:evolve(SecondGeneration),
  {ActualSecondGrid,_,_} = SecondGeneration,
  {ActualThirdGrid,_,_} = ThirdGeneration,

  ?assertEqual(ExpectedSecondGrid, ActualSecondGrid),
  ?assertEqual(ExpectedThirdGrid, ActualThirdGrid).


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
