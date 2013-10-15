-module (egol_file_tests).
-include_lib ("eunit/include/eunit.hrl").

save_load_test() ->
  ExpectedBoard = egol_game:init_from_list([
    {{1,1},1},{{2,1},0},{{3,1},0},
    {{1,2},1},{{2,2},1},{{3,2},0} ]),
  egol_file:save_game("board.dat", ExpectedBoard),
  LoadedBoard = egol_file:load_game("board.dat"),
  ?assertEqual(ExpectedBoard, LoadedBoard).

