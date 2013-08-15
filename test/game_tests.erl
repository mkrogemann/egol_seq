-module (game_tests).
-include_lib ("eunit/include/eunit.hrl").

alive_test() ->
  ?assert(game:alive(1)),
  ?assertNot(game:alive(0)).

