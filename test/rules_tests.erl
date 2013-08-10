-module (rules_tests).
-include_lib ("eunit/include/eunit.hrl").

evaluate_4_neighbors_test() ->
  ?assertNot(rules:evaluate(4, true)),
  ?assertNot(rules:evaluate(4, false)).

evaluate_3_neighbors_test() ->
  ?assert(rules:evaluate(3, true)),
  ?assert(rules:evaluate(3, false)).

evaluate_2_neighbors_test() ->
  ?assert(rules:evaluate(2, true)),
  ?assertNot(rules:evaluate(2, false)).

evaluate_1_neighbor_test() ->
  ?assertNot(rules:evaluate(1, true)),
  ?assertNot(rules:evaluate(1, false)).

evaluate_0_neighbors_test() ->
  ?assertNot(rules:evaluate(0, true)),
  ?assertNot(rules:evaluate(0, false)).
