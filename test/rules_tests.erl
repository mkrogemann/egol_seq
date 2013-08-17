-module (rules_tests).
-include_lib ("eunit/include/eunit.hrl").

evaluate_4_neighbors_test() ->
  ?assertEqual(0, rules:evaluate(4, true)),
  ?assertEqual(0, rules:evaluate(4, false)).

evaluate_3_neighbors_test() ->
  ?assertEqual(1, rules:evaluate(3, true)),
  ?assertEqual(1, rules:evaluate(3, false)).

evaluate_2_neighbors_test() ->
  ?assertEqual(1, rules:evaluate(2, true)),
  ?assertEqual(0, rules:evaluate(2, false)).

evaluate_1_neighbor_test() ->
  ?assertEqual(0, rules:evaluate(1, true)),
  ?assertEqual(0, rules:evaluate(1, false)).

evaluate_0_neighbors_test() ->
  ?assertEqual(0, rules:evaluate(0, true)),
  ?assertEqual(0, rules:evaluate(0, false)).
