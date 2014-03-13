-module(egol_rules_tests).
-include_lib("eunit/include/eunit.hrl").

evaluate_4_neighbors_test() ->
    ?assertEqual(0, egol_rules:evaluate(4,1)),
    ?assertEqual(0, egol_rules:evaluate(4,0)).

evaluate_3_neighbors_test() ->
    ?assertEqual(1, egol_rules:evaluate(3,1)),
    ?assertEqual(1, egol_rules:evaluate(3,0)).

evaluate_2_neighbors_test() ->
    ?assertEqual(1, egol_rules:evaluate(2,1)),
    ?assertEqual(0, egol_rules:evaluate(2,0)).

evaluate_1_neighbor_test() ->
    ?assertEqual(0, egol_rules:evaluate(1,1)),
    ?assertEqual(0, egol_rules:evaluate(1,0)).

evaluate_0_neighbors_test() ->
    ?assertEqual(0, egol_rules:evaluate(0,1)),
    ?assertEqual(0, egol_rules:evaluate(0,0)).
