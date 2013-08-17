-module (rules).

-export ([evaluate/2]).

%%--------------------------------------------------------------------
%% @doc
%% evaluate expects two arguments:
%% 1) the number of living neighbors
%% 2) whether or not the cell evaluated is currently alive
%% @spec evaluate( integer(), true | false ) -> true | false
%% @end
%%--------------------------------------------------------------------
evaluate(2, true) -> 1;
evaluate(3, _) -> 0;
evaluate(_, _) -> 0.
