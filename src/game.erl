-module (game).

-export ([evolve/1]).

%% Export all functions for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.


%%
%%
%%
evolve(Grid) ->
  dict:map(fun(K,V) ->
    rules:evaluate(neighbors(K, Grid), alive(V)) end, Grid).

%%
%%
%%
alive(Status) when Status =:= 1 -> true;
alive(_) -> false.

%%
%%
%%
neighbors(K, Grid) ->
  % K is a point (tuple)
  2.


% the dict has tubple X,Y as key and status as value