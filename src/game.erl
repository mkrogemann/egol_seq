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

%%--------------------------------------------------------------------
%% @doc
%% alive takes one argument which represents a Cell's status
%% A value of 1 means alive, everything else means not alive
%% @spec alive( integer() ) -> true | false
%% @end
%%--------------------------------------------------------------------
alive(Status) when Status =:= 1 -> true;
alive(_) -> false.

%%
%%
%%
neighbors(Cell, Grid) ->
  {X,Y} = Cell,
  lists:sum(
    [state({X-1,Y-1}, Grid), state({X-1,Y}, Grid), state({X-1,Y+1}, Grid),
     state({X,Y-1}, Grid), state({X,Y+1}, Grid),
     state({X+1,Y-1}, Grid), state({X+1,Y}, Grid), state({X+1,Y+1}, Grid)]).


%%--------------------------------------------------------------------
%% @doc
%% state takes two arguments:
%% 1) the cell to find (a tuple of X,Y coordinates)
%% 2) the grid in which to find the cell
%% @spec alive( { integer(), integer() }, dict ) -> 1 | 0
%% @end
%%--------------------------------------------------------------------
state(Cell, Grid) ->
  {ok,Value} = dict:find(Cell, Grid),
  Value.
