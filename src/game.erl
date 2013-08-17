-module (game).

-export ([init/1]).
-export ([evolve/1]).

%% Export all functions for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

%%--------------------------------------------------------------------
%% @doc
%% init takes one List argument which represents the Game's state.
%% From this list it constructs the Game's Board plus some meta
%% information, namely Width and Height of the Game's Board.
%% @spec init(List) -> GameBoard
%%       List = [ {{ integer(), integer() }, integer() } ]
%%       GameBoard = []
%% @TODO Document GameBoard
%% @end
%% information, namely the Board's Width and Height
%%--------------------------------------------------------------------
init(L) ->
  Grid = dict:from_list(L),
  Points = dict:fetch_keys(Grid),
  XCoords = [X || {X,_} <- Points],
  YCoords = [Y || {_,Y} <- Points],
  [Grid, lists:max(XCoords), lists:max(YCoords)].

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


%%--------------------------------------------------------------------
%% @doc
%% neighbors takes two arguments:
%% 1) the cell for which to find the living neighbors (a tuple of X,Y)
%% 2) the grid in which to the the neighbors
%% @spec neighbors( { integer(), integer() }, dict ) -> integer()
%% @end
%%--------------------------------------------------------------------
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
%% @spec state( { integer(), integer() }, dict ) -> 1 | 0
%% @end
%%--------------------------------------------------------------------
state(Cell, Grid) ->
  % X,Y = Cell,
  {ok,Value} = dict:find(Cell, Grid),
  Value.
