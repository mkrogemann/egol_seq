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
%% @spec init(List) -> Board
%%       List = [ {{ integer(), integer() }, integer() } ]
%%       Board = []
%% @TODO Document Board once I grasp EDoc
%% @end
%% information, namely the Board's Width and Height
%%--------------------------------------------------------------------
init(L) ->
  Grid = dict:from_list(L),
  Points = dict:fetch_keys(Grid),
  XCoords = [X || {X,_} <- Points],
  YCoords = [Y || {_,Y} <- Points],
  Board = {Grid, lists:max(XCoords), lists:max(YCoords)},
  Board.

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
neighbors(Cell, Board) ->
  {X,Y} = Cell,
  lists:sum(
    [state({X-1,Y-1}, Board), state({X-1,Y}, Board), state({X-1,Y+1}, Board),
     state({X,Y-1}, Board), state({X,Y+1}, Board),
     state({X+1,Y-1}, Board), state({X+1,Y}, Board), state({X+1,Y+1}, Board)]).


%%--------------------------------------------------------------------
%% @doc
%% state takes two arguments:
%% 1) the cell to find (a tuple of X,Y coordinates)
%% 2) the grid in which to find the cell
%% @spec state( { integer(), integer() }, dict ) -> 1 | 0
%% @end
%%--------------------------------------------------------------------
state(Cell, Board) ->
  {Grid,_,_} = Board,
  {X,Y} = wrap_if_required(Cell, Board),
  {ok,Value} = dict:find({X,Y}, Grid),
  Value.

wrap_if_required(Cell, Board) ->
  {X,Y} = Cell,
  {_,Width,Height} = Board,
  XWrapped = if X > Width -> 1
    ; X < 1 -> Width
    ; true -> X
  end,
  YWrapped = if Y > Height -> 1
    ; Y < 1 -> Height
    ; true -> Y
  end,
  {XWrapped,YWrapped}.
