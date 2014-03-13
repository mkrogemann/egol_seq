-module(egol_game).

-export([init_from_list/1,
         evolve/1,
         init_from_file/1]).

%% Export all functions for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

%%--------------------------------------------------------------------
%% @doc
%% init_from_list takes one List argument which represents the Game's state.
%% From this list it constructs the Game's Board plus some meta
%% information, namely Width and Height of the Game's Board.
%% @spec init_from_list(List) -> Board
%%       List = [ {{ integer(), integer() }, integer() } ]
%% @TODO Document Board once I grasp EDoc
%% @end
%%--------------------------------------------------------------------
init_from_list(L) ->
    Grid = dict:from_list(L),
    Points = dict:fetch_keys(Grid),
    XCoords = [X || {X,_} <- Points],
    YCoords = [Y || {_,Y} <- Points],
    Board = {Grid, lists:max(XCoords), lists:max(YCoords)},
    Board.

init_from_file(File) ->
    Board = egol_file:load_game(File),
    Board.


%%--------------------------------------------------------------------
%% @doc
%% evolve takes one argument (Board) which represents the Game's state.
%% By applying the rules of the Game, the next state gets computed
%% and returned as a new Board.
%% @spec evolve(Board) -> Board
%% @end
%%--------------------------------------------------------------------
evolve(Board) ->
    {Grid,Width,Height} = Board,
    NewGrid = dict:map(fun(K,V) ->
                egol_rules:evaluate(neighbors(K, Board), V) end, Grid),
    {NewGrid,Width,Height}.


%%--------------------------------------------------------------------
%% @doc
%% neighbors takes two arguments:
%% 1) the Cell for which to find the living neighbors (a tuple of X,Y)
%% 2) the Board (tuple) containing the Grid in which to lookup the
%% neighbors. By neighbors we here mean neighboring cells that are
%%alive.
%% Each cell has eight adjacent cells. The formatting chosen below
%% attempts to visualize the algorithm.
%% @spec neighbors( Cell, Board )  -> integer()
%% @end
%%--------------------------------------------------------------------
neighbors(Cell, Board) ->
    {X,Y} = Cell,
    lists:sum(
        [state({X-1,Y-1},Board), state({X-1,Y},Board), state({X-1,Y+1},Board),
         state({X,  Y-1},Board),                       state({X,  Y+1},Board),
         state({X+1,Y-1},Board), state({X+1,Y},Board), state({X+1,Y+1},Board)]).


%%--------------------------------------------------------------------
%% @doc
%% state takes two arguments:
%% 1) the Cell to find (a tuple of X,Y coordinates)
%% 2) the Board, containing the Game's Grid in which to find the cell
%% @spec state( Cell, Board ) -> 1 | 0
%% @end
%%--------------------------------------------------------------------
state(Cell, Board) ->
    {Grid,_,_} = Board,
    {X,Y} = wrap_if_required(Cell, Board),
    {ok,Value} = dict:find({X,Y}, Grid),
    Value.


%%--------------------------------------------------------------------
%% @doc
%% The coordinates need to be wrapped around the edges of the Game's
%% Grid so that it appears to be infinite. This means that cells that
%% 'move' beyond any border of the Grid reappear at the opposite
%% border.
%% @spec wrap_if_required( Cell, Board ) -> Cell
%% @end
%%--------------------------------------------------------------------
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

