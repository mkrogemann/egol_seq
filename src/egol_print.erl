-module(egol_print).

-export([print_game/1]).

-ifdef(TEST).
-compile(export_all).
-endif.


print_game(Board) ->
    {Grid, Width, Height} = Board,
    print_cell(1, 1, Grid, Width, Height).

print_cell(_XCoord, YCoord, _Grid, _Width, Height) when YCoord > Height ->
    ok;
print_cell(XCoord, YCoord, Grid, Width, Height) when XCoord > Width ->
    io:nl(),
    print_cell(1, YCoord+1, Grid, Width, Height);
print_cell(XCoord, YCoord, Grid, Width, Height) ->
    {ok,CellState} = dict:find({XCoord,YCoord}, Grid),
    if CellState =:= 1 ->
            io:fwrite("o")
            ; true -> io:fwrite(" ") end,
    print_cell(XCoord+1, YCoord, Grid, Width, Height).

