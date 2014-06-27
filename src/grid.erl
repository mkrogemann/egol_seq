-module (grid).

-export([init/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% spawn cell processes and store in ets
init(Width, Height) ->
    CellCoords = [{X, Y} || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)],
    spawn_cells(CellCoords).

spawn_cells([]) ->
    ok;
spawn_cells([H|T]) ->
    Pid = spawn(cell, loop, [1]),
    ets:insert(cells, {H, Pid}),
    spawn_cells(T).

