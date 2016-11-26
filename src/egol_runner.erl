-module(egol_runner).

-export([random_start/2, next_gen/3]).

random_start(Width, Height) ->
    CellStates = [rand:uniform(2) - 1 || _ <- lists:seq(1, Width*Height)],
    CoordsList = [{X,Y} || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)],
    BoardAsList = lists:zip(CoordsList, CellStates),
    egol_game:init_from_list(BoardAsList).


next_gen(_Board, Generation, MaxGenerations) when MaxGenerations < Generation -> ok;
next_gen(Board, Generation, MaxGenerations) ->
    NextBoard = egol_game:evolve(Board),
    timer:sleep(50),
    io:format("\e[2J"),
    io:format("Generation ~w~n", [Generation]),
    egol_print:print_game(NextBoard),
    next_gen(NextBoard, Generation+1, MaxGenerations).
