-module(egol_runner).

-export([random_start/2, next_gen/3]).

random_start(Width, Height) ->
    random:seed(os:timestamp()),
    CellStates = [random:uniform(2) - 1 || _ <- lists:seq(1, Width*Height)],
    BoardAsList = [{{X,Y},Z} || X <- lists:seq(1, Width),
                                Y <- lists:seq(1, Height),
                                Z <- CellStates],
    egol_game:init_from_list(BoardAsList).


next_gen(_Board, Generation, MaxGenerations) when MaxGenerations < Generation ->
    ok;
next_gen(Board, Generation, MaxGenerations) ->
    NextBoard = egol_game:evolve(Board),
    next_gen(NextBoard, Generation+1, MaxGenerations).

