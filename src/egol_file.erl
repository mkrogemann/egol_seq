-module(egol_file).

-export([save_game/2, load_game/1]).

save_game(File, Board) ->
    {ok,S} = file:open(File, write),
    io:format(S, "~p.~n", [Board]),
    file:close(S).

load_game(File) ->
    {ok,Data} = file:consult(File),
    hd(Data).

