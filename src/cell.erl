-module(cell).

-export([init/1, loop/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

%%init(random) ->
%%    init(random:uniform(2)-1);
init(State) ->
    loop(State).

loop(State) ->
    receive
        next ->
            io:format("next..."),
            loop(State)
    end.

