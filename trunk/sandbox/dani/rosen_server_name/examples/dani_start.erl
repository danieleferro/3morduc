-module(dani_start).

-export([go/0]).

go() ->
    spawn_link(dani, go, [rosen1, robot1]),
    spawn_link(dani, go, [rosen2, robot2]).
    
