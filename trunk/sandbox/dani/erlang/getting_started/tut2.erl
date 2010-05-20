%% Threads of execution in Erlang share no data,
%% that's why we call them processes

-module(tut2).
-export([start/0, say_st/2]).
-export([ping/1, pong/0]).

%% process di stampa
say_st(_, 0) ->
    done;
say_st(What, N) ->
    io:format("~p~n", [What]),
    say_st(What, N-1).

%% process di ping e pong

ping(0) ->
    pong ! finished,
    io:format("ping finished~n", []);

ping(N) ->
    pong ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.


start() ->
    register(pong, spawn(tut2, pong, [])),
    spawn(tut2, ping, [3]),
    ok.
