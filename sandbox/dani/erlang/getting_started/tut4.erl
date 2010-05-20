-module(tut4).
-export([start/1,  ping/2, pong/0]).

ping(N, Pong_Pid) ->

    %% ping è chiamato solo una volta
    %% per creare inizialmente il link con pong
    link(Pong_Pid), 

    ping1(N, Pong_Pid).

ping1(0, _) ->

    %% abnormal exit
    exit(general_reason);


ping1(N, Pong_Pid) ->

    Pong_Pid ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping1(N - 1, Pong_Pid).


pong() ->

    %% pong è chiamato solo una volta
    %% per gestire le abnormal exit provenienti
    %% da altri lik
    process_flag(trap_exit, true), 
    pong1().

pong1() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong1();
        {'EXIT', From, Reason} ->
            io:format("pong exiting, got ~p~n", [{'EXIT', From, Reason}])
            %% pong1 ends here
    end.

start(Ping_Node) ->
    PongPID = spawn(tut4, pong, []),
    spawn(Ping_Node, tut4, ping, [3, PongPID]).
