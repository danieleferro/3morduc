-module(tut3).
-export([start_ping/1, start_pong/0, ping/2, pong/0, start/1]).

ping(0, Pong_node) ->

    { pong, Pong_node } ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_node) ->

%% We use a tuple {registered_name,node_name} instead
%% of just the registered_name.
%% Dobbiamo conoscere dove si trova il registry.

    { pong, Pong_node } ! { ping, self() },
    
    receive
	
	pong ->
	    io:format("Ping received pong~n", [])
    
    end,
    ping(N-1, Pong_node).


pong() ->

%% Erlang pids contain information about where the process executes
%% so if you know the pid of a process, the "!" operator can be used
%% to send it a message if the process is on the same node or on a
%% different node.

    receive

	{ ping, Ping_ID } ->
	    io:format("Pong received ping~n", []),
	    Ping_ID ! pong,
	    pong();
	
	finished ->
	    io:format("pong finished~n", [])
    end.


start_pong() ->
    register(pong, spawn(tut3, pong, [])).


start_ping(Pong_Node) ->
    
    %% pong node è l'identificativo su cui si trova il registry.
    spawn(tut3, ping, [5, Pong_Node]).


start(Ping_Node) ->
    %% spawn can also be used to start processes in other nodes
    %% funzione da chiamare dal nodo "pong", passando parametro
    %% con nodo "ping" per attivare lì il processo. 

    %% node() ritorna indirizzo del nodo.
    %% L'output sarà ridiretto tutto sul nodo "ping", dove sono
    %% state eseguite le spawn.
    register(pong, spawn(tut3, pong, [])),
    spawn(Ping_Node, tut3, ping, [3, node()]).
