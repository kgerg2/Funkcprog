-module(pingpong).
-export([run/0, proc1/0, proc2/1]).

run() ->
    Pid1 = spawn(pingpong, proc1, []),
    spawn(pingpong, proc2, [Pid1]).

proc1() ->
    io:format("Started proc1: ~p~n", [self()]),
    receive
        {ping, From} ->
            io:format("ping"),
            From ! pong
    end.

proc2(Pid) ->
    io:format("Started proc2: ~p~n", [self()]),
    Pid ! {ping, self()},
    receive
        pong -> io:format("pong")
    end.