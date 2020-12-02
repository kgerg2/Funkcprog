-module(client).
-export([start/1]).

start(Name) when is_list(Name) ->
    case chatsrv:login(Name) of
        logged_in ->
            LoopPid = self(),
            spawn(fun() -> input(LoopPid) end),
            client_loop();
        timeout -> io:format("Connection timed out~n")
    end.

client_loop() ->
    receive
        stop ->
            chatsrv:logout(),
            io:format("Client terminated~n");
        {send, Text} ->
            chatsrv:send(Text),
            client_loop();
        {msg, Msg} ->
            io:format("~p~n", [Msg]),
            client_loop()
    end.

input(Pid) ->
    case lists:droplast(io:get_line("--> ")) of
        "quit" -> Pid ! stop;
        Text ->
            Pid ! {send, Text},
            input(Pid)
    end.