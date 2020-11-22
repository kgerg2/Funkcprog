-module(chatsrv).
-export([start/0]).
-export([]).

start() ->
    register(chatsrv, spawn(fun init/0)).

init() ->
    io:format("Chat server started."),
    srv_loop(#{}). % #{Pid => Nickname}

srv_loop(State) ->
    receive
        {login, {Pid, Nick}} ->
            Pid ! ok,
            srv_loop(State#{Pid => Nick});
        {send, {Pid, Msg}} ->
            % [Pid ! Msg || maps:keys()];
            #{Pid := Nick} = State,
            NMsg = Nick ++ ": " ++ Msg,
            maps:fold(fun (To, _, _) -> To ! NMsg end, ok, State),
            srv_loop(State);
        {logout, Pid} ->
            NewState = maps:remove(Pid, State),
            srv_loop(NewState);
        dump -> io:format("Server state: ~p~n", [State]);
        stop -> terminate(State)
    end.

terminate(State) -> 
    io:format("Server terminated with state ~p~n", [State]).