-module(chatsrv).

% Server interface
-export([start/0]).

% Client interface
-export([login/1, logout/0, send/1]).

-define(SRV, {chatsrv, 'srv@192.168.1.49'}).

start() ->
    register(chatsrv, spawn(fun init/0)).

init() ->
    io:format("Chat server started."),
    srv_loop(#{}). % #{Pid => Nickname}

login(Nick) ->
    ?SRV ! {login, {self(), Nick}},
    receive
        ok -> logged_in
    after 5000 -> timeout
    end.

logout() ->
    ?SRV ! {logout, self()}.

send(Msg) ->
    ?SRV ! {send, {self(), Msg}}.

srv_loop(State) ->
    receive
        {login, {Pid, Nick}} ->
            Pid ! ok,
            srv_loop(State#{Pid => Nick});
        {send, {Pid, Msg}} ->
            % [Pid ! Msg || maps:keys()];
            #{Pid := Nick} = State,
            NMsg = Nick ++ ": " ++ Msg,
            maps:fold(fun (To, _, _) -> To ! {msg, NMsg} end, ok, State),
            srv_loop(State);
        {logout, Pid} ->
            NewState = maps:remove(Pid, State),
            srv_loop(NewState);
        dump ->
            io:format("Server state: ~p~n", [State]),
            srv_loop(State);
        stop -> terminate(State)
    end.

terminate(State) -> 
    io:format("Server terminated with state ~p~n", [State]).