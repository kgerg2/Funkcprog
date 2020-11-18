-module(taskfarm).
-export([eval/1, eval/2]).

eval(Job) -> eval(Job, erlang:system_info(logical_processors_available)).

eval({job, F, List}, K) ->
    Dispatcher = spawn(fun() -> dispatcher(List, K) end),
    Collector = spawn(fun() -> collector([]) end),
    [spawn(fun() -> worker(F, Dispatcher, Collector) end) || _ <- lists:seq(1, K)],
    % [Dispatcher ! {ready, W} || W <- Workers],
    % receive Res -> Res end.
    Collector.

collector(Result) ->
    receive
        {give_me, From} ->
            From ! Result,
            collector(Result);
        {stop, From} -> From ! Result;
        {result, Data} -> collector([Data | Result])
    end.

dispatcher([], 0) -> io:format("Dispatcher terminated.~n");
dispatcher([], K) -> 
    receive
        {ready, From} ->
            From ! stop,
            dispatcher([], K - 1)
    end;
dispatcher([H|T], K) ->
    receive
        {ready, From} ->
            From ! {data, H},
            dispatcher(T, K)
    end.

worker(F, D, C) ->
    D ! {ready, self()},
    receive
        {data, Data} ->
            C ! {result, {Data, catch F(Data)}},
            worker(F, D, C);
        stop -> io:format("Worker ~p terminated.~n", [self()])
    end.