-module(ring).
-export([run/1, worker/1]).

run(N) when N > 0 ->
    Main = self(),
    % First = spawn(fun() -> worker(Main, N-1) end),
    % [First|_] = [spawn(fun() -> worker(Main, N-1) end) || _ <- lists:seq(1, N)],
    First = lists:foldl(fun(_, Acc) -> spawn(ring, worker, [Acc]) end, Main, lists:seq(1, N)),
    First ! ok,
    receive
        ok -> ok
    end.

% worker(Main, 0) -> 
%     receive
%         ok -> Main ! ok
%     end;
% worker(Main, N) ->
%     receive
%         ok -> spawn(fun() -> worker(Main, N-1) end) ! ok
%     end.

worker(Next) ->
    receive
        ok -> Next ! ok
    end.