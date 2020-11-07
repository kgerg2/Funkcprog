-module(parany).
-export([pany/2]).

pany(F, L) ->
    Main = self(),
    Pid = spawn(fun() -> recTrue(Main, length(L)) end),
    [spawn(fun() -> Pid ! {E, F(E)} end) || E <- L],
    receive A -> A end.

recTrue(Pid, 0) -> Pid ! false;
recTrue(Pid, N) ->
    receive
        {E, true} -> Pid ! {true, E};
        {_, false} -> recTrue(Pid, N-1)
    end.
