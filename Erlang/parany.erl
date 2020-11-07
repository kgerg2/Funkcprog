-module(parany).
-export([parany/2]).

parany(F, L) ->
    Main = self(),
    Pid = spawn(fun() -> recTrue(Main, length(L)) end),
    [spawn(fun() -> Pid ! F(E) end) || E <- L],
    receive A -> A end.

recTrue(Pid, 0) -> Pid ! false;
recTrue(Pid, N) ->
    receive
        true -> Pid ! true;
        false -> recTrue(Pid, N-1)
    end.
