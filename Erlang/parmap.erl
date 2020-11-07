-module(parmap).
-compile(export_all).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

map(F, [H|T]) ->
    [F(H) | map(F, T)];
map(_, []) -> [].

parmap0(F, L) ->
    Main = self(),
    [spawn(fun() -> Main ! {E, F(E)} end) || E <- L],
    [receive {E, Res} -> Res end || E <- L].

parmap1(F, L) ->
    Main = self(),
    Workers = [spawn(fun() -> Main ! {self(), F(E)} end) || E <- L],
    [receive {W, Res} -> Res end || W <- Workers].