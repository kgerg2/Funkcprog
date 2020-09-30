-module(second).
-compile(export_all).
% -akarmi("Tetszoleges").

count_1([]) -> 0;
% count_1([H|T]) when H =:= 1 ->
count_1([1|T]) -> 1 + count_1(T);
count_1([_|T]) -> count_1(T).

count_1if([]) -> 0;
count_1if([H|T]) ->
    if H =:= 1 -> 1 + count_1(T);
       true    -> count_1(T)
    end.

% count_1lc(L, X) -> length([ok || 1 <- L]).
count_1lc(L) ->
    % Felesleges = length([E || E <- L, E =:= 1]),
    length([ok || 1 <- L]).

% count(E, L) -> length([0 || X <- L, E =:= X]).

count(_, []) -> 0;
% count(E, [H|T]) when E =:= H -> 1 + count(E, T);
count(E, [E|T]) -> 1 + count(E, T);
count(E, [_|T]) -> count(E, T).

% -export([count_tail/2]).

count_tail(E, L) -> count_tail(E, L, 0).

count_tail(_, [], Res) -> Res;
count_tail(E, [E|T], Res) -> count_tail(E, T, 1 + Res);
count_tail(E, [_|T], Res) -> count_tail(E, T, Res).