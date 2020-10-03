-module(third).
-compile(export_all).
% -export([count/2]).
% -import(second, [count/2]).

freq1(L) -> lists:usort([{E, second:count(E, L)} || E <- L]).

freq2([]) -> [];
freq2([H|T]) -> [{H, second:count(H, T) + 1} | freq2(lists:filter(fun (X) -> X /= H end, T))].

freq3(L) -> freq3(L, []).
freq3([], R) -> R;
freq3([H|T], R) -> 
    case fstelem(H, R) of
        true -> freq3(T, R);
        false -> freq3(T, [{H, second:count(H, T)+1} | R])
    end.

fstelem(E, [{E, _}|_]) -> true;
fstelem(E, [_|T]) -> fstelem(E, T);
fstelem(_, []) -> false.

freq4(L) -> freq4(L, #{}).
freq4([], M) -> M;
freq4([H|T], M) -> 
    % case M of
    %     #{H := _} -> freq4(T, M);
    %     _         -> freq4(T, M#{H => second:count(H, T) + 1})
    % end.
    case maps:is_key(H, M) of
        true -> freq4(T, M);
        _    -> freq4(T, M#{H => second:count(H, T) + 1})
    end.

freq5(L) -> freq5(L, #{}).
freq5([], M) -> M;
freq5([H|T], M) -> 
    case M of
        #{H := C} -> freq5(T, M#{H => C+1});
        _         -> freq5(T, M#{H => 1})
    end.


freq([]) -> #{};
freq([H|T]) -> freq(lists:sort(T), {H, 1}, #{}).

% freq([], P, R) -> [P|R];
% freq([H|T], {H, C}, R) -> freq(T, {H, C+1}, R);
% freq([H|T], P, R) -> freq(T, {H, 1}, [P|R]).

freq([], {E, C}, R) -> R#{E => C};
freq([H|T], {H, C}, R) -> freq(T, {H, C+1}, R);
freq([H|T], {E, C}, R) -> freq(T, {H, 1}, R#{E => C}).