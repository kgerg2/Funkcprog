-module(xor_cipher).
-export([encrypt/2, decrypt/2, getKey/2, decodeMessage/2]).

toBitString(0) -> [];
toBitString(A) -> [A rem 2 | toBitString(A div 2)].

charToBitString(A) -> toBitString(A).
    % The following lines produce and 8 long list, but it is not needed for this task.
    % Bits = toBitString(A),
    % Pad = [0 || _ <- lists:seq(1, 8 - length(Bits))],
    % lists:append(Bits, Pad).

bitStringToChar(A) -> lists:foldr(fun(X, Y) -> X + 2*Y end, 0, A).

xOr([], A) -> A;
xOr(A, []) -> A;
xOr([A|As], [B|Bs]) -> [A bxor B | xOr(As, Bs)].

% cycle(L, N) -> lists:flatten(lists:duplicate(N div length(L), L), lists:sublist(L, N rem length(L))).
cycle(L, N) -> cycle([], N, L).

cycle(_, 0, _) -> [];
cycle([], N, [H|T]) -> [H | cycle(T, N-1, [H|T])];
cycle([H|T], N, L) -> [H | cycle(T, N-1, L)].

encrypt(Text, Key) ->
    Keys = cycle(Key, length(Text)),
    lists:zipwith(fun(X, Y) -> bitStringToChar(xOr(charToBitString(X), charToBitString(Y))) end, Text, Keys).

decrypt(Text, Key) -> encrypt(Text, Key).

% isCycledIn(A, B) -> lists:prefix(A, B) andalso (cycle(A, length(B)) == B).
isCycledIn(A, B) -> lists:prefix(A, B) andalso isCycledIn([], B, A).

isCycledIn(_, [], _) -> true;
isCycledIn([], [B|Bs], [A|As]) -> A == B andalso isCycledIn(As, Bs, [A|As]);
isCycledIn([A|As], [B|Bs], L) -> A == B andalso isCycledIn(As, Bs, L).

getKey(Text, Cipher) -> 
    Keys = encrypt(Text, Cipher),
    Prefixes = [lists:sublist(Keys, N) || N <- lists:seq(1, length(Keys))],
    {value, Key} = lists:search(fun(X) -> isCycledIn(X, Keys) end, Prefixes), % cannot fail, last element is the whole list
    Key.

decodeMessage(Cipher, PartOfText) ->
    KeyFirst = getKey(PartOfText, Cipher),
    Keys = [{N-1, getKey(PartOfText, lists:sublist(Cipher, N, length(PartOfText)))} || N <- lists:seq(2, length(Cipher) - length(PartOfText) + 1)],
    {N, KeyShifted} = lists:foldr(fun({N, X}, {M, Y}) -> if length(X) < length(Y) -> {N, X}; true -> {M, Y} end end, {0, KeyFirst}, Keys),
    Shift = length(KeyShifted) - (N rem length(KeyShifted)),
    Key = lists:concat([lists:sublist(KeyShifted, Shift + 1, length(KeyShifted)), lists:sublist(KeyShifted, Shift)]),
    decrypt(Cipher, Key).