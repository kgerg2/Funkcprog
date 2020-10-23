-module(xor_cipher).
-export([encrypt/2, decrypt/2, getKey/2, decodeMessage/2]).

toBitString(0) -> [];
toBitString(A) -> [A rem 2 | toBitString(A div 2)].

charToBitString(A) ->
    Bits = toBitString(A),
    Pad = [0 || _ <- lists:seq(1, 8 - length(Bits))],
    lists:append(Bits, Pad).

bitStringToChar(A) -> lists:foldr(fun(X, Y) -> X + 2*Y end, 0, A).

xOr([], A) -> A;
xOr(A, []) -> A;
xOr([A|AS], [B|BS]) -> [A bxor B | xOr(AS, BS)].

cycle(L, N) -> lists:flatten(lists:duplicate(N div length(L), L), lists:sublist(L, N rem length(L))).

encrypt(Text, Key) ->
    Keys = cycle(Key, length(Text)),
    lists:zipwith(fun(X, Y) -> bitStringToChar(xOr(charToBitString(X), charToBitString(Y))) end, Text, Keys).

decrypt(Text, Key) -> encrypt(Text, Key).

isCycledIn(A, B) -> lists:prefix(A, B) and (cycle(A, length(B)) == B).

getKey(Text, Cipher) -> 
    Keys = encrypt(Text, Cipher),
    Prefixes = [lists:sublist(Keys, N) || N <- lists:seq(1, length(Keys))],
    {value, Key} = lists:search(fun(X) -> isCycledIn(X, Keys) end, Prefixes),
    Key.

decodeMessage(Cipher, PartOfText) ->
    KeyFirst = getKey(PartOfText, Cipher),
    {N, KeyShifted} = lists:foldr(fun({N, X}, {M, Y}) -> if length(X) < length(Y) -> {N, X}; true -> {M, Y} end end, {0, KeyFirst}, [{N-1, getKey(PartOfText, lists:sublist(Cipher, N, length(PartOfText)))} || N <- lists:seq(2, length(Cipher) - length(PartOfText) + 1)]),
    Key = lists:concat([lists:sublist(KeyShifted, length(KeyShifted) - (N rem length(KeyShifted)) + 1, N rem length(KeyShifted)), lists:sublist(KeyShifted, length(KeyShifted) - (N rem length(KeyShifted)))]),
    decrypt(Cipher, Key).