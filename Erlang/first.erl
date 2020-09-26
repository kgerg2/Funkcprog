-module(first).
-export([zero/0, id/1, head/1]).

zero() -> 0.

id(X) -> X.

head([X|_]) -> X.


% nincs hatványozás
% bitműveletek
% fun first:zero/0 fv típusú adat
% fun (X) -> X+1 end.