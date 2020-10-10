-module(fifth).
-export([eval/0]).

eval() ->
    {ok, Mod} = io:read("Module name:"),
    {ok, Fun} = io:read("Function name:"),
    {ok, Par} = io:read("Parameter"),
    apply(Mod, Fun, Par).