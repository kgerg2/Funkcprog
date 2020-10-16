-module(fifth).
-export([eval/0]).

eval() ->
    try
        {ok, Mod} = io:read("Module name: "),
        FunList = Mod:module_info(exports),
        {ok, Fun} = io:read("Function name: "),
        {_, Count} = lists:keyfind(Fun, 1, FunList),
        % Args = [Par || _ <- lists:seq(1, Count), {ok, Par} <- io:read("Parameter: ")],
        Args = [begin
                    {ok, Par} = io:read("Parameter: "),
                    Par
                end || _ <- lists:seq(1, Count)],
        apply(Mod, Fun, Args)
        % Args
    of
        Res -> Res
    catch
        % ErrorClass:ErrorType -> {ErrorClass, ErrorType}
        error:badarg            -> "Module name not an atom.";
        error:undef             -> "Module does not exist.";
        error:{badmatch, false} -> "Function does not exists.";
        error:{badmatch, _}     -> "Not a valid input argument.";
        _    :_                 -> "Error when evaluating function"
    after
        io:format("After\n")
    end.