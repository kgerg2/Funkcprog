-module(pingpong).
-export([run/0, proc1/0]).

run() -> spawn(pingpong, proc1, []).

proc1() -> io:format("Started proc1.").

proc2() -> io:format("Started proc2.").