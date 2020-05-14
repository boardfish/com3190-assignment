-module(erlnigma_test).
-import(erlnigma, [start/0, ping/2, pong/0]).

-include_lib("eunit/include/eunit.hrl").

expect(Val) ->
    receive
      Val -> ok;
      Other -> {error, Other}
    end.

ping1_test() ->
    PID = spawn(erlnigma, ping, [1, self()]),
    ok = expect({ping, PID}),
    PID ! pong,
    ok = expect(finished).