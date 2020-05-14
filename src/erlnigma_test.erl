-module(erlnigma_test).
-import(erlnigma, []).

-include_lib("eunit/include/eunit.hrl").

expect(Val) ->
    receive
      Val -> ok;
      Other -> {error, Other}
    end.

% ping1_test() ->
%     PID = spawn(erlnigma, ping, [1, self()]),
%     ok = expect({ping, PID}),
%     PID ! pong,
%     ok = expect(finished).

% Tests required:
% - Some tests to check that Reflector returns the result of frefl(x)
% - Calling Keyboard(x) with a linked Plugboard sends x to the plugboard and 