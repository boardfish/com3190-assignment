-module(erlnigma_test).
-import(erlnigma, [index_of/2, reflectorA/0]).

-include_lib("eunit/include/eunit.hrl").

expect(Val) ->
    receive
      Val -> ok;
      Other -> {error, Other}
    end.

index_of_test() ->
    ?assertEqual(erlnigma:index_of($A, reflectorA()), 1),
    ?assertEqual(erlnigma:index_of($B, reflectorA()), 2),
    ?assertEqual(erlnigma:index_of($C, reflectorA()), 3).

% Tests required:
% - Some tests to check that Reflector returns the result of frefl(x)
% - Calling Keyboard(x) with a linked Plugboard sends x to the plugboard and 