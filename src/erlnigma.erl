-module(erlnigma).

%% API exports
-export([main/1, ping/2, pong/0, start/0]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    start(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

ping(0, Pong_PID) ->
    Pong_PID ! finished, io:format("Pingfinished~n", []);
ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive pong -> io:format("Pingreceivedpong~n", []) end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
      finished -> io:format("Pongfinished~n", []);
      {ping, Ping_PID} ->
	  io:format("Pongreceivedping~n", []),
	  Ping_PID ! pong,
	  pong()
    end.

start() ->
    Pong_PID = spawn(pingpong, pong, []),
    spawn(pingpong, ping, [3, Pong_PID]).