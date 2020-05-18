-module(erlnigma).

-include("enigma.hrl").

-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    io:format("Spawning reflector.~n"),
    Reflector = spawn(erlnigma, receiver, [self()]),
    io:format("Reflector PID: ~p~n", [Reflector]),
    io:format("Broadcasting with args: ~p, ~p, ~p~n", [Reflector, rcv, bar]),
    broadcast(Reflector, rcv, bar),
    receive
      {close} -> erlang:halt(0)
    end.

%%====================================================================
%% Helpers functions
%%====================================================================

broadcast(Target, Channel, Value) ->
    io:format("-> ~p - ~p~n", [Channel, Value]),
    Target ! {rcv, Value}.

receiver(Parent) ->
    io:format("foo~n"),
    receive
      {rcv, Value} -> io:format("<- ~p~n", [Value]);
      AnythingElse -> io:format("<- ~p~n", [AnythingElse])
    end,
    io:format("foo~n"),
    Parent ! {close},
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

% Schema for each part is as follows:
% - Each process knows which one created it
% - The rest of its arguments are processes that can talk to it, or it talks to

% message schema - {Channel_PID,sender, contents}

%% Returns the index of a character in one of the rotor arrays, searching by
%% first item.
forward_index_of(Char, List) ->
    string:str(lists:map(fun ({Key, _}) -> Key end, List),
	       [Char]).

%% Returns the index of a character in one of the rotor arrays, searching by
%% last item.
backward_index_of(Char, List) ->
    string:str(lists:map(fun ({_, Key}) -> Key end, List),
	       [Char]).

% http://erlang.org/eeps/eep-0043.html

setup(ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting) ->
    ok.

crypt(Enigma_PID, TextString) -> ok.

kill(Enigma_PID) -> ok.
