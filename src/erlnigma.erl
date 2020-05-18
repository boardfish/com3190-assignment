-module(erlnigma).

-include("enigma.hrl").

-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    Inc = spawn(erlnigma, receiver, [inc]),
    io:format("Spawning keyboard.~n"),
    Keyboard = spawn(erlnigma, keyboard, [self(), Inc]),
    io:format("Keyboard PID: ~p~n", [Keyboard]),
    io:format("Broadcasting with args: ~p, ~p, ~p~n", [Keyboard, key, x]),
    broadcast(Keyboard, key, x),
    broadcast(Keyboard, lamp, true),
    broadcast(Keyboard, key, y),
    receive
      {close} -> erlang:halt(0)
    end.

%%====================================================================
%% Helpers functions
%%====================================================================

broadcast(Target, Channel, Value) ->
    io:format("-> ~p - ~p~n", [Channel, Value]),
    Target ! {Channel, Value}.

receiver(Channel) ->
    receive
      {Channel, Value} -> io:format("<- [~p] ~p~n", [Channel, Value])
    end,
    ok.

receiver(Channel, Callback) ->
    receive
      {Channel, Value} ->
        io:format("<- [~p] ~p~n", [Channel, Value]),
        Callback()
    end,
    ok.

keyboard(Parent, Inc) ->
  io:format("New keyboard.~n"),
  io:format("Receiving key.~n"),
  receiver(key),
  io:format("Broadcasting inc.~n"),
  broadcast(Inc, inc, 1),
  io:format("Awaiting lamp.~n"),
  receiver(lamp, fun() -> keyboard(Parent, Inc) end).

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
