-module(erlnigma).

-include("enigma.hrl").

-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    L = spawn(erlnigma, receiver, [l]),
    R = spawn(erlnigma, receiver, [r]),
    io:format("Spawning plugboard.~n"),
    Plugboard = spawn(erlnigma, plugboard, [self(), [{$A, $E}, {$E, $A}], R, L]),
    io:format("Plugboard PID: ~p~n", [Plugboard]),
    io:format("Broadcasting with args: ~p, ~p, ~p~n", [Plugboard, l, $A]),
    broadcast(Plugboard, l, $A),
    io:format("Broadcasting with args: ~p, ~p, ~p~n", [Plugboard, r, $E]),
    broadcast(Plugboard, r, $E),
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
      {Channel, Value} ->
        io:format("<- [~p] ~p~n", [Channel, Value]),
        Value
    end.

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

reflector(Parent, Out) ->
  io:format("New reflector.~n"),
  io:format("Receiving in.~n"),
  Key = receiver(in),
  io:format("Broadcasting out key for ~p.~n", [Key]),
   {F_refl_result,_} = lists:keyfind(Key, 2, reflectorA()), % Unsure about this.
  broadcast(Out, out, F_refl_result),
  io:format("Looping back.~n"),
  reflector(Parent, Out).

plugboard(Parent, Plugboard, Right, Left) ->
  io:format("New Plugboard initialised.~n"),
  receive
    {l, Value} ->
      {F_plug_result,_} = lists:keyfind(Value, 2, Plugboard), % Unsure about this.
      io:format("Received ~p on L, broadcasting ~p on R.~n", [Value, F_plug_result]),
      broadcast(Right, l, F_plug_result),
      plugboard(Parent, Plugboard, Right, Left);
    {r, Value} ->
      {F_plug_result,_} = lists:keyfind(Value, 2, Plugboard), % Unsure about this.
      io:format("Received ~p on R, broadcasting ~p on L.~n", [Value, F_plug_result]),
      broadcast(Left, r, F_plug_result),
      plugboard(Parent, Plugboard, Right, Left)
  end.


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
