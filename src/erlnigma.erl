-module(erlnigma).

-include("enigma.hrl").

-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

message_broker(RegReceivers, UnsentMsgs) ->
    io:format("Message Broker: ~p ~p ~p~n",
	      [self(), RegReceivers, UnsentMsgs]),
    receive
      {broadcasts, Source, Channel, Value} ->
    io:format("~p -> ~p: ~p~n", [Source, Channel, Value]),
	  case lists:keyfind(Channel, 1, RegReceivers) of
	    false ->
		message_broker(RegReceivers,
			       [{Channel, Value, Source} | UnsentMsgs]);
	    {Channel, Target} ->
		Target ! {receives, Channel, Value},
		Source ! {ok, {broadcasts, Channel, Value}},
		message_broker(lists:keydelete(Channel, 1,
					       RegReceivers),
			       UnsentMsgs)
	  end;
      {receives, Channel, Target} ->
    io:format("~p <- ~p~n", [Target, Channel]),
	  case lists:keyfind(Channel, 1, UnsentMsgs) of
	    false ->
		message_broker([{Channel, Target} | RegReceivers],
			       UnsentMsgs);
	    {Channel, Value, Source} ->
		Target ! {receives, Channel, Value},
		Source ! {ok, {broadcasts, Channel, Value}},
		message_broker(RegReceivers,
			       lists:keydelete(Channel, 1, UnsentMsgs))
	  end
    end.

%% escript Entry point
main(Args) ->
    Parent = spawn(erlnigma, message_broker, [[], []]),
    Inc_L = spawn(erlnigma, receives, [Parent, incl]),
    Rotor = spawn(erlnigma, keyboard, [Parent, incl]),
    io:format("RotorPID: ~p~n", [Rotor]),
    broadcasts(Parent, self(), key, $A),
    broadcasts(Parent, self(), lamp, $A),
    % broadcasts(Parent, self(), l, $A),
    % broadcasts(Parent, self(), r, $A),
    receive {close} -> erlang:halt(0) end.

%%====================================================================
%% Helper functions
%%====================================================================

broadcasts(Target, Source, Channel, Value) ->
    io:format("-> [~5w ~p] ~p~n", [Channel, Source, Value]),
    Target ! {broadcasts, Source, Channel, Value},
    receive {ok, {broadcasts, Channel, Value}} -> ok end.

receives(Parent, Channel) ->
    Parent ! {receives, Channel, self()},
    receive
      {receives, Channel, Value} ->
	  io:format("<- [~5w] ~p~n", [Channel, Value]), Value
    end.

receives(Parent, Channel, Callback) ->
    Parent ! {receives, Channel, self()},
    receive
      {receives, Channel, Value} ->
	  io:format("<- [~5w] ~p~n", [Channel, Value]),
	  Callback()
    end.

keyboard(Parent, Inc) ->
    io:format("New keyboard.~n"),
    io:format("Receiving key.~n"),
    receives(Parent, key),
    io:format("Broadcasting inc.~n"),
    broadcasts(Parent, self(), Inc, 1),
    io:format("Awaiting lamp.~n"),
    receives(Parent, lamp, fun () -> keyboard(Parent, Inc) end).

reflector(Parent, Out) ->
    io:format("New reflector.~n"),
    io:format("Receiving in.~n"),
    Key = receives(Parent, in),
    io:format("Broadcasting out key for ~p.~n", [Key]),
    {F_refl_result, _} = lists:keyfind(Key, 2,
				       reflectorA()), % Unsure about this.
    broadcasts(Parent, self(), out, F_refl_result),
    io:format("Looping back.~n"),
    reflector(Parent, Out).

plugboard(Parent, Plugboard, Right, Left) ->
    io:format("New Plugboard initialised.~n"),
    receive
      {l, Value} ->
	  {F_plug_result, _} = lists:keyfind(Value, 2,
					     Plugboard), % Unsure about this.
	  io:format("Received ~p on L, broadcasting ~p on "
		    "R.~n",
		    [Value, F_plug_result]),
	  broadcasts(Parent, self(), r, F_plug_result),
	  plugboard(Parent, Plugboard, Right, Left);
      {r, Value} ->
	  {F_plug_result, _} = lists:keyfind(Value, 2,
					     Plugboard), % Unsure about this.
	  io:format("Received ~p on R, broadcasting ~p on "
		    "L.~n",
		    [Value, F_plug_result]),
	  broadcasts(Parent, self(), l, F_plug_result),
	  plugboard(Parent, Plugboard, Right, Left)
    end.

% Todo: f_rotor-result is wrong, should take c and p
rotorFunction(Parent, Right, Left) ->
    io:format("Calling rotorFunction."),
    receive
      {l, Value} ->
	  {F_rotor_result, _} = lists:keyfind(Value, 2,
					      rotorI()), % Unsure about this.
	  io:format("Received ~p on L, broadcasting ~p on "
		    "R.~n",
		    [Value, F_rotor_result]),
	  broadcasts(Parent, self(), l, F_rotor_result);
      {r, Value} ->
	  {F_rotor_result, _} = lists:keyfind(Value, 2,
					      rotorI()), % Unsure about this.
	  io:format("Received ~p on R, broadcasting ~p on "
		    "L.~n",
		    [Value, F_rotor_result]),
	  broadcasts(Parent, self(), r, F_rotor_result)
    end.

rotor(Parent, Inc_l, C, P) ->
    io:format("Hello, this is Rotor ~p ~p ~n", [C, P]),
    receive
      {inc, _} ->
	  case C of
	    26 ->
		% send inc to incl
		rotor(Parent, Inc_l, 0, P - 26);
	    _ ->
		% Rotor(Parent, Inc_L, 0, P - 26
		rotor(Parent, Inc_l, C + 1, P + 1)
	  end;
      {l, Value} ->
	  io:format("Received ~p on L, broadcasting ~p on "
		    "R.~n",
		    [Value, "something"]),
	  % rotorFunction(Parent, );
	  rotor(Parent, Inc_l, C, P);
      {r, Value} ->
	  io:format("Received ~p on R, broadcasting ~p on "
		    "L.~n",
		    [Value, "something"]),
	  % rotorFunction(Parent, )
	  rotor(Parent, Inc_l, C, P)
    end.

    % inc - new rotor
    % l/r - forward to rotorfunction? I guess that's the equivalent of expanding the process.

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
    % Keyboard sends on Key and Inc
    % Plugboard sends on L and R
    % Rotors send on L, R and Inc
    % Reflector sends on Out
    ok.

crypt(Enigma_PID, TextString) -> ok.

kill(Enigma_PID) -> ok.
