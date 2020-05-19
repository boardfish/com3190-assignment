-module(erlnigma).

-include("enigma.hrl").

-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

message_broker(RegReceivers, UnsentMsgs) ->
    io:format("MB: ~p ~p ~p~n",
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
    L = spawn(erlnigma, receives, [Parent, l]),
    R = spawn(erlnigma, receives, [Parent, r]),
    RotorFunction = spawn(erlnigma, rotorFunction, [Parent, r, l]),
    io:format("RF: ~p~n", [RotorFunction]),
    broadcasts(Parent, self(), r, $A),
    broadcasts(Parent, self(), l, $Z),
    % io:format("Broadcasting with args: ~p, ~p, ~p~n", [Parent, l, $E]),
    receive
      {close} -> erlang:halt(0)
    end.


%%====================================================================
%% Helper functions
%%====================================================================

broadcasts(Target, Source, Channel, Value) ->
    io:format("BC: ~p, ~p, ~p~n", [Target, Channel, Value]),
    Target ! {broadcasts, Source, Channel, Value},
    receive {ok, {broadcasts, Channel, Value}} -> ok end.

receives(Parent, Channel) ->
    Parent ! {receives, Channel, self()},
    receive
      {receives, Channel, Value} ->
	  io:format("<- [~5w ~p] ~p~n", [Channel, Parent, Value]), Value
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

reflector(Parent, In, Out) ->
    io:format("RE: New reflector.~n"),
    io:format("RE: Receiving in.~n"),
    Key = receives(Parent, In),
    io:format("RE: Broadcasting out key for ~p.~n", [Key]),
    {_, F_refl_result} = lists:keyfind(Key, 1,
				       reflectorA()), % Unsure about this.
    broadcasts(Parent, self(), Out, F_refl_result),
    io:format("RE: Looping back.~n"),
    reflector(Parent, In, Out).

plugboard(Parent, Plugboard, Input, Output) ->
    io:format("PB: New Plugboard initialised.~n"),
    Key = receives(Parent, Input),
	  {F_plug_result, _} = lists:keyfind(Key, 2,
					     Plugboard), % Unsure about this.
	  io:format("PB: Received ~p on input, broadcasting ~p on "
		    "output.~n",
		    [Key, F_plug_result]),
	  broadcasts(Parent, self(), Output, F_plug_result),
    % Respond on the opposite channel this time.
	  io:format("PB: New plugboard hours~n"),
	  plugboard(Parent, Plugboard, Output, Input).

% Todo: calculate f_rotor-result
% Todo: refactor, there's duplication here
rotorFunction(Parent, Right, Left) ->
    io:format("Calling rotorFunction."),
    Key = receives(Parent, Right),
	  {F_rotor_result, _} = lists:keyfind(Key, 2,
					      rotorI()), % Unsure about this.
	  io:format("RF: Received ~p on right, broadcasting ~p on "
		    "left.~n",
		    [Key, F_rotor_result]),
	  broadcasts(Parent, self(), Left, F_rotor_result),
    % Respond on the opposite channel this time.
	  io:format("RF: Switcheroo time."),
    Key = receives(Parent, Left),
	  {F_rotor_result, _} = lists:keyfind(Key, 2,
					      rotorI()), % Unsure about this.
	  io:format("RF: Received ~p on left, broadcasting ~p on "
		    "right.~n",
		    [Key, F_rotor_result]),
	  broadcasts(Parent, self(), Right, F_rotor_result).

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
