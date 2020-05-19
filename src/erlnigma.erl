-module(erlnigma).

-include("enigma.hrl").

-compile(export_all).

%% escript entry point
%% Gets called by scripts/run
main(Args) ->
    Parent = spawn(erlnigma, message_broker, [[], []]),
    Rotor = spawn(erlnigma, rotor,
		  [Parent, incl, incr, r, l, 0, 0]),
    IncR = spawn(erlnigma, receives, [Parent, incr]),
    L = spawn(erlnigma, receives, [Parent, l]),
    io:format("RF: ~p~n", [Rotor]),
    broadcasts(Parent, self(), incl, 1),
    broadcasts(Parent, self(), r, $A),
    broadcasts(Parent, self(), l, $A),
    % io:format("Broadcasting with args: ~p, ~p, ~p~n", [Parent, l, $E]),
    receive {close} -> erlang:halt(0) end.

%%====================================================================
%% Enigma parts
%%====================================================================

keyboard(Parent, Inc) ->
    io:format("New keyboard.~n"),
    io:format("Receiving key.~n"),
    receives(Parent, key),
    io:format("Broadcasting inc.~n"),
    broadcasts(Parent, self(), Inc, 1),
    io:format("Awaiting lamp.~n"),
    receives(Parent, lamp,
	     fun () -> keyboard(Parent, Inc) end).

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
    io:format("PB: Received ~p on input, broadcasting "
	      "~p on output.~n",
	      [Key, F_plug_result]),
    broadcasts(Parent, self(), Output, F_plug_result),
    % Respond on the opposite channel this time.
    io:format("PB: New plugboard hours~n"),
    plugboard(Parent, Plugboard, Output, Input).

% Todo: calculate f_rotor-result
% Todo: refactor, there's duplication here
rotorFunction(Parent, Right, Left) ->
    io:format("Calling rotorFunction.~n"),
    Key = receives(Parent, Right),
    {F_rotor_result, _} = lists:keyfind(Key, 2,
					rotorI()), % Unsure about this.
    io:format("RF: Received ~p on right, broadcasting "
	      "~p on left.~n",
	      [Key, F_rotor_result]),
    broadcasts(Parent, self(), Left, F_rotor_result),
    % Respond on the opposite channel this time.
    io:format("RF: Switcheroo time."),
    Key = receives(Parent, Left),
    {F_rotor_result, _} = lists:keyfind(Key, 2,
					rotorI()), % Unsure about this.
    io:format("RF: Received ~p on left, broadcasting "
	      "~p on right.~n",
	      [Key, F_rotor_result]),
    broadcasts(Parent, self(), Right, F_rotor_result).

rotor(Parent, Inc_L, Inc_R, Right, Left, C, P) ->
    io:format("Hello, this is Rotor ~p ~p ~n", [C, P]),
    IncL = receives(Parent, Inc_L),
    case C of
      26 ->
	  % send inc to incl
	  broadcasts(Parent, self(), Inc_R,
		     case IncL of
		       1 -> 1;
		       _ -> 0
		     end),
	  rotorFunction(Parent, Right, Left),
	  rotor(Parent, Inc_L, Inc_R, Right, Left, 0, P - 26);
      _ ->
	  broadcasts(Parent, self(), Inc_R, 0),
	  rotorFunction(Parent, Right, Left),
	  rotor(Parent, Inc_L, Inc_R, Right, Left, C + 1, P + 1)
    end.

%%====================================================================
%% Helper functions
%%====================================================================

% Shared message broker. Each of the Enigma parts communicates on channels whose
% names are passed into each one as arguments. The message broker, along with
% broadcasts and receives, make sure that each process blocks until a fitting
% message comes in on the right channel. This makes it a lot easier for me to
% move from the pi-calculus representation to Erlang code and also means that
% processes don't depend on PIDs (that might not yet exist) on startup.
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

% Target is usually the parent process (the Enigma itself, which runs the
% message broker). Source gives the PID of the process that called for the
% broadcast, for debug reasons. Channel is an atom, and Value is the message
% content.
broadcasts(Target, Source, Channel, Value) ->
    io:format("BC: ~p, ~p, ~p~n", [Target, Channel, Value]),
    Target ! {broadcasts, Source, Channel, Value},
    receive {ok, {broadcasts, Channel, Value}} -> ok end.

% receives awaits a message on the channel, then returns its value.
receives(Parent, Channel) ->
    Parent ! {receives, Channel, self()},
    receive
      {receives, Channel, Value} ->
	  io:format("<- [~5w ~p] ~p~n", [Channel, Parent, Value]),
	  Value
    end.

% This version awaits a message on the channel, then runs a callback function
% passed to it.
receives(Parent, Channel, Callback) ->
    Parent ! {receives, Channel, self()},
    receive
      {receives, Channel, Value} ->
	  io:format("<- [~5w] ~p~n", [Channel, Value]), Callback()
    end.

%%====================================================================
%% Exported functions
%%====================================================================

setup(ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting) ->
    % Keyboard sends on Key and Inc
    % Plugboard sends on L and R
    % Rotors send on L, R and Inc
    % Reflector sends on Out
    ok.

crypt(Enigma_PID, TextString) -> ok.

kill(Enigma_PID) -> ok.
