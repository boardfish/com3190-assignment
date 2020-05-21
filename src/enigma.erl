-module(enigma).

-include("enigma.hrl").

-compile(export_all).

%% escript entry point
%% Gets called by scripts/run
main(Args) ->
    Parent = setup("B", {"II", "I", "III"}, {12, 6, 18}, [{$A, $E}, {$F, $J}, {$P, $R}], {$D, $F, $R}),
    broadcasts(Parent, self(), x, $A),
    % Parent = spawn(enigma, message_broker, [[], []]),
    % Rotor = spawn(enigma, rotor,
    %   [Parent, incl, incr, r, l, 0, 0]),
    % IncR = spawn(enigma, receives, [Parent, incr]),
    % L = spawn(enigma, receives, [Parent, l]),
    % io:format("RF: ~p~n", [Rotor]),
    % broadcasts(Parent, self(), incl, 1),
    % broadcasts(Parent, self(), r, 69),
    % io:format("Broadcasting with args: ~p, ~p, ~p~n", [Parent, l, $E]),
    receive {close} -> erlang:halt(0) end.

normaliseAsciiNum(Num) -> Num rem 25 + 50.

%%====================================================================
%% Enigma parts
%%====================================================================

keyboard(Parent, Keys, Lamp, Inc) ->
    io:format("KB: ~p~n", [self()]),
    X = receives(Parent, x),
    broadcasts(Parent, self(), Inc, 1),
    broadcasts(Parent, self(), Keys, X),
    receives(Parent, Lamp,
      fun () -> keyboard(Parent, Keys, Lamp, Inc) end).

reflector(Parent, In, Out, Reflector) ->
    io:format("RE: ~p~n", [self()]),
    Key = receives(Parent, In),
    io:format("RE: Broadcasting out key for ~p.~n", [Key]),
    F_refl_result = f_refl(Reflector, Key),
    broadcasts(Parent, self(), Out, F_refl_result),
    io:format("RE: Looping back.~n"),
    reflector(Parent, In, Out, Reflector).

f_refl(Reflector, Input) ->
    case string:str(lists:map(fun ({Key, _}) -> Key end,
			      Reflector),
		    [Input])
	of
      0 -> f_refl(Reflector, Input, true);
      Otherwise ->
	  lists:nth(Otherwise, [Y || {_, Y} <- Reflector])
    end.

% If a match wasn't found on the first pass, throw in an arbitrary third
% argument to search by the second value of the tuple.
f_refl(Reflector, Input, _) ->
    io:format("Reflector got ~p!~n", [Input]),
    io:format("Reflector is ~p!~n", [Reflector]),
    case string:str(lists:map(fun ({_, Key}) -> Key end,
			      Reflector),
		    [Input])
	of
      0 -> {error, "Match not found."};
      Otherwise ->
	  lists:nth(Otherwise, [X || {X, _} <- Reflector])
    end.

plugboard(Parent, Plugboard, Input, Output) ->
    io:format("PB: ~p~n", [self()]),
    Key = receives(Parent, Input),
    F_plug_result = f_plug(Plugboard, Key),
    io:format("PB: Received ~p on input, broadcasting "
	      "~p on output.~n",
	      [Key, F_plug_result]),
    broadcasts(Parent, self(), Output, F_plug_result),
    % Respond on the opposite channel this time.
    io:format("PB: New plugboard hours~n"),
    plugboard(Parent, Plugboard, Output, Input).

f_plug(Plugboard, Input) -> f_refl(Plugboard, Input).

% Todo: calculate f_rotor-result
% Todo: refactor, there's duplication here
rotorFunction(Parent, Right, Left, Rotor, P) ->
    rotorPass(Parent, Right, f_rotor, Left, Rotor, P),
    rotorPass(Parent, Left, inverse_f_rotor, Right, Rotor,
	      P).

% params:

rotorPass(Parent, Input, EncryptionFunction, Output,
	  Rotor, P) ->
    Key = receives(Parent, Input),
    F_rotor_result = enigma:EncryptionFunction(Rotor, P,
						 Key),
    broadcasts(Parent, self(), Output, F_rotor_result).

rotor(Parent, Inc_L, Inc_R, Right, Left, C, P) ->
    io:format("RO: ~p ~p ~p ~n", [self(), C, P]),
    io:format("RO: ~p receives on ~p ~n", [self(), Inc_R]),
    IncR = receives(Parent, Inc_R),
    case C of
      26 ->
	  % send inc to incl
	  broadcasts(Parent, self(), Inc_L,
		     case IncR of
		       1 -> 1;
		       _ -> 0
		     end),
    io:format("RO: rotorFunction is go for ~p~n", [self()]),
	  rotorFunction(Parent, Right, Left, rotorI(), P),
	  rotor(Parent, Inc_L, Inc_R, Right, Left, 0, P - 26);
      _ ->
	  broadcasts(Parent, self(), Inc_L, 0),
	  rotorFunction(Parent, Right, Left, rotorI(), P),
	  rotor(Parent, Inc_L, Inc_R, Right, Left, C + 1, P + 1)
    end.

f_rotor(Rotor, P, X) ->
    NewCharacter = wrapToRange(X + P, $A, $Z),
    io:format("Calling frotor with P = ~p and X = ~p = ~p~n",
	      [P, X, NewCharacter]),
    io:format("PUTSBASEDDEBUG: character lookup for ~p: ~p~n", [NewCharacter, lists:keyfind(NewCharacter, 1, Rotor)]),
    element(2, lists:keyfind(NewCharacter, 1, Rotor)).

inverse_f_rotor(Rotor, P, X) ->
    NewCharacter = ((X - (P - $Z)) rem 26),
    io:format("Calling inverse frotor with P = ~p and "
	      "X = ~p = ~p~n",
	      [P, X, ($A + NewCharacter)]),
    io:format("~p",
	      [Rotor]),
    ($A + (element(1, lists:keyfind($A + NewCharacter, 2, Rotor)) rem 26)).

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

% Target should be a message broker. Source gives the PID of the process that
% called for the broadcast, for debug reasons. Channel is an atom, and Value is
% the message content.
broadcasts(Target, Source, Channel, Value) ->
    io:format("BC: ~p, ~p, ~p~n", [Target, Channel, Value]),
    Target ! {broadcasts, Source, Channel, Value},
    if Channel == none ->
      io:format("RO: Final rotor broadcasts.~n", []),
      ok;
    true ->
      receive {ok, {broadcasts, Channel, Value}} -> ok end
    end.

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

% Takes an atom and a string, and returns the return value of the corresponding
% function in enigma.hrl.
listFor(Type, Name) ->
    FunctionName =
	list_to_atom(lists:flatten([atom_to_list(Type), Name])),
    enigma:FunctionName().

% Wraps to a range, inclusive of max.
wrapToRange(Input, Min, Max) ->
  if Input < Min ->
    $A + (((Input - $A) rem 26) + 26) rem 26;
  true ->
    Min + ((Input - Min) rem ((Max + 1) - Min))
  end.

% generateRotors(Parent, RotorNames, RingSettings, InitialSetting) ->
%   InputChannels = {m1, m2, m3},
%   OutputChannels = {ref, m1, m2},
%   rotor(Parent, incl, incr, right, left, c, p) element(1, RotorNames)
%   % for each rotorName:
%   Rotor = listFor(rotor, RotorName)
%   {
%     rotor(Parent, )
%   }

%%====================================================================
%% Exported functions
%%====================================================================

enigma(ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting) ->
    Reflector = spawn(enigma, reflector,
		      [self(), ref, ref, listFor(reflector, ReflectorName)]),
% rotor(Parent, Inc_L, Inc_R, Right, Left, C, P) ->
    Rotor3 = spawn(enigma, rotor,
		   [self(), none, i3, m1, ref, element(1, RingSettings),
		    element(1, InitialSetting)]),
    Rotor2 = spawn(enigma, rotor,
		   [self(), i3, i2, m2, m1, element(2, RingSettings),
		    element(2, InitialSetting)]),
    Rotor1 = spawn(enigma, rotor,
		   [self(), i2, i1, m3, m2, element(3, RingSettings),
		    element(3, InitialSetting)]),
    Plugboard = spawn(enigma, plugboard,
		      [self(), PlugboardPairs, keys, m3]),
    Keyboard = spawn(enigma, keyboard,
		     [self(), keys, lamp, i1]),
    message_broker([], []).

setup(ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting) ->
    spawn(enigma, enigma, [ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting]).

crypt(Enigma_PID, TextString) -> ok.

kill(Enigma_PID) -> ok.
