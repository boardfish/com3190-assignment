-module(enigma).

-include("enigma.hrl").

-compile(export_all).

%% escript entry point
%% Call this by running the following in a shell:
%% rebar3 escriptize && ./_build/default/bin/enigma "Text you want to encrypt here"
%% ---
%% Development note: I used this often, passing Res to a second machine's crypt
main(Args) ->
    Enigma = enigma:setup("B", {"II", "I", "III"},
			  {26, 23, 4},
			  [{$E, $Z}, {$B, $L}, {$X, $P}, {$W, $R}, {$I, $U},
			   {$V, $M}, {$J, $O}],
			  {$A, $G, $I}),
    Res = enigma:crypt(Enigma, Args),
    io:format("Result: ~p~n", [Res]).

normaliseAsciiNum(Num) -> Num rem 25 + 50.

%%====================================================================
%% Enigma parts
%%====================================================================

keyboard(Parent, Keys, Lamp, Inc) ->
    io:format("KB: ~p~n", [self()]),
    X = receives(Parent, x),
    broadcasts(Parent, self(), Inc, 1),
    broadcasts(Parent, self(), Keys, X),
    Output = receives(Parent, Lamp),
    broadcasts(Parent, self(), x, Output),
    keyboard(Parent, Keys, Lamp, Inc).

reflector(Parent, In, Out, Reflector) ->
    % io:format("RE: ~p ~n", [self()]),
    Key = receives(Parent, In),
    F_refl_result = f_refl(Reflector, Key, 1),
    io:format("[RE] ~p -> ~p.~n", [Key, F_refl_result]),
    broadcasts(Parent, self(), Out, F_refl_result),
    reflector(Parent, In, Out, Reflector).

f_refl(Reflector, Input, ElementToCheck) ->
    if (ElementToCheck < 1) or (ElementToCheck > 2) ->
	   Input;
       true ->
	   case lists:keyfind(Input, ElementToCheck, Reflector) of
	     false -> f_refl(Reflector, Input, ElementToCheck + 1);
	     Otherwise -> element(3 - ElementToCheck, Otherwise)
	   end
    end.

plugboard(Parent, Plugboard, Input, Output, Offset) ->
    % io:format("PB: ~p~n", [self()]),
    Key = receives(Parent, Input),
    F_plug_result = f_plug(Plugboard, Key),
    io:format("[PB] ~p -> ~p~n", [Key, F_plug_result]),
    broadcasts(Parent, self(), Output,
	       wrapChar(F_plug_result + Offset)),
    % Respond on the opposite channel this time.
    plugboard(Parent, Plugboard, Output, Input,
	      -1 * Offset).

f_plug(Plugboard, Input) -> f_refl(Plugboard, Input, 1).

% Todo: calculate f_rotor-result
% Todo: refactor, there's duplication here
rotorFunction(Parent, Right, Left, Rotor, P,
	      RotationDirection) ->
    OffsetValue = RotationDirection,
    rotorPass(Parent, Right, f_rotor, Left, Rotor, P,
	      OffsetValue),
    rotorPass(Parent, Left, inverse_f_rotor, Right, Rotor,
	      P, OffsetValue).

% params:

rotorPass(Parent, Input, EncryptionFunction, Output,
	  Rotor, P, RotationDirection) ->
    Key = wrapChar(receives(Parent, Input)),
    F_rotor_result = enigma:EncryptionFunction(Rotor,
					       P * RotationDirection, Key)
		       - P * RotationDirection,
    % io:format("[FR] Offset = ~p -> Out = ~p",
    %     [OutputOffset, [F_rotor_result]]),
    broadcasts(Parent, self(), Output,
	       wrapChar(F_rotor_result)).

% NB: offset's actually supposed to be direction and it's redundant
% the real offset is NotchPointOffset
rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P,
      Offset, Notch, FirstRotor, NotchPointOffset) ->
    io:format("[RO] ~p C = ~p, P = ~p ~n", [self(), C, P]),
    io:format("[RO] ~p receives on ~p ~n", [self(), Inc_R]),
    IncR = receives(Parent, Inc_R),
    NotchPoint = Notch,
    TurnPoint = wrapChar(25 + $A + NotchPointOffset),
    io:format("NPO: ~p, Turnpoint: ~p~n",
	      [NotchPointOffset, [TurnPoint]]),
    NotchBump = case wrapChar(P + $A) of
		  NotchPoint -> 1;
		  TurnPoint -> 0;
		  _ -> 0
		end,
    case NotchBump of
      1 ->
	  io:format("[~p/~p] Notching/turning. ~p -> N: ~p "
		    "W: ~p~n",
		    [C, P, [wrapChar(P + $A)], [NotchPoint], [TurnPoint]]);
      _ ->
	  io:format("[~p/~p] Am not notching/turning. ~p "
		    "~p ~p~n",
		    [C, P, [wrapChar(P + $A)], [NotchPoint], [TurnPoint]])
    end,
    io:format("NotchPoint for ~p (~p) at initial offset "
	      "~p is ~p. ~n",
	      [[Notch], Notch - $A, [NotchPointOffset + $A],
	       NotchPoint]),
    io:format("TurnPoint for ~p (~p) at initial offset "
	      "~p is ~p (that many chars from Z?).~n",
	      [[Notch], Notch - $A, [NotchPointOffset + $A],
	       [wrapChar(TurnPoint + $A)]]),
    case C of
      TurnPoint ->
	  % send inc to incl
	  io:format("~p (~p, ~p) is turning.~n",
		    [self(), C, [Notch]]),
	  broadcasts(Parent, self(), Inc_L, NotchBump),
	  io:format("[~p/~p], P = ~p + ~p = ~p~n",
		    [C, [wrapChar(P + $A)], P, IncR,
		     [wrapChar(P + IncR + $A)]]),
	  rotorFunction(Parent, Right, Left, Rotor, P + IncR,
			Offset),
	  case IncR of
	    1 ->
		rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, 1,
		      P - 25, Offset, Notch, FirstRotor, NotchPointOffset);
	    _ ->
		rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P,
		      Offset, Notch, FirstRotor, NotchPointOffset)
	  end;
      _ ->
	  %
	  % case C of
	  %   Notch -> io:format(">> [~p/~p] N) Sending ~p to next rotor...~n", [C, Notch, abs(IncR - 1)]);
	  %   _ -> io:format(">> [~p/~p] Sending ~p to next rotor...~n", [C, Notch, max(FirstRotor, 0)])
	  % end,
	  %
	  io:format("[~p/~p], P = ~p + ~p = ~p~n",
		    [C, [wrapChar(P + $A)], P, IncR,
		     [wrapChar(P + IncR + $A)]]),
	  broadcasts(Parent, self(), Inc_L, NotchBump),
	  rotorFunction(Parent, Right, Left, Rotor, P + IncR,
			Offset),
	  case IncR of
	    1 ->
		rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C + 1,
		      P + 1, Offset, Notch, FirstRotor, NotchPointOffset);
	    _ ->
		rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P,
		      Offset, Notch, FirstRotor, NotchPointOffset)
	  end
    end.

f_rotor(Rotor, P, X) ->
    NewCharacter = wrapChar(X + P),
    io:format("[FR] P = ~p, X = ~p, Result = ~p, Out "
	      "= ~p~n",
	      [P, [X], [NewCharacter],
	       [element(2, lists:keyfind(NewCharacter, 1, Rotor))]]),
    element(2, lists:keyfind(NewCharacter, 1, Rotor)).

inverse_f_rotor(Rotor, P, X) ->
    Character = element(1,
			lists:keyfind(wrapChar(X + P), 2, Rotor)),
    NewCharacter = Character,
    io:format("~p~n", [lists:keyfind(X + P, 2, Rotor)]),
    io:format("[FR]#P = ~p, X = ~p, Result = ~p, Out "
	      "= ~p~n",
	      [P, X,
	       element(1, lists:keyfind(wrapChar(X + P), 2, Rotor)),
	       NewCharacter]),
    io:format("[FR]#P = ~p, X = ~p, Result = ~p, Out "
	      "= ~p~n",
	      [P, [X], [wrapChar(X + P)], [NewCharacter]]),
    NewCharacter.    % NewCharacter = wrapToRange(P - X, $A, $Z),
		     % io:format("[FR]#P = ~p, X = ~p, Result = ~p, Out = ~p~n",

          %     [P, X, $A + NewCharacter, ($A + (element(1, lists:keyfind($A + NewCharacter, 2, Rotor)) rem 26))]),
    % ($A + (element(1, lists:keyfind($A + NewCharacter, 2, Rotor)) rem 26)).

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
    receive
      {broadcasts, Source, Channel, Value} ->
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
	  % io:format("~p <- ~p~n", [Target, Channel]),
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
    case Channel of
      x ->
	  io:format("[-> ~4w] ~p ~p~n",
		    [Channel, Source,
		     case Channel of
		       x -> [Value];
		       keys -> [Value];
		       m1 -> [Value];
		       m2 -> [Value];
		       m3 -> [Value];
		       _ -> Value
		     end]);
      _ -> io:format("")
    end,
    Target ! {broadcasts, Source, Channel, Value},
    if Channel == none -> ok;
       true ->
	   receive {ok, {broadcasts, Channel, Value}} -> ok end
    end.

% receives awaits a message on the channel, then returns its value.
receives(Parent, Channel) ->
    Parent ! {receives, Channel, self()},
    receive {receives, Channel, Value} -> Value end.

% This version awaits a message on the channel, then runs a callback function
% passed to it.
receives(Parent, Channel, Callback) ->
    Parent ! {receives, Channel, self()},
    receive {receives, Channel, Value} -> Callback() end.

% Takes an atom and a string, and returns the return value of the corresponding
% function in enigma.hrl.
listFor(Type, Name) ->
    FunctionName =
	list_to_atom(lists:flatten([atom_to_list(Type), Name])),
    enigma:FunctionName().

% Wraps to a range, inclusive of max.
wrapToRange(Input, Min, Max) ->
    if Input < Min ->
	   $A + ((Input - $A) rem 26 + 26) rem 26;
       true -> Min + (Input - Min) rem (Max + 1 - Min)
    end.

wrapChar(Input) -> wrapToRange(Input, $A, $Z).

notchFor(RotorNumber) ->
    element(2,
	    lists:keyfind(RotorNumber, 1,
			  [{"I", $Q}, {"II", $E}, {"III", $V}, {"IV", $J},
			   {"V", $Z}])).

% generateRotors(Parent, RotorNames, RingSettings, InitialSetting) ->
%   InputChannels = {m1, m2, m3},
%   OutputChannels = {ref, m1, m2},
%   rotor(Parent, incl, incr, right, left, c, p) element(1, RotorNames)
%   % for each rotorName:
%   Rotor = listFor(rotor, RotorName)
%   {
%     rotor(Parent, )
%   }

ringstellung(Rotor, Ringstellung) ->
    % How To Ringstellung
    % 1: Shift all the characters up by the Ringstellung minus 1!
    YShiftedRotorData = [{X,
			  wrapChar(Y + (Ringstellung - 1))}
			 || {X, Y} <- Rotor],
    % 2: Shift the other side up by the Ringstellung minus 1!
    XShiftedRotorData = [{wrapChar(X + (Ringstellung - 1)),
			  Y}
			 || {X, Y} <- YShiftedRotorData],
    XShiftedRotorData.

configureRotor(RotorName, Ringstellung) ->
    ringstellung(listFor(rotor, RotorName), Ringstellung).

%%====================================================================
%% Exported functions
%%====================================================================

enigma(ReflectorName, RotorNames, InitialSetting,
       PlugboardPairs, RingSettings) ->
    Reflector = spawn(enigma, reflector,
		      [self(), ref, ref, listFor(reflector, ReflectorName)]),
    % rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P) ->
    Rotor3 = spawn(enigma, rotor,
		   [self(),
		    configureRotor(element(1, RotorNames),
				   element(1, InitialSetting)),
		    none, i3, m1, ref, 0, element(1, RingSettings) - $A, 1,
		    notchFor(element(1, RotorNames)), 0,
		    element(1, RingSettings) - $A]),
    io:format("Rotor3: ~p~n", [Rotor3]),
    Rotor2 = spawn(enigma, rotor,
		   [self(),
		    configureRotor(element(2, RotorNames),
				   element(2, InitialSetting)),
		    i3, i2, m2, m1, 0, element(2, RingSettings) - $A, 1,
		    notchFor(element(2, RotorNames)), 0,
		    element(2, RingSettings) - $A]),
    Rotor1 = spawn(enigma, rotor,
		   [self(),
		    configureRotor(element(3, RotorNames),
				   element(3, InitialSetting)),
		    i2, i1, m3, m2, 0, element(3, RingSettings) - $A, 1,
		    notchFor(element(3, RotorNames)), 1,
		    element(3, RingSettings) - $A]),
    Plugboard = spawn(enigma, plugboard,
		      [self(), PlugboardPairs, keys, m3, 0]),
    Keyboard = spawn(enigma, keyboard,
		     [self(), keys, keys, i1]),
    message_broker([], []).

setup(ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting) ->
    spawn(enigma, enigma,
	  [ReflectorName, RotorNames, RingSettings,
	   PlugboardPairs, InitialSetting]).

crypt(Enigma_PID, TextString) ->
    % Convert everything to uppercase
    encryptWithState(Enigma_PID,
		     lists:filter(fun (X) -> (X =< $Z) and (X >= $A) end,
				  string:uppercase(TextString)),
		     []).

encryptWithState(Enigma_PID, TextString,
		 EncryptedString) ->
    io:format("---~nEncrypting ~p~n---~n", [TextString]),
    case TextString of
      [] -> lists:reverse(EncryptedString);
      [Head | Tail] ->
	  broadcasts(Enigma_PID, self(), x, Head),
	  EncryptedChar = receives(Enigma_PID, x),
	  encryptWithState(Enigma_PID, Tail,
			   [EncryptedChar | EncryptedString])
    end.

kill(Enigma_PID) -> ok.
