-module(enigma).

-include("enigma.hrl").

-compile(export_all).

%% escript entry point
%% Gets called by scripts/run
main(Args) ->
    % Parent = spawn(enigma, message_broker, [[], []]),
    % Out = spawn(enigma, receives, [Parent, out]),
    % io:format("Spawning reflector.~n"),
    % Reflector = spawn(enigma, reflector, [Parent, in, out, reflectorB()]),
    % io:format("Reflector PID: ~p~n", [Reflector]),
    % io:format("Broadcasting with args: ~p, ~p, ~p~n", [Reflector, in, $X]),
    % broadcasts(Parent, Reflector, in, $X),
    % receive {close} -> erlang:halt(0) end,

    % Enigma = enigma:setup("B",{"III","II","I"},{1,1,1},[], {$A,$A,$A}),
    % Res = enigma:crypt(Enigma,Args),
    % io:format("starting reverse~n---~n"),
    % Enigma2 = enigma:setup("B",{"III","II","I"},{1,1,1},[], {$A,$A,$A}),
    % Res2 = enigma:crypt(Enigma2,Res),
    % io:format("Done! Check this out: ~p~n", [Res]),
    % io:format("Done! Check this out: ~p~n", [Res2]).
    
    Enigma = enigma:setup("B",{"I","II","III"},{1,1,1},[], {$A,$A,$A}),
    Res = enigma:crypt(Enigma,Args),
    io:format("starting reverse~n---~n"),
    % Enigma2 = enigma:setup("B",{"I","II","III"},{1,1,1},[], {$A,$A,$A}),
    % Res2 = enigma:crypt(Enigma2,Res),
    io:format("Done! Check this out: ~p~n", [Res]).
    % io:format("Done! Check this out: ~p~n", [Res2]).

    % Rotor = spawn(enigma, rotor,
    %   [Parent, incl, incr, r, l, 0, 0]),
    % IncR = spawn(enigma, receives, [Parent, incr]),
    % L = spawn(enigma, receives, [Parent, l]),
    % io:format("RF: ~p~n", [Rotor]),
    % broadcasts(Parent, self(), incl, 1),
    % broadcasts(Parent, self(), r, 69),
    % io:format("Broadcasting with args: ~p, ~p, ~p~n", [Parent, l, $E]),

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
    io:format("[RE] ~p -> ~p.~n", [[Key], [F_refl_result]]),
    broadcasts(Parent, self(), Out, F_refl_result),
    reflector(Parent, In, Out, Reflector).

f_refl(Reflector, Input, ElementToCheck) ->
    if ((ElementToCheck < 1) or (ElementToCheck > 2)) ->
      Input;
    true ->
      case lists:keyfind(Input, ElementToCheck, Reflector) of
        false ->
          f_refl(Reflector, Input, ElementToCheck + 1);
        Otherwise ->
          element(3 - ElementToCheck, Otherwise)
      end
    end.

plugboard(Parent, Plugboard, Input, Output, Offset) ->
    % io:format("PB: ~p~n", [self()]),
    Key = receives(Parent, Input),
    F_plug_result = f_plug(Plugboard, Key),
    io:format("[PB] ~p -> ~p~n",
	      [Key, F_plug_result]),
    broadcasts(Parent, self(), Output, wrapToRange(F_plug_result + Offset, $A, $Z)),
    % Respond on the opposite channel this time.
    plugboard(Parent, Plugboard, Output, Input, (-1*Offset)).

f_plug(Plugboard, Input) -> f_refl(Plugboard, Input, 1).

% Todo: calculate f_rotor-result
% Todo: refactor, there's duplication here
rotorFunction(Parent, Right, Left, Rotor, P, Offset) ->
    OffsetValue = Offset,
    rotorPass(Parent, Right, f_rotor, Left, Rotor, P, 0, OffsetValue),
    rotorPass(Parent, Left, inverse_f_rotor, Right, Rotor,
	      P, 0, OffsetValue).

% params:

rotorPass(Parent, Input, EncryptionFunction, Output, Rotor, P, InputOffset, OutputOffset) ->
    Key = receives(Parent, Input),
    F_rotor_result = enigma:EncryptionFunction(Rotor, P * OutputOffset,
						 Key),
    % io:format("[FR] Offset = ~p -> Out = ~p",
	  %     [OutputOffset, [F_rotor_result]]),
    broadcasts(Parent, self(), Output, F_rotor_result).

rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P, Offset, Notch, FirstRotor) ->
    io:format("[RO] ~p C = ~p, P = ~p ~n", [self(), C, P]),
    io:format("[RO] ~p receives on ~p ~n", [self(), Inc_R]),
    IncR = receives(Parent, Inc_R),
    Break = lists:member(wrapToRange(C + P, $A, $Z), [$Z + 1, Notch]),
    case Break of
      true ->
        % send inc to incl
        io:format("~p (~p, ~p) is going up.~n", [self(), C, Notch]),
        broadcasts(Parent, self(), Inc_L, case C of Notch -> 1; _ -> 0 end),
        io:format("[~p/~p], P = ~p + ~p~n", [C, Notch, P, IncR]),
        rotorFunction(Parent, Right, Left, Rotor, P + IncR, Offset),
        case IncR of
          1 -> rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, $A, P - 25, Offset, Notch, FirstRotor);
          _ -> rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P, Offset, Notch, FirstRotor) end;
      _ ->
        %
        % case C of 
        %   Notch -> io:format(">> [~p/~p] N) Sending ~p to next rotor...~n", [C, Notch, abs(IncR - 1)]);
        %   _ -> io:format(">> [~p/~p] Sending ~p to next rotor...~n", [C, Notch, max(FirstRotor, 0)])
        % end,
        %
        broadcasts(Parent, self(), Inc_L, max(FirstRotor, abs(IncR - 1))),
        io:format("[~p/~p], P = ~p + ~p~n", [C, Notch, P, IncR]),
        rotorFunction(Parent, Right, Left, Rotor, P + IncR, Offset),
        case IncR of
          1 -> rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P + 1, Offset, Notch, FirstRotor);
          _ -> rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P, Offset, Notch, FirstRotor) end
    end.

f_rotor(Rotor, P, X) ->
    NewCharacter = wrapToRange(X + (P), $A, $Z),
    io:format("[FR] P = ~p, X = ~p, Result = ~p, Out = ~p~n",
	      [P, [X], [NewCharacter], [element(2, lists:keyfind(NewCharacter, 1, Rotor))]]),
    element(2, lists:keyfind(NewCharacter, 1, Rotor)).

inverse_f_rotor(Rotor, P, X) ->
    Character = element(1, lists:keyfind(X, 2, Rotor)),
    NewCharacter = wrapToRange(Character - (P), $A, $Z),
    io:format("[FR]#P = ~p, X = ~p, Result = ~p, Out = ~p~n",
	      [P, [X], [Character], [NewCharacter]]),
    NewCharacter.
    % NewCharacter = wrapToRange(P - X, $A, $Z),
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
      x -> io:format("[-> ~4w] ~p ~p~n", [Channel, Source, case Channel of x -> [Value]; keys -> [Value]; m1 -> [Value]; m2 -> [Value]; m3 -> [Value]; _ -> Value end]);
      _ -> io:format("")
    end,
    Target ! {broadcasts, Source, Channel, Value},
    if Channel == none ->
      ok;
    true ->
      receive {ok, {broadcasts, Channel, Value}} -> ok end
    end.

% receives awaits a message on the channel, then returns its value.
receives(Parent, Channel) ->
    Parent ! {receives, Channel, self()},
    receive
      {receives, Channel, Value} ->
	  Value
    end.

% This version awaits a message on the channel, then runs a callback function
% passed to it.
receives(Parent, Channel, Callback) ->
    Parent ! {receives, Channel, self()},
    receive
      {receives, Channel, Value} ->
	  Callback()
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

notchFor(RotorNumber) ->
  element(2, lists:keyfind(RotorNumber, 1, [
    {"I", $Q},
    {"II", $E},
    {"III", $V},
    {"IV", $J},
    {"V", $Z}
  ])).

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

enigma(ReflectorName, RotorNames, InitialSetting,
      PlugboardPairs, RingSettings) ->
    Reflector = spawn(enigma, reflector,
		      [self(), ref, ref, listFor(reflector, ReflectorName)]),
% rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P) ->
    Rotor3 = spawn(enigma, rotor,
		   [self(), listFor(rotor, element(1, RotorNames)), none, i3, m1, ref, element(1, RingSettings),
		    element(1, InitialSetting) - 1, -1, notchFor(element(1, RotorNames)), 0]),
    io:format("Rotor3: ~p~n", [Rotor3]),
    Rotor2 = spawn(enigma, rotor,
		   [self(), listFor(rotor, element(2, RotorNames)), i3, i2, m2, m1, element(2, RingSettings),
		    element(2, InitialSetting) - 1, -1, notchFor(element(2, RotorNames)), 0]),
    Rotor1 = spawn(enigma, rotor,
		   [self(), listFor(rotor, element(3, RotorNames)), i2, i1, m3, m2, element(3, RingSettings),
		    element(3, InitialSetting) - 1, 1, notchFor(element(3, RotorNames)), 1]),
    Plugboard = spawn(enigma, plugboard,
		      [self(), PlugboardPairs, keys, m3, 0]),
    Keyboard = spawn(enigma, keyboard,
		     [self(), keys, keys, i1]),
    message_broker([], []).

setup(ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting) ->
    spawn(enigma, enigma, [ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting]).

crypt(Enigma_PID, TextString) ->
    % Convert everything to uppercase
    encryptWithState(Enigma_PID, string:uppercase(TextString), []).

encryptWithState(Enigma_PID, TextString, EncryptedString) ->
    io:format("---~nEncrypting ~p~n---~n", [TextString]),
    case TextString of
      [] -> lists:reverse(EncryptedString);
      [Head|Tail] ->
        broadcasts(Enigma_PID, self(), x, Head),
        EncryptedChar = receives(Enigma_PID, x),
        encryptWithState(Enigma_PID, Tail, [EncryptedChar|EncryptedString])
    end.

kill(Enigma_PID) -> ok.
