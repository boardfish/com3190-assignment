-module(enigma).

-include("enigma.hrl").

-compile(export_all).

%% escript entry point
%% Call this by running the following in a shell:
%% rebar3 escriptize && ./_build/default/bin/enigma "Text you want to encrypt \
%% here"
%% This won't work with the single file for assignment submission.
%% ---
%% Development note: I used this often, passing Res to a second machine's crypt
%% function to make sure that my implementation was bijective all the way
%% through.
main(Args) ->
    Enigma = enigma:setup("B", {"II", "I", "III"},
			  {26, 23, 4},
			  [{$E, $Z}, {$B, $L}, {$X, $P}, {$W, $R}, {$I, $U},
			   {$V, $M}, {$J, $O}],
			  {$A, $G, $I}),
    Res = enigma:crypt(Enigma, Args),
    io:format("Result: ~p~n", [Res]).

%%====================================================================
%% Enigma parts
%%====================================================================

%% Keyboard: Reflects the initial specification using receives and broadcasts.
%% I tested this by observing the broadcasts and receives - early in
%% development, both methods logged as they were called, allowing me to do this
%% with ease. After devising the message broker, I could spawn dummy receives to
%% stub the Keys channel, and broadcast messages to mock the input from the Lamp
%% channel.
keyboard(Parent, Keys, Lamp, Inc) ->
    X = receives(Parent, x),
    broadcasts(Parent, Inc, 1),
    broadcasts(Parent, Keys, X),
    Output = receives(Parent, Lamp),
    broadcasts(Parent, x, Output),
    keyboard(Parent, Keys, Lamp, Inc).

%% Reflector: Also reflects the initial spec. This was one of the earlier parts
%% I worked on, as it was easy to both implement and test in isolation - it
%% didn't have any state of its own, unlike the rotors. I tested it in a similar
%% manner to the keyboard, with mocking and stubbing of the inlet and outlet.
reflector(Parent, In, Out, Reflector) ->
    Key = receives(Parent, In),
    F_refl_result = f_refl(Reflector, Key, 1),
    broadcasts(Parent, Out, F_refl_result),
    reflector(Parent, In, Out, Reflector).

%% f_refl checks the reflector at both sides for the character it wishes to
%% reflect. The element of the reflector tuples that is being checked is
%% ElementToCheck - if this goes out of range, the original input is returned as
%% a fallback.
f_refl(Reflector, Input, ElementToCheck) ->
    if (ElementToCheck < 1) or (ElementToCheck > 2) ->
	   Input;
       true ->
	   case lists:keyfind(Input, ElementToCheck, Reflector) of
	     false -> f_refl(Reflector, Input, ElementToCheck + 1);
	     Otherwise -> element(3 - ElementToCheck, Otherwise)
	   end
    end.

%% My implementation of Plugboard deviates from the original spec - rather than
%% a choice of either channel being available, it only responds on the channel
%% that should be accepting input. If the input is making its way towards the
%% reflector from the keyboard, it accepts on the right channel. After taking
%% that input, it accepts on the left channel by swapping its output and input
%% channels.
plugboard(Parent, Plugboard, Input, Output) ->
    Key = receives(Parent, Input),
    F_plug_result = f_plug(Plugboard, Key),
    broadcasts(Parent, Output, wrapChar(F_plug_result)),
    % Respond on the opposite channel this time.
    plugboard(Parent, Plugboard, Output, Input).

%% f_plug works in the same way as f_refl, as both map characters to just one
%% other bijectively. Thus, it's safe to inherit that behaviour.
f_plug(Plugboard, Input) -> f_refl(Plugboard, Input, 1).

%% RotorFunction is implemented similarly to Plugboard - rather than accepting
%% on both channels, it accepts on the right channel first, then on the left.
%% In this instance, it's necessary to specify the function that's being
%% executed, as f_rotor isn't bijective and needs a specified inverse.
rotorFunction(Parent, Right, Left, Rotor, P) ->
    rotorPass(Parent, Right, f_rotor, Left, Rotor, P),
    rotorPass(Parent, Left, inverse_f_rotor, Right, Rotor,
	      P).

%% rotorPass represents one pass of data through a rotor, receiving on an input
%% and transforming it before outputting it.
%% The result of the rotor function is offset by P anticlockwise.
rotorPass(Parent, Input, EncryptionFunction, Output,
	  Rotor, P) ->
    Key = wrapChar(receives(Parent, Input)),
    F_rotor_result = enigma:EncryptionFunction(Rotor, P,
					       Key)
		       - P,
    broadcasts(Parent, Output, wrapChar(F_rotor_result)).

%% This is the rotor process. It takes all its channels and C and P as arguments
rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P,
      Notch, NotchPointOffset) ->
    IncR = receives(Parent, Inc_R),
    NotchPoint = Notch,
    TurnPoint = wrapChar(25 + $A + NotchPointOffset),
    NotchBump = case wrapChar(P + $A) of
		  NotchPoint -> 1;
		  _ -> 0
		end,
    case C of
      TurnPoint ->
	  % send inc to incl
	  broadcasts(Parent, Inc_L, NotchBump),
	  rotorFunction(Parent, Right, Left, Rotor, P + IncR),
	  case IncR of
	    1 ->
		rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, 0,
		      P - 25, Notch, NotchPointOffset);
	    _ ->
		rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left, C, P,
		      Notch, NotchPointOffset)
	  end;
      _ ->
	  broadcasts(Parent, Inc_L, NotchBump),
	  rotorFunction(Parent, Right, Left, Rotor, P + IncR),
	  rotor(Parent, Rotor, Inc_L, Inc_R, Right, Left,
		C + IncR, P + IncR, Notch, NotchPointOffset)
    end.

% f_rotor takes the result of offsetting X by P, then looks it up on the rotor.
f_rotor(Rotor, P, X) ->
    NewCharacter = wrapChar(X + P),
    element(2, lists:keyfind(NewCharacter, 1, Rotor)).

%% f_rotor takes the result of offsetting X by P, then does a reverse lookup on
%% the rotor.
inverse_f_rotor(Rotor, P, X) ->
    Character = element(1,
			lists:keyfind(wrapChar(X + P), 2, Rotor)),
    NewCharacter = Character,
    NewCharacter.

%%====================================================================
%% Helper functions
%%====================================================================

%% Shared message broker. Each of the Enigma parts communicates on channels
%% whose names are passed into each one as arguments. The message broker, along
%% with broadcasts and receives, make sure that each process blocks until a
%% fitting message comes in on the right channel. This makes it a lot easier for
%% me to move from the pi-calculus representation to Erlang code and also means
%% that processes don't depend on PIDs (that might not yet exist) on startup.
message_broker(RegReceivers, UnsentMsgs) ->
    receive
      {broadcasts, _, commands, kill} -> ok;
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

%% The broadcasts helper function alerts the message broker (Target) of a
%% message (Value) on some channel (Channel). The message broker keeps this
%% message until it can be delegated to a channel that is receiving. The source
%% process that called broadcasts must be passed along so that it can be
%% notified when its input has been addresssed.
broadcasts(Target, Channel, Value) ->
    Target ! {broadcasts, self(), Channel, Value},
    case Channel of
      none -> ok;
      _ ->
	  receive {ok, {broadcasts, Channel, Value}} -> ok end
    end.

%% receives alerts the message broker (Parent) that a channel is ready to
%% receive a message. It then awaits a response from the broker on the channel,
%% and returns its value.
receives(Parent, Channel) ->
    Parent ! {receives, Channel, self()},
    receive {receives, Channel, Value} -> Value end.

%% Takes an atom and a string, and returns the return value of the corresponding
%% function in enigma.hrl. e.g. listFor(rotor, "III") -> rotorIII().
listFor(Type, Name) ->
    FunctionName =
	list_to_atom(lists:flatten([atom_to_list(Type), Name])),
    enigma:FunctionName().

%% Wraps numbers to a range, inclusive of max. Only ever used to wrap characters
%% that have been offset, thus wrapChar.
wrapToRange(Input, Min, Max) ->
    Range = Max - Min + 1,
    if Input < Min ->
	   Min + ((Input - Min) rem Range + Range) rem Range;
       true -> Min + (Input - Min) rem Range
    end.

%% Wraps characters that have been offset. wrapChar($Z + 1) = $A.
%% wrapChar($A - 1) = $Z.
wrapChar(Input) -> wrapToRange(Input, $A, $Z).

%% Returns the character at which the notch sits for any of the first five
%% rotors.
notchFor(RotorNumber) ->
    element(2,
	    lists:keyfind(RotorNumber, 1,
			  [{"I", $Q}, {"II", $E}, {"III", $V}, {"IV", $J},
			   {"V", $Z}])).

%% Calculates a rotor's Ringstellung as detailed. A Ringstellung of {1,1,1} is
%% considered the default, and thus doesn't result in any offsetting at all.
ringstellung(Rotor, Ringstellung) ->
    % 1: Shift all the characters up by the Ringstellung minus 1
    YShiftedRotorData = [{X,
			  wrapChar(Y + (Ringstellung - 1))}
			 || {X, Y} <- Rotor],
    % 2: Shift the other side up by the Ringstellung minus 1
    XShiftedRotorData = [{wrapChar(X + (Ringstellung - 1)),
			  Y}
			 || {X, Y} <- YShiftedRotorData],
    XShiftedRotorData.

%% Gets a rotor list and sets its Ringstellung in one fell swoop.
%% E.g. configureRotor("I", 3) gets you rotorI in C Ringstellung.
configureRotor(RotorName, Ringstellung) ->
    ringstellung(listFor(rotor, RotorName), Ringstellung).

%%====================================================================
%% Exported functions
%%====================================================================

%% Host process for the Enigma itself. Spawns all of the parts of the Enigma,
%% wired up on the standard channels suggested in the assignment document, and
%% hosts the message broker process.
enigma(ReflectorName, RotorNames, InitialSetting,
       PlugboardPairs, RingSettings) ->
    spawn_link(enigma, reflector,
	       [self(), ref, ref, listFor(reflector, ReflectorName)]),
    spawn_link(enigma, rotor,
	       [self(),
		configureRotor(element(1, RotorNames),
			       element(1, InitialSetting)),
		none, i3, m1, ref, 0, element(1, RingSettings) - $A,
		notchFor(element(1, RotorNames)),
		element(1, RingSettings) - $A]),
    spawn_link(enigma, rotor,
	       [self(),
		configureRotor(element(2, RotorNames),
			       element(2, InitialSetting)),
		i3, i2, m2, m1, 0, element(2, RingSettings) - $A,
		notchFor(element(2, RotorNames)),
		element(2, RingSettings) - $A]),
    spawn_link(enigma, rotor,
	       [self(),
		configureRotor(element(3, RotorNames),
			       element(3, InitialSetting)),
		i2, i1, m3, m2, 0, element(3, RingSettings) - $A,
		notchFor(element(3, RotorNames)),
		element(3, RingSettings) - $A]),
    spawn_link(enigma, plugboard,
	       [self(), PlugboardPairs, keys, m3]),
    spawn_link(enigma, keyboard, [self(), keys, keys, i1]),
    message_broker([], []).

%% Spawns, and returns the PID of, an Enigma machine process.
setup(ReflectorName, RotorNames, RingSettings,
      PlugboardPairs, InitialSetting) ->
    spawn(enigma, enigma,
	  [ReflectorName, RotorNames, RingSettings,
	   PlugboardPairs, InitialSetting]).

%% Returns the string of responses from the Enigma machine. The handling of
%% communications is delegated to a child process that keeps track of state.
%% Converts everything to uppercase, then strips out anything that's not [A-Z].
crypt(Enigma_PID, TextString) ->
    encryptWithState(Enigma_PID,
		     lists:filter(fun (X) -> (X =< $Z) and (X >= $A) end,
				  string:uppercase(TextString)),
		     []).

%% Handles communications with the Enigma, passing in characters on channel x
%% and listening for the Enigma's output on the same channel. The responses from
%% the Enigma are collected in reverse order (i.e. appending to the head, rather
%% than the tail), so the list is reversed on output.
encryptWithState(Enigma_PID, TextString,
		 EncryptedString) ->
    case TextString of
      [] -> lists:reverse(EncryptedString);
      [Head | Tail] ->
	  broadcasts(Enigma_PID, x, Head),
	  EncryptedChar = receives(Enigma_PID, x),
	  encryptWithState(Enigma_PID, Tail,
			   [EncryptedChar | EncryptedString])
    end.

%% Sends a kill signal to an Engima PID. It figures that if all the processes
%% are linked to the Enigma and it dies, they should die with it. However, I
%% couldn't find a way to repeatably test this in time for the deadline.
%% It might have something to do with the message broker possibly having a
%% different PID to the Enigma itself
kill(Enigma_PID) ->
    broadcasts(Enigma_PID, commands, kill).
