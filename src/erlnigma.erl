-module(erlnigma).

%% API exports
-export([main/1, index_of/2, reflectorA/0]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

%%====================================================================
%% Rotor/Reflector Headers
%%====================================================================

rotorI() ->
    [{$A, $E}, {$B, $K}, {$C, $M}, {$D, $F}, {$E, $L},
     {$F, $G}, {$G, $D}, {$H, $Q}, {$I, $V}, {$J, $Z},
     {$K, $N}, {$L, $T}, {$M, $O}, {$N, $W}, {$O, $Y},
     {$P, $H}, {$Q, $X}, {$R, $U}, {$S, $S}, {$T, $P},
     {$U, $A}, {$V, $I}, {$W, $B}, {$X, $R}, {$Y, $C},
     {$Z, $J}].

rotorII() ->
    [{$A, $A}, {$B, $J}, {$C, $D}, {$D, $K}, {$E, $S},
     {$F, $I}, {$G, $R}, {$H, $U}, {$I, $X}, {$J, $B},
     {$K, $L}, {$L, $H}, {$M, $W}, {$N, $T}, {$O, $M},
     {$P, $C}, {$Q, $Q}, {$R, $G}, {$S, $Z}, {$T, $N},
     {$U, $P}, {$V, $Y}, {$W, $F}, {$X, $V}, {$Y, $O},
     {$Z, $E}].

rotorIII() ->
    [{$A, $B}, {$B, $D}, {$C, $F}, {$D, $H}, {$E, $J},
     {$F, $L}, {$G, $C}, {$H, $P}, {$I, $R}, {$J, $T},
     {$K, $X}, {$L, $V}, {$M, $Z}, {$N, $N}, {$O, $Y},
     {$P, $E}, {$Q, $I}, {$R, $W}, {$S, $G}, {$T, $A},
     {$U, $K}, {$V, $M}, {$W, $U}, {$X, $S}, {$Y, $Q},
     {$Z, $O}].

rotorIV() ->
    [{$A, $E}, {$B, $S}, {$C, $O}, {$D, $V}, {$E, $P},
     {$F, $Z}, {$G, $J}, {$H, $A}, {$I, $Y}, {$J, $Q},
     {$K, $U}, {$L, $I}, {$M, $R}, {$N, $H}, {$O, $X},
     {$P, $L}, {$Q, $N}, {$R, $F}, {$S, $T}, {$T, $G},
     {$U, $K}, {$V, $D}, {$W, $C}, {$X, $M}, {$Y, $W},
     {$Z, $B}].

rotorV() ->
    [{$A, $V}, {$B, $Z}, {$C, $B}, {$D, $R}, {$E, $G},
     {$F, $I}, {$G, $T}, {$H, $Y}, {$I, $U}, {$J, $P},
     {$K, $S}, {$L, $D}, {$M, $N}, {$N, $H}, {$O, $L},
     {$P, $X}, {$Q, $A}, {$R, $W}, {$S, $M}, {$T, $J},
     {$U, $Q}, {$V, $O}, {$W, $F}, {$X, $E}, {$Y, $C},
     {$Z, $K}].

rotorVI() ->
    [{$A, $J}, {$B, $P}, {$C, $G}, {$D, $V}, {$E, $O},
     {$F, $U}, {$G, $M}, {$H, $F}, {$I, $Y}, {$J, $Q},
     {$K, $B}, {$L, $E}, {$M, $N}, {$N, $H}, {$O, $Z},
     {$P, $R}, {$Q, $D}, {$R, $K}, {$S, $A}, {$T, $S},
     {$U, $X}, {$V, $L}, {$W, $I}, {$X, $C}, {$Y, $T},
     {$Z, $W}].

rotorVII() ->
    [{$A, $N}, {$B, $Z}, {$C, $J}, {$D, $H}, {$E, $G},
     {$F, $R}, {$G, $C}, {$H, $X}, {$I, $M}, {$J, $Y},
     {$K, $S}, {$L, $W}, {$M, $B}, {$N, $O}, {$O, $U},
     {$P, $F}, {$Q, $A}, {$R, $I}, {$S, $V}, {$T, $L},
     {$U, $P}, {$V, $E}, {$W, $K}, {$X, $Q}, {$Y, $D},
     {$Z, $T}].

rotorVIII() ->
    [{$A, $F}, {$B, $K}, {$C, $Q}, {$D, $H}, {$E, $T},
     {$F, $L}, {$G, $X}, {$H, $O}, {$I, $C}, {$J, $B},
     {$K, $J}, {$L, $S}, {$M, $P}, {$N, $D}, {$O, $Z},
     {$P, $R}, {$Q, $A}, {$R, $M}, {$S, $E}, {$T, $W},
     {$U, $N}, {$V, $I}, {$W, $U}, {$X, $Y}, {$Y, $G},
     {$Z, $V}].

rotorBeta() ->
    [{$A, $L}, {$B, $E}, {$C, $Y}, {$D, $J}, {$E, $V},
     {$F, $C}, {$G, $N}, {$H, $I}, {$I, $X}, {$J, $W},
     {$K, $P}, {$L, $B}, {$M, $Q}, {$N, $M}, {$O, $D},
     {$P, $R}, {$Q, $T}, {$R, $A}, {$S, $K}, {$T, $Z},
     {$U, $G}, {$V, $F}, {$W, $U}, {$X, $H}, {$Y, $O},
     {$Z, $S}].

rotorGamma() ->
    [{$A, $F}, {$B, $S}, {$C, $O}, {$D, $K}, {$E, $A},
     {$F, $N}, {$G, $U}, {$H, $E}, {$I, $R}, {$J, $H},
     {$K, $M}, {$L, $B}, {$M, $T}, {$N, $I}, {$O, $Y},
     {$P, $C}, {$Q, $W}, {$R, $L}, {$S, $Q}, {$T, $P},
     {$U, $Z}, {$V, $X}, {$W, $V}, {$X, $G}, {$Y, $J},
     {$Z, $D}].

reflectorA() ->
    [{$A, $E}, {$B, $J}, {$C, $M}, {$D, $Z}, {$F, $L},
     {$G, $Y}, {$H, $X}, {$I, $V}, {$K, $W}, {$N, $R},
     {$O, $Q}, {$P, $U}, {$S, $T}].

reflectorB() ->
    [{$A, $Y}, {$B, $R}, {$C, $U}, {$D, $H}, {$E, $Q},
     {$F, $S}, {$G, $L}, {$I, $P}, {$J, $X}, {$K, $N},
     {$M, $O}, {$T, $Z}, {$V, $W}].

reflectorC() ->
    [{$A, $F}, {$B, $V}, {$C, $P}, {$D, $J}, {$E, $I},
     {$G, $O}, {$H, $Y}, {$K, $R}, {$L, $Z}, {$M, $X},
     {$N, $W}, {$T, $Q}, {$S, $U}].

reflectorThinB() ->
    [{$A, $E}, {$B, $N}, {$C, $K}, {$D, $Q}, {$F, $U},
     {$G, $Y}, {$H, $W}, {$I, $J}, {$L, $O}, {$M, $P},
     {$R, $X}, {$S, $Z}, {$T, $V}].

reflectorThinC() ->
    [{$A, $R}, {$B, $D}, {$C, $O}, {$E, $J}, {$F, $N},
     {$G, $T}, {$H, $K}, {$I, $V}, {$L, $M}, {$P, $W},
     {$Q, $Z}, {$S, $X}, {$U, $Y}].

%%====================================================================
%% Helpers functions
%%====================================================================

broadcast(Parent,Receiver,Value) ->
  Parent!{o,Receiver,{self(),Value}}.

receiver(Parent,Channel) ->
  Parent!{i,Channel,{self(),0}},
  receive
    {i,Channel,{_, Result}} ->
      Result
    end.

%%====================================================================
%% Internal functions
%%====================================================================

% Schema for each part is as follows:
% - Each process knows which one created it
% - The rest of its arguments are processes that can talk to it, or it talks to

% message schema - {[i/o],Channel_PID,contents}
% reflector(Parent, In, Out) ->
%   X = receiver(Parent,In),
%   broadcast(Parent, Out, f_refl(X)),
%   reflector(Parent, In, Out).

% f_refl(x) ->
%   lists:filter(fun({X,Y}reflectorA()
%   ok.

index_of(Char, List) ->
  string:str(lists:map(fun({Key, _}) -> Key end, List), [Char]).

setup ( ReflectorName , RotorNames , RingSettings , PlugboardPairs , InitialSetting ) ->
  ok. 

crypt (Enigma_PID, TextString) ->
  ok.

kill (Enigma_PID) ->
  ok.
