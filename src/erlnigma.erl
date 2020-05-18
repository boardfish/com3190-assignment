-module(erlnigma).
-include("enigma.hrl").
-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

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

% http://erlang.org/eeps/eep-0043.html

setup ( ReflectorName , RotorNames , RingSettings , PlugboardPairs , InitialSetting ) ->
  ok. 

crypt (Enigma_PID, TextString) ->
  ok.

kill (Enigma_PID) ->
  ok.
