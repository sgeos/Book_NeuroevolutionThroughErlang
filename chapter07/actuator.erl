%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module(actuator).
%-compile(export_all).
-export(
  [
    gen/2,
    prep/1,
    loop/7,
    pts/2,
    xor_SendOutput/2
  ]
).
-include("records.hrl").

% When gen/2 is executed it spawns the actuator element and immediately begins to wait for its
% initial state message.
gen(ExoSelf_PId, Node) ->
  spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
  receive
    {ExoSelf_PId, {Id, Cx_PId, Scape, ActuatorName, Fanin_PIds}} ->
      loop(Id, ExoSelf_PId, Cx_PId, Scape, ActuatorName, {Fanin_PIds, Fanin_PIds}, [])
  end.

% The actuator process gathers the control signals from the neurons, appending them to the
% accumulator. The order in which the signals are accumulated into a vector is in the same order as
% the neuron ids are stored within NIds. Once all the signals have been gathered, the actuator sends
% cortex the sync signal, executes its function, and then again begins to wait for the neural signals
% from the output layer by reseting the Fanin_PIds from the second copy of the list.
loop(Id, ExoSelf_PId, Cx_PId, Scape, AName, {[From_PId | Fanin_PIds], MFanin_PIds}, Acc) ->
  receive
    {From_PId, forward, Input} ->
      loop(
        Id,
        ExoSelf_PId,
        Cx_PId,
        Scape,
        AName,
        {Fanin_PIds, MFanin_PIds},
        lists:append(Input, Acc)
      );
    {ExoSelf_PId, terminate} ->
      ok
  end;
loop(Id, ExoSelf_PId, Cx_PId, Scape, AName, {[], MFanin_PIds}, Acc) ->
  {Fitness, EndFlag} = actuator:AName(lists:reverse(Acc), Scape),
  Cx_PId ! {self(), sync, Fitness, EndFlag},
  loop(Id, ExoSelf_PId, Cx_PId, Scape, AName, {MFanin_PIds, MFanin_PIds}, []).

% --- --- --- Actuators --- ---- ---

% The pts actuation function simply prints to screen the vector passed to it.
pts(Result, _Scape) ->
  io:format("actuator:pts(Result): ~p~n", [Result]),
  {1, 0}. % {Fitness, EndFlag}

xor_SendOutput(Output, Scape) ->
  Scape ! { self(), action, Output },
  receive
    { Scape, Fitness, HaltFlag } ->
      { Fitness, HaltFlag }
  end.

