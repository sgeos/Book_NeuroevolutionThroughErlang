%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module( actuator ).
%-compile( export_all ).
-export(
  [
    gen/2,
    prep/1,
    loop/8,
    pts/3,
    xor_SendOutput/3,
    pb_SendOutput/3,
    dtm_SendOutput/3
  ]
).
-include( "records.hrl" ).

% When gen/2 is executed it spawns the actuator element and immediately begins to wait for its
% initial state message.
gen( ExoSelf_PId, Node ) ->
  spawn( Node, ?MODULE, prep, [ ExoSelf_PId ] ).

prep( ExoSelf_PId ) ->
  receive
    { ExoSelf_PId, { Id, Cx_PId, Scape, ActuatorName, Parameters, Fanin_PIds } } ->
      loop(
        Id,
        ExoSelf_PId,
        Cx_PId,
        Scape,
        ActuatorName,
        Parameters,
        { Fanin_PIds, Fanin_PIds },
        []
      )
  end.

% The actuator process gathers the control signals from the neurons, appending them to the
% accumulator. The order in which the signals are accumulated into a vector is in the same order as
% the neuron ids are stored within NIds. Once all the signals have been gathered, the actuator sends
% cortex the sync signal, executes its function, and then again begins to wait for the neural signals
% from the output layer by reseting the Fanin_PIds from the second copy of the list.
loop(
  Id,
  ExoSelf_PId,
  Cx_PId,
  Scape,
  AName,
  Parameters,
  { [ From_PId | Fanin_PIds ], MFanin_PIds },
  Acc
) ->
  receive
    { From_PId, forward, Input } ->
      loop(
        Id,
        ExoSelf_PId,
        Cx_PId,
        Scape,
        AName,
        Parameters,
        { Fanin_PIds, MFanin_PIds },
        lists:append( Input, Acc )
      );
    { ExoSelf_PId, terminate } ->
      io:format( "Actuator: ~p is terminating.~n", [ self() ] ),
      ok
  end;
loop( Id, ExoSelf_PId, Cx_PId, Scape, AName, Parameters, { [], MFanin_PIds }, Acc ) ->
  { Fitness, EndFlag } = actuator:AName( lists:reverse( Acc ), Parameters, Scape ),
  Cx_PId ! { self(), sync, Fitness, EndFlag },
  loop( Id, ExoSelf_PId, Cx_PId, Scape, AName, Parameters, { MFanin_PIds, MFanin_PIds }, [] ).

% --- --- --- Actuators --- ---- ---

% The pts actuation function simply prints to screen the vector passed to it.
pts( Result, _Parameters, _Scape ) ->
  io:format( "actuator:pts(Result): ~p~n", [ Result ] ),
  { 1, 0 }. % { Fitness, HaltFlag }

% xor_SendOutput/2 function simply forwards the Output vector to the XOR simulator, and waits for the
% resulting Fitness and EndFlag from the simulation process.
xor_SendOutput( Output, _Parameters, Scape ) ->
  Scape ! { self(), action, Output },
  receive
    { Scape, Fitness, HaltFlag } ->
      { Fitness, HaltFlag }
  end.

pb_SendOutput( Output, Parameters, Scape ) ->
  Scape ! { self(), push, Parameters, Output },
  receive
    { Scape, Fitness, HaltFlag } ->
      { Fitness, HaltFlag }
  end.

dtm_SendOutput( Output, Parameters, Scape ) ->
  Scape ! { self(), move, Parameters, Output },
  receive
    { Scape, Fitness, HaltFlag } ->
      io:format( "self(): ~p  Fitness: ~p  HaltFlag: ~p~n", [ self(), Fitness, HaltFlag ] ),
      { Fitness, HaltFlag }
  end.

