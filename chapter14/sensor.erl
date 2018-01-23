%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module( sensor ).
%-compile( export_all ).
-export(
  [
    gen/2,
    prep/1,
    loop/8,
    rng/2,
    rng1/2,
    xor_GetInput/3,
    pb_GetInput/3,
    dtm_GetInput/3
  ]
).
-include( "records.hrl" ).

% When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial
% state message.
gen( ExoSelf_PId, Node ) ->
  spawn( Node, ?MODULE, prep, [ ExoSelf_PId ] ).

prep( ExoSelf_PId ) ->
  receive
    { ExoSelf_PId, { Id, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds } } ->
      loop( Id, ExoSelf_PId, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds )
  end.

% The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be
% triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex
% requests so.
loop( Id, ExoSelf_PId, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds ) ->
  receive
    { Cx_PId, sync } ->
      SensoryVector = sensor:SensorName( VL, Parameters, Scape ),
      [ Pid ! { self(), forward, SensoryVector } || Pid <- Fanout_PIds ],
      loop( Id, ExoSelf_PId, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds );
    { ExoSelf_PId, terminate } ->
      io:format( "Sensor: ~p is terminating.~n", [ Id ] ),
      ok
  end.

% rng/2 is a simple random number generator that produces a vector of random values, each between 0
% and 1. The length of the vector is defined by the VL, which itself is specified within the sensor
% record.
rng( VL, _Scape ) ->
  rng1( VL, [] ) .
rng1( 0, Acc ) ->
  Acc;
rng1( VL, Acc ) ->
  rng1( VL - 1, [ rand:uniform() | Acc ] ).

% xor_GetInput/2 contacts the XOR simulator and requests the sensory vector, which in this case
% should be a vector of length 2. The sensor checks that the incoming sensory signal, the percept,
% is indeed of length 2. If the vector length differs, then this is printed to the console and a
% dummy vector of appropriate length is constructed and used. This prevents unnecessary crashes in
% the case of errors, and gives the researcher a chance to fix the error and hotswap the code.
xor_GetInput( VL, _Parameters, Scape ) ->
  Scape ! { self(), sense },
  receive
    { Scape, percept, SensoryVector } ->
      case VL == length( SensoryVector ) of
        true ->
          SensoryVector;
        false ->
          io:format(
            "Error in sensor:xor_sim/3, VL: ~p  SensoryVector: ~p~n",
            [ VL, SensoryVector ]
          ),
          lists:duplicate( VL, 0 )
      end
  end.

pb_GetInput( VL, Parameters, Scape ) ->
  Scape ! { self(), sense, Parameters },
  receive
    { Scape, percept, SensoryVector } ->
      case VL == length( SensoryVector ) of
        true ->
          SensoryVector;
        false ->
          io:format(
            "Error in sensor:pb_GetInput/3, VL: ~p SensoryVector: ~p~n",
            [ VL, SensoryVector ]
          ),
          lists:duplicate( VL, 0 )
      end
  end.

dtm_GetInput( VL, Parameters, Scape ) ->
  Scape ! { self(), sense, Parameters },
  receive
    { Scape, percept, SensoryVector } ->
      io:format( "self(): ~p  SensoryVector: ~p~n", [ self(), SensoryVector ] ),
      case VL == length( SensoryVector ) of
        true ->
          SensoryVector;
        false ->
          io:format(
            "Error in sensor:dtm_GetInput/3, VL: ~p SensoryVector: ~p~n",
            [ VL, SensoryVector ]
          ),
          lists:duplicate( VL, 0 )
      end
  end.

