%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module( cortex ).
%-compile( export_all ).
-export(
  [
    gen/2,
    prep/1,
    loop/9
  ]
).
-include( "records.hrl" ).
%-record(
%  state,
%  {
%    id,
%    exoself_pid,
%    spids,
%    npids,
%    apids,
%    cycle_acc = 0,
%    fitness_acc = 0,
%    endflag = 0,
%    status
%  }
%).

% The gen/2 function spawns the cortex element, which immediately starts to wait for its initial
% state message from the same process that spawned it, exoself. The initial state message contains
% the sensor, actuator, and neuron PId lists. Before dropping into the main loop, CycleAcc,
% FitnessAcc, and HFAcc (HaltFlag Acc), are all set to 0, and the status of the cortex is set to
% active, prompting it to begin the synchronization process and call the sensors to action.
gen( ExoSelf_PId, Node ) ->
  spawn( Node, ?MODULE, prep, [ ExoSelf_PId ] ).

% !!! Using cryptographically strong seed instead of erlang:now() and random:seed().
% !!! reference: (see How to generate cryptographically strong seeds with the new rand library)
% https://hashrocket.com/blog/posts/the-adventures-of-generating-random-numbers-in-erlang-and-elixir
%
% !!! Consider using erlang:monotonic_time() instead of erlang:timestamp().
% !!! reference: http://erlang.org/doc/apps/erts/time_correction.html (How to Work with the New API)
prep( ExoSelf_PId ) ->
  << I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer >> =
    crypto:strong_rand_bytes( 12 ),
  rand:seed( exsplus, { I1, I2, I3 } ),
  receive
    { ExoSelf_PId, Id, SPIds, NPIds, APIds } ->
      put( start_time, erlang:timestamp() ),
      [ SPId ! { self(), sync } || SPId <- SPIds ],
      loop( Id, ExoSelf_PId, SPIds, { APIds, APIds }, NPIds, 1, 0, 0, active )
  end.

% !!! Consider using erlang:monotonic_time() instead of erlang:timestamp().
% !!! Consider using ordinary subtraction instead of timer:now_diff().
% !!! reference: http://erlang.org/doc/apps/erts/time_correction.html (How to Work with the New API)
%
% The cortex's goal is to synchronize the NN system's sensors and actuators. When the actuators have
% received all their control signals, they forward the sync messages, the Fitness, and the HaltFlag
% messages to the cortex. The cortex accumulates these Fitness and HaltFlag signals, and if any of
% the HaltFlag signals have been set to 1, HFAcc will be greater than 0, signifying that the cortex
% should halt. When EFAcc > 0, the cortex calculates the total amount of time it has ran (TimeDiff),
% and forwards to exoself the values: FitnessAcc, CycleAcc, and TimeDiff. Afterwards, the cortex
% enters the inactive mode and awaits further instructions from the exoself. If none of the HaltFlags
% were set to 0, then the value HFAcc == 0, and the cortex triggers off another Sense-Think-Act
% cycle. The reason the cortex process stores 2 copies of the actuator PIds: the APIds, and the
% MemoryAPIds (MAPIds), is so that once all the actuators have sent it the sync messages, it can
% restore the APIds list from the MAPIds.
loop(
  Id,
  ExoSelf_PId,
  SPIds,
  { [ APId | APIds ], MAPIds },
  NPIds,
  CycleAcc,
  FitnessAcc,
  EFAcc,
  active
) ->
  receive
    { APId, sync, Fitness, EndFlag } ->
      io:format( "FitnessAcc: ~p~n", [ FitnessAcc ] ),
      case Fitness == goal_reached of
        true ->
          put( goal_reached, true ),
          loop(
            Id,
            ExoSelf_PId,
            SPIds,
            { APIds, MAPIds },
            NPIds,
            CycleAcc,
            FitnessAcc + Fitness,
            EFAcc + EndFlag,
            active
          );
        false ->
          loop(
            Id,
            ExoSelf_PId,
            SPIds,
            { APIds, MAPIds },
            NPIds,
            CycleAcc,
            FitnessAcc + Fitness,
            EFAcc + EndFlag,
            active
          )
      end;
    terminate ->
      io:format( "Cortex: ~p is terminating.~n", [ Id ] ),
      [ PId ! { self(), terminate } || PId <- SPIds ],
      [ PId ! { self(), terminate } || PId <- MAPIds ],
      [ PId ! { self(), terminate } || PId <- NPIds ]
  end;
loop( Id, ExoSelf_PId, SPIds, { [], MAPIds }, NPIds, CycleAcc, FitnessAcc, EFAcc, active ) ->
  case 0 < EFAcc of
    % Organism finished evaluation
    true ->
      TimeDif = timer:now_diff( erlang:timestamp(), get( start_time ) ),
      ExoSelf_PId ! {
       	self(),
       	evaluation_completed,
       	FitnessAcc,
       	CycleAcc,
       	TimeDif,
       	get( goal_reached )
      },
      cortex:loop(
        Id,
        ExoSelf_PId,
        SPIds,
        { MAPIds, MAPIds },
        NPIds,
        CycleAcc,
        FitnessAcc,
        EFAcc,
        inactive
      );
    false ->
      [ PId ! { self(), sync } || PId <- SPIds  ],
      cortex:loop(
        Id,
        ExoSelf_PId,
        SPIds,
        { MAPIds, MAPIds },
        NPIds,
        CycleAcc + 1,
        FitnessAcc,
        EFAcc,
        active
      )
  end;
loop(
  Id,
  ExoSelf_PId,
  SPIds,
  { MAPIds, MAPIds },
  NPIds,
  _CycleAcc,
  FitnessAcc,
  _EFAcc,
  inactive
) ->
  receive
    { ExoSelf_PId, reactivate } ->
      put( start_time, erlang:timestamp() ),
      [ SPId ! { self(), sync } || SPId <- SPIds ],
      io:format( "FitnessAcc: ~p~n", [ FitnessAcc ] ),
      cortex:loop( Id, ExoSelf_PId, SPIds, { MAPIds, MAPIds }, NPIds, 1, 0, 0, active );
    { ExoSelf_PId, terminate } ->
      io:format( "Cortex: ~p is terminating.~n", [ Id ] ),
      ok
  end.

