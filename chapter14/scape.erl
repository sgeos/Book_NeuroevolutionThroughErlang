%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module( scape ).
%-compile( export_all ).
-export(
  [
    gen/2,
    prep/1,
    xor_sim/1,
    xor_sim/3,
    sse/3,
    pb_sim/1,
    pb_sim/2,
    sm_DoublePole/3,
    dtm_sim/1,
    dtm_sim/2,
    move/5,
    set_tmaze_sectors/0,
    distance/2
  ]
).
-include( "records.hrl" ).

% gen/2 is executed by the exoself. The function spawns prep/1 process, and awaits the name of the
% scape from the exoself. Each scape is a separate and independent process, a self contained system
% that was developed to interface with the sensors and actuators from which its name was extracted.
% The name of the scape is the name of its main process loop.
gen( ExoSelf_PId, Node ) ->
  spawn( Node, ?MODULE, prep, [ ExoSelf_PId ] ).

prep( ExoSelf_PId ) ->
  receive
    { ExoSelf_PId, Name } ->
      scape:Name( ExoSelf_PId )
  end.

% xor_sim/3 is a scape that simulates the XOR operation, interacts with the NN, and gages the NN’s
% performance. xor_sim expects two types of messages from the NN, one message from the sensor and one
% from the actuator. The message: {From,sense} prompts the scape to send the NN the percept, which is
% a vector of length 2 and contains the XOR input. The second expected message from the NN is the
% message from the actuator, which is expected to be an output of the NN and packaged into the form:
% {From,action,Output}. At this point xor_sim/3 compares the Output with the expected output that is
% associated with the sensory message that should have been gathered by the sensors, and then sends
% back to the actuator process a message composed of the scape’s PId, Fitness, and a HaltFlag which
% specifies whether the simulation has ended for the NN. The scape keeps track of the Mean Squared
% Error between the NN’s output and the correct output. Once the NN has processed all 4 signals for
% the XOR, the scape computes the total MSE, converts it to fitness, and finally forwards this
% fitness and the HaltFlag=1 to the NN. This particular scape uses the lifetime based fitness, rather
% than step-based fitness. During all the other steps the scape sends the actuator the signal:
% {Scape_PId,0,0}, while it accumulates the errors, and only at the very end does it calculate the
% total fitness, which is the inverse of the error with a small extra added value to avoid the divide
% by 0 errors. Afterwards, xor_sim resets back to its initial state and awaits anew for signals from
% the NN.
xor_sim( ExoSelf_PId ) ->
  XOR = [
    { [-1, -1], [-1] },
    { [-1,  1], [ 1] },
    { [ 1, -1], [ 1] },
    { [ 1,  1], [-1] }
  ],
  xor_sim( ExoSelf_PId, { XOR, XOR }, 0 ).

xor_sim( ExoSelf_PId, { [ { Input, CorrectOutput } | XOR ], MXOR }, ErrAcc ) ->
  receive
    { From, sense } ->
      From ! { self(), percept, Input },
      xor_sim( ExoSelf_PId, { [ { Input, CorrectOutput } | XOR ], MXOR }, ErrAcc );
    { From, action, Output } ->
      Error = sse( Output, CorrectOutput, 0 ),
      io:format( "{Output, CorrectOutput}: ~p~n", [ { Output, CorrectOutput } ] ),
      case XOR of
        [] ->
          SSE = ErrAcc + Error,
          Fitness = 1 / ( SSE + 0.000001 ),
          io:format( "SSE: ~p  Fitness: ~p~n", [ SSE, Fitness ] ),
          From ! { self(), Fitness, 1 },
          xor_sim( ExoSelf_PId, { MXOR, MXOR }, 0 );
        _ ->
          From ! { self(), 0, 0 },
          xor_sim( ExoSelf_PId, { XOR, MXOR }, ErrAcc + Error )
      end;
    { ExoSelf_PId, terminate } ->
      ok
  end.

sse( [ T | Target ], [ O | Output ], SSEAcc ) ->
  SSE = math:pow( T - O, 2 ),
  sse( Target, Output, SSE + SSEAcc );
sse( [], [], SSEAcc ) ->
  SSEAcc.

-record(
  pb_state,
  {
    cpos = 0,
    cvel = 0,
    p1_angle = 3.6 * ( 2 * math:pi() / 360 ),
    p1_vel = 0,
    p2_angle = 0,
    p2_vel = 0,
    time_step = 0,
    goal_steps = 90000,
    fitness_acc = 0
  }
).

% !!! Using cryptographically strong seed instead of erlang:now() and random:seed().
% !!! reference: (see How to generate cryptographically strong seeds with the new rand library)
% https://hashrocket.com/blog/posts/the-adventures-of-generating-random-numbers-in-erlang-and-elixir
%
pb_sim( ExoSelf_PId ) ->
  << I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer >> =
    crypto:strong_rand_bytes( 12 ),
  rand:seed( exsplus, { I1, I2, I3 } ),
  io:format( "Starting pb_sim: ~p~n", [ self() ] ),
  pb_sim( ExoSelf_PId, #pb_state{} ).
  
pb_sim( ExoSelf_PId, S ) ->
  receive
    { From_PId, sense, [ Parameter ] } ->
      io:format( "Sense request received: ~p~n", [ From_PId ] ),
      SenseSignal = case Parameter of
        cpos ->
          [ S#pb_state.cpos ];
        cvel ->
          [ S#pb_state.cvel ];
        p1_angle ->
          [ S#pb_state.p1_angle ];
        p1_vel ->
          [ S#pb_state.p1_vel ];
        p2_angle ->
          [ S#pb_state.p2_angle ];
        p2_vel ->
          [ S#pb_state.p2_vel ];
        2 ->
          [ S#pb_state.cpos, S#pb_state.p1_angle ];
        3 ->
          [ S#pb_state.cpos, S#pb_state.p1_angle, S#pb_state.p2_angle ];
        4 ->
          [ S#pb_state.cpos, S#pb_state.cvel, S#pb_state.p1_angle, S#pb_state.p1_vel ];
        6 ->
          [
            S#pb_state.cpos,
            S#pb_state.cvel,
            S#pb_state.p1_angle,
            S#pb_state.p1_vel,
            S#pb_state.p2_angle,
            S#pb_state.p2_vel
          ]
      end,
      From_PId ! { self(), percept, SenseSignal },
      scape:pb_sim( ExoSelf_PId, S );
    { From_PId, push, [ Damping_Flag, DPB_Flag ], [ F ] } ->
      AL = 2 * math:pi() * ( 36 / 360 ),
      U_S = sm_DoublePole( F * 10, S, 2 ),
      TimeStep = U_S#pb_state.time_step,
      CPos = U_S#pb_state.cpos,
      CVel = U_S#pb_state.cvel,
      PAngle1 = U_S#pb_state.p1_angle,
      PVel1 = U_S#pb_state.p1_vel,
      case
        ( AL < abs( PAngle1 ) ) or
        ( AL < abs( U_S#pb_state.p2_angle ) * DPB_Flag ) or
        ( 2.4 < abs( CPos ) ) or
        ( U_S#pb_state.goal_steps < TimeStep )
      of
        true ->
          case ( U_S#pb_state.goal_steps < TimeStep ) of
            % fitness goal reached
            true ->
              From_PId ! { self(), goal_reached, 1 },
              scape:pb_sim( ExoSelf_PId, #pb_state{} );
            false ->
              From_PId ! { self(), 0, 1 },
              scape:pb_sim( ExoSelf_PId, #pb_state{} )
          end;
        false ->
          Fitness = case Damping_Flag of
            without_damping ->
              1;
            with_damping ->
              Fitness1 = TimeStep / 1000,
              Fitness2 = case TimeStep < 100 of
                true ->
                  0;
                false ->
                  0.75 / ( abs( CPos ) + abs( CVel ) + abs( PAngle1 ) + abs( PVel1 ) )
              end,
              Fitness1 * 0.1 + Fitness2 * 0.9
          end,
          From_PId ! { self(), Fitness, 0 },
          scape:pb_sim(
            ExoSelf_PId,
            U_S#pb_state{ fitness_acc = U_S#pb_state.fitness_acc + Fitness }
          )
      end;
    { ExoSelf_PId, terminate } ->
      ok
  end.
  
sm_DoublePole( _F, S, 0) ->
  S#pb_state{ time_step = S#pb_state.time_step + 1 };
sm_DoublePole( F, S, SimStepIndex ) ->
  CPos = S#pb_state.cpos,
  CVel = S#pb_state.cvel,
  PAngle1 = S#pb_state.p1_angle,
  PAngle2 = S#pb_state.p2_angle,
  PVel1 = S#pb_state.p1_vel,
  PVel2 = S#pb_state.p2_vel,
  _X = CPos, % EdgePositions = [ -2.4, 2.4 ], % Why is X bound?  Underscore added.
  PHalfLength1 = 0.5, % half-length of pole 1
  PHalfLength2 = 0.05, % half-length of pole 2
  M = 1, % CartMass
  PMass1 = 0.1, % Pole1 mass
  PMass2 = 0.01, % Pole2 mass
  MUc = 0.0005, % Cart-Track Friction Coefficient
  MUp = 0.000002, % Pole-Hinge Friction Coefficient
  G = -9.81, % Gravity
  Delta = 0.01, % Timestep
  EM1 = PMass1 * ( 1 - ( 3 / 4 ) * math:pow( math:cos( PAngle1 ), 2 ) ),
  EM2 = PMass2 * ( 1 - ( 3 / 4 ) * math:pow( math:cos( PAngle2 ), 2 ) ),
  EF1 =
    PMass1 * PHalfLength1 * math:pow( PVel1, 2 ) * math:sin( PAngle1 ) +
    ( 3 / 4 ) * PMass1 * math:cos( PAngle1 ) *
    ( ( ( MUp * PVel1 ) / (PMass1 * PHalfLength1 ) ) + G * math:sin( PAngle1 ) ),
  EF2 =
    PMass2 * PHalfLength2 * math:pow( PVel2, 2 ) * math:sin( PAngle2 ) +
    ( 3 / 4 ) * PMass2 * math:cos( PAngle2 ) *
    ( ( ( MUp * PVel2 ) / ( PMass1 * PHalfLength2 ) ) + G * math:sin( PAngle2 ) ),
  NextCAccel = ( F - MUc * functions:sgn( CVel ) + EF1 + EF2 ) / ( M + EM1 + EM2 ),
  NextPAccel1 =
    -( 3 / ( 4 * PHalfLength1 ) ) * ( ( NextCAccel * math:cos( PAngle1 ) ) +
    ( G * math:sin( PAngle1 ) ) + ( ( MUp * PVel1 ) / ( PMass1 * PHalfLength1 ) ) ),
  NextPAccel2 =
    -( 3 / ( 4 * PHalfLength2 ) ) * ( ( NextCAccel * math:cos( PAngle2 ) ) +
    ( G * math:sin( PAngle2 ) ) + ( ( MUp * PVel2 ) / ( PMass2 * PHalfLength2 ) ) ),
  NextCVel = CVel + ( Delta * NextCAccel ),
  NextCPos = CPos + ( Delta * CVel ),
  NextPVel1 = PVel1 + ( Delta * NextPAccel1 ),
  NextPAngle1 = PAngle1 + ( Delta * NextPVel1 ),
  NextPVel2 = PVel2 + ( Delta * NextPAccel2 ),
  NextPAngle2 = PAngle2 + ( Delta * NextPVel2 ),
  U_S = S#pb_state{
    cpos = NextCPos,
    cvel = NextCVel,
    p1_angle = NextPAngle1,
    p1_vel = NextPVel1,
    p2_angle = NextPAngle2,
    p2_vel = NextPVel2
  },
  sm_DoublePole( 0, U_S, SimStepIndex - 1 ).
  
-record(
  dtm_sector,
  {
    id,
    description = [],
    r
  }
).
-record(
  dtm_state,
  {
    agent_position = [ 0, 0 ],
    agent_direction = 90,
    sectors = set_tmaze_sectors(),
    tot_runs = 100,
    run_index = 0,
    switch_event = 35 + rand:uniform( 30 ),
    switched = false,
    step_index = 0,
    fitness_acc = 50
  }
).

% !!! Using cryptographically strong seed instead of erlang:now() and random:seed().
% !!! reference: (see How to generate cryptographically strong seeds with the new rand library)
% https://hashrocket.com/blog/posts/the-adventures-of-generating-random-numbers-in-erlang-and-elixir
%
dtm_sim( ExoSelf_PId ) ->
  io:format( "Starting dtm_sim~n" ),
  << I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer >> =
    crypto:strong_rand_bytes( 12 ),
  rand:seed( exsplus, { I1, I2, I3 } ),
  io:format( "Starting pb_sim: ~p~n", [ self() ] ),
  dtm_sim( ExoSelf_PId, #dtm_state{} ).

dtm_sim( ExoSelf_PId, S )
  when ( S#dtm_state.run_index == S#dtm_state.switch_event ) and ( S#dtm_state.switched == false ) ->
  io:format( "Switch event: ~p~n", [ S#dtm_state.switch_event ] ),
  Sectors = S#dtm_state.sectors,
  SectorA = lists:keyfind( [ +1, 1 ], 2, Sectors ),
  SectorB = lists:keyfind( [ -1, 1 ], 2, Sectors ),
  U_SectorA = SectorA#dtm_sector{ r = SectorB#dtm_sector.r },
  U_SectorB = SectorB#dtm_sector{ r = SectorA#dtm_sector.r },
  U_Sectors = lists:keyreplace(
    [ -1, 1 ],
    2,
    lists:keyreplace( [ 1, 1 ], 2, Sectors, U_SectorA ),
    U_SectorB
  ),
  scape:dtm_sim( ExoSelf_PId, S#dtm_state{ sectors = U_Sectors, switched = true } );
dtm_sim( ExoSelf_PId, S ) ->
  receive
    { From_PId, sense, Parameters } ->
      io:format( "Sense: ~p~n", [ Parameters ] ),
      APos = S#dtm_state.agent_position,
      ADir = S#dtm_state.agent_direction,
      Sector = lists:keyfind( APos, 2, S#dtm_state.sectors ),
      { ADir, _NextSec, RangeSense } = lists:keyfind( ADir, 1, Sector#dtm_sector.description ),
      SenseSignal = case Parameters of
        [ all ] ->
          RangeSense ++ [ Sector#dtm_sector.r ];
        [ range_sense ] ->
          RangeSense;
        [ reward ] ->
          [ Sector#dtm_sector.r ]
      end,
      io:format( "Position: ~p  SenseSignal: ~p ", [ APos, SenseSignal ] ),
      From_PId ! { self(), percept, SenseSignal },
      scape:dtm_sim( ExoSelf_PId, S );
    { From_PId, move, _Parameters, [ Move ] } ->
      timer:sleep( 500 ),
      APos = S#dtm_state.agent_position,
      ADir = S#dtm_state.agent_direction,
      Sector = lists:keyfind( APos, 2, S#dtm_state.sectors ),
      U_StepIndex = S#dtm_state.step_index + 1,
      io:format( "S: ~p~n", [ S ] ),
      io:format(
        "Move: ~p  StepIndex: ~p  RunIndex: ~p~n",
        [ Move, U_StepIndex, S#dtm_state.run_index ]
      ),
      { ADir, NextSec, _RangeSense } = lists:keyfind( ADir, 1, Sector#dtm_sector.description ),
      if
        ( APos == [ 1, 1 ] ) or ( APos == [ -1, 1 ] ) ->
          Updated_RunIndex = S#dtm_state.run_index + 1,
          case S#dtm_state.tot_runs =< Updated_RunIndex of
            true ->
              From_PId ! { self(), S#dtm_state.fitness_acc + Sector#dtm_sector.r, 1 },
              io:format( "Ok1: ~p~n", [ S#dtm_state.fitness_acc ] ),
              dtm_sim( ExoSelf_PId, #dtm_state{} );
            false ->
              From_PId ! { self(), 0, 0 },
              U_S = S#dtm_state{
                agent_position = [ 0, 0 ],
                agent_direction = 90,
                run_index = Updated_RunIndex,
                step_index = 0,
                fitness_acc = S#dtm_state.fitness_acc + Sector#dtm_sector.r
              },
              dtm_sim( ExoSelf_PId, U_S )
          end;
        % clockwise
        0.33 < Move ->
          NewDir = ( S#dtm_state.agent_direction + 270 ) rem 360,
          { NewDir, NewNextSec, _NewRangeSense } =
            lists:keyfind( NewDir, 1, Sector#dtm_sector.description ),
          U_S = move(
            ExoSelf_PId,
            From_PId,
            S#dtm_state{
              agent_direction = NewDir
            },
            NewNextSec,
            U_StepIndex
          ),
          dtm_sim( ExoSelf_PId, U_S );
        % counterclockwise
        Move < -0.33 ->
          NewDir = ( S#dtm_state.agent_direction + 90 ) rem 360,
          { NewDir, NewNextSec, _NewRangeSense } =
            lists:keyfind( NewDir, 1, Sector#dtm_sector.description ),
          U_S = move(
            ExoSelf_PId,
            From_PId,S#dtm_state{
              agent_direction = NewDir
            },
            NewNextSec,
            U_StepIndex
          ),
          dtm_sim( ExoSelf_PId, U_S );
        % forward
        true ->
          move( ExoSelf_PId, From_PId, S, NextSec, U_StepIndex )
      end;
    { ExoSelf_PId, terminate } ->
      ok
  end.

move( ExoSelf_PId, From_PId, S, NextSec, U_StepIndex ) ->
  case NextSec of
    % wall crash/restart_state
    [] ->
      Updated_RunIndex = S#dtm_state.run_index + 1,
      case S#dtm_state.tot_runs =< Updated_RunIndex of
        true ->
          From_PId ! { self(), S#dtm_state.fitness_acc - 0.4, 1 },
          io:format( "Ok: ~p~n", [ S#dtm_state.fitness_acc - 0.4 ] ),
          dtm_sim( ExoSelf_PId, #dtm_state{} );
        false ->
          From_PId ! { self(), 0, 0 },
          U_S = S#dtm_state{
            agent_position = [ 0, 0 ],
            agent_direction = 90,
            run_index = Updated_RunIndex,
            step_index = 0,
            fitness_acc = S#dtm_state.fitness_acc - 0.4
          },
          dtm_sim( ExoSelf_PId, U_S )
        end;
    % move
    _ ->
      From_PId ! { self(), 0, 0 },
      U_S = S#dtm_state{
        agent_position = NextSec,
        step_index = U_StepIndex
      },
      dtm_sim( ExoSelf_PId, U_S )
  end.

set_tmaze_sectors() ->
  Sectors = [
    #dtm_sector{
      id = [ 0, 0 ],
      description = [
        { 0, [], [ 1, 0, 0 ] },
        { 90, [ 0, 1 ], [ 0, 1, 0 ] },
        { 180, [], [ 0, 0, 1 ] },
        { 270, [], [ 0, 0, 0 ] }
      ],
      r = 0
    },
    #dtm_sector{
      id = [ 0, 1 ],
      description = [
        { 0, [ 1, 1 ], [ 0, 1, 1 ] },
        { 90, [], [ 1, 0, 1 ] },
        { 180, [ -1, 1 ], [ 1, 1, 0 ] },
        { 270, [ 0, 0 ], [ 1, 1, 1 ] }
      ],
      r=0
    },
    #dtm_sector{
      id = [ 1, 1 ],
      description = [
        { 0, [], [ 0, 0, 0 ] },
        { 90, [],[ 2, 0, 0 ] },
        { 180, [ 0, 1 ], [ 0, 2, 0 ] },
        { 270, [], [ 0, 0, 2 ] }
      ],
      r = 1
    },
    #dtm_sector{
      id = [ -1, 1 ],
      description = [
        { 0, [ 0, 1 ], [ 0, 2, 0 ] },
        { 90, [], [ 0, 0, 2 ] },
        { 180, [], [ 0, 0, 0 ] },
        { 270, [], [ 2, 0, 0 ] }
      ],
      r = 0.2
    }
  ],
  Sectors.

distance( Vector1, Vector2 ) ->
  distance( Vector1, Vector2, 0 ).

distance( [ Val1 | Vector1 ], [ Val2 | Vector2 ], Acc ) ->
  distance( Vector1, Vector2, Acc + math:pow( Val2 - Val1, 2 ) );
distance( [], [], Acc ) ->
  math:sqrt( Acc ).

