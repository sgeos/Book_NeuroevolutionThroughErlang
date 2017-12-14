%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

% !!! No morphology.erl module in chapter06; split from constructor.erl?
% !!! Unknown morphology called "test" from "previous chapter."

-module(morphology).
%-compile(export_all).
-export(
  [
    get_InitSensor/1,
    get_InitActuator/1,
    get_Sensors/1,
    get_Actuators/1,
    xor_mimic/1,
    generate_id/0
  ]
).
-include("records.hrl").

% --- --- --- Get Init Actuators / Sensors --- --- --

get_InitSensor(Morphology) ->
  Sensors = morphology:Morphology(sensors),
  lists:nth(1, Sensors).

get_InitActuator(Morphology) ->
  Actuators = morphology:Morphology(actuators),
  lists:nth(1, Actuators).

get_Sensors(Morphology) ->
  morphology:Morphology(sensors).

get_Actuators(Morphology) ->
  morphology:Morphology(actuators).

% --- --- --- Morphologies --- --- ---

% Every sensor and actuator uses some kind of function associated with it. A function that either
% polls the environment for sensory signals (in the case of a sensor) or acts upon the environment
% (in the case of an actuator). It is a function that we need to define and program before it is
% used, and the name of the function is the same as the name of the sensor or actuator it self. For
% example, the create_Sensor/1 has specified only the rng sensor, because that is the only sensor
% function we've finished developing. The rng function has its own vl specification, which will
% determine the number of weights that a neuron will need to allocate if it is to accept this
% sensor's output vector. The same principles apply to the create_Actuator function. Both,
% create_Sensor and create_Actuator function, given the name of the sensor or actuator, will return
% a record with all the specifications of that element, each with its own unique Id.
xor_mimic(sensors) ->
  [
    #sensor{id={sensor, generate_id()}, name=xor_GetInput, scape={private, xor_sim}, vl=2}
  ];
xor_mimic(actuators) ->
  [
    #actuator{id={actuator, generate_id()}, name=xor_SendOutput, scape={private, xor_sim}, vl=1}
  ].

% !!! Consider using something like erlang:unique_integer([monotonic]) instead of erlang:timestamp().
% !!! reference: http://erlang.org/doc/apps/erts/time_correction.html (How to Work with the New API)
generate_id() ->
  {MegaSeconds, Seconds, MicroSeconds} = erlang:timestamp(),
  1 / (MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).

