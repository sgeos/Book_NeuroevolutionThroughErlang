%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module( tuning_duration ).
%-compile( export_all ).
-export(
  [
    const/3,
    wsize_proportional/3,
    extract_RecGenNIds/4,
    extract_NWeightCount/2,
    nsize_proportional/3
  ]
).
-include( "records.hrl" ).

% const/3 returns the preset const max_attempts value.
const( Parameter, _N_Ids, _Generation ) ->
  ConstMaxAttempts = Parameter,
  ConstMaxAttempts.

% wsize_proportional/3 calculats the max_attempts value based on the individual agent's parameters,
% in this case the max_attempts is proportional to the agent's number of weights belonging to the
% neurons which were added or mutated within the last 3 generations.
wsize_proportional( Parameter, N_Ids, Generation ) ->
  Power = Parameter,
  Active_NIds = extract_RecGenNIds( N_Ids, Generation, 3, [] ),
  Tot_ActiveNeuron_Weights = extract_NWeightCount( Active_NIds, 0 ),
  20 + functions:sat( round( math:pow( Tot_ActiveNeuron_Weights, Power ) ), 100, 0 ).

% extract_RecGenNIds/4 extracts the NIds of all neurons whose age is lower or equal to the AgeLimit.
extract_RecGenNIds( [ N_Id | N_Ids ], Generation, AgeLimit, Acc ) ->
  N = genotype:dirty_read( { neuron, N_Id } ),
  NeuronGen = N#neuron.generation,
  case ( Generation - AgeLimit ) =< NeuronGen of
    true ->
      extract_RecGenNIds( N_Ids, Generation, AgeLimit, [ N_Id | Acc ] );
    false ->
      extract_RecGenNIds( N_Ids, Generation, AgeLimit, Acc )
  end;
extract_RecGenNIds( [], _Generation, _AgeLimit, Acc ) ->
  Acc.

% extract_NWeightCount/2 counts the number of weights in total belonging to the list of neuron ids
% that the function was called with.
extract_NWeightCount( [ N_Id | RecGenN_Ids ], Acc ) ->
  N = genotype:dirty_read( { neuron, N_Id } ),
  Input_IdPs = N#neuron.input_idps,
  TotWeights = lists:sum( [ length( Weights ) || { _IId, Weights } <- Input_IdPs ] ),
  extract_NWeightCount( RecGenN_Ids, TotWeights + Acc );
extract_NWeightCount( [], Acc ) ->
  Acc.

% nsize_proportional/3 calculates the max_attempts to be proportional to the number of neurons which
% were within the last 3 generations mutated or added to the NN.
nsize_proportional( Parameter, N_Ids, Generation ) ->
  Power = Parameter,
  Tot_Neurons = length( extract_RecGenNIds( N_Ids, Generation, 3, [] ) ),
  20 + functions:sat( round( math:pow( Tot_Neurons, Power ) ), 100, 0 ).

