%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module( tuning_selection ).
%-compile( export_all ).
-export(
  [
    dynamic/4,
    extract_CurGenNIdPs/6,
    dynamic_random/4,
    choose_randomNIdPs/2,
    choose_randomNIdPs/3,
    active/4,
    active_random/4,
    current/4,
    current_random/4,
    all/4,
    all_random/4
  ]
).
-include( "records.hrl" ).

% The dynamic/4 selection function randomly selects an age limit for its neuron id pool. The age
% limit is chosen by executing math:sqrt(1/random:uniform()), which creates a value between 1 and
% infinity. Using this function there is 75% that the number will be =<2, 25% that it will be >=2,
% 11% that it will be >=3... Everytime this selection function is executed, the AgeLimit is generated
% anew, thus different times it will produce different neuron id pools for tuning.
dynamic( N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter ) ->
  AgeLimit = math:sqrt( 1 / rand:uniform() ),
  ChosenN_IdPs = case
    extract_CurGenNIdPs(
      N_Ids,
      AgentGeneration,
      AgeLimit,
      PerturbationRange,
      AnnealingParameter,
      []
    )
  of
    [] ->
      [ N_Id | _ ] = N_Ids,
      [ { N_Id, PerturbationRange * math:pi() } ];
    ExtractedN_IdPs ->
      ExtractedN_IdPs
  end,
  %io:format( "ChosenN_IdPs: ~p~n", [ ChosenN_IdPs ] ),
  ChosenN_IdPs.

% The extract_CurGenNIdPs/6 composes a neuron id pool from neurons who are younger than the AgeLimit
% parameter. This is calculated by comparing the generation when they were created or touched by
% mutation, with that of the agent which ages with every topological mutation phase. Id pool
% accumulates not just the neurons but also the spread which will be used for the synaptic weight
% perturbation. The spread is calculated by multiplying the perturbation_range variable by math:pi(),
% and then multiplied by the annealing factor which is math:pow(AnnealingParameter,Age). Annealing
% parameter is less than 1, thus the greater the age of the neuron, the lower the Spread will be.
extract_CurGenNIdPs( [ N_Id | N_Ids ], Generation, AgeLimit, PR, AP, Acc ) ->
  N = genotype:dirty_read( { neuron, N_Id } ),
    NeuronGen = N#neuron.generation,
    case ( Generation - AgeLimit ) =< NeuronGen of
      true ->
        Age = Generation - NeuronGen,
        % Spread = PR * math:pi() * math:pow( 0.5, Age ),
        Spread = PR * math:pi() * math:pow( AP, Age ),
        extract_CurGenNIdPs( N_Ids, Generation, AgeLimit, PR, AP, [ { N_Id, Spread } | Acc ] );
      false ->
        extract_CurGenNIdPs( N_Ids, Generation, AgeLimit, PR, AP, Acc )
    end;
extract_CurGenNIdPs( [], _Generation, _AgeLimit, _PR, _AP, Acc ) ->
  Acc.

% dyanimic_random/4 selection function composes the neuron id pool the same way as the dynamic/4
% selection function, but after this id pool is generated, this selection function extracts ids from
% it randomly with a probability of 1/math:sqrt(Tot_Neurons). Thus the probability of a neuron being
% selected from this pool is proportional to the number of ids in that pool. If through chance no ids
% are selected, then the first element in the id pool is automatically selected, and given the
% highest spread.
dynamic_random( N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter ) ->
  ChosenN_IdPs = case
    extract_CurGenNIdPs(
      N_Ids,
      AgentGeneration,
      math:sqrt( 1 / rand:uniform() ),
      PerturbationRange,
      AnnealingParameter,
      []
    )
  of
    [] ->
      [ N_Id | _ ] = N_Ids,
      [ { N_Id, PerturbationRange * math:pi() } ];
    ExtractedN_IdPs->
      ExtractedN_IdPs
  end,
  io:format( "ChosenN_IdPs: ~p~n", [ ChosenN_IdPs ] ),
  Tot_Neurons = length( ChosenN_IdPs ),
  MutationP = 1 / math:sqrt( Tot_Neurons ),
  choose_randomNIdPs( MutationP, ChosenN_IdPs ).

% choose_randomNIdPs/2 and choose_randomNIdPs/3 accepts a mutation probability parameter and a list
% of tuples composed of neuron ids and their spreads, and then selects from this list randomly with a
% probability MutationP, composing a new sub list.
choose_randomNIdPs( MutationP, N_IdPs ) ->
  case choose_randomNIdPs( N_IdPs, MutationP, [] ) of
    [] ->
      { NId, Spread } = lists:nth( rand:uniform( length( N_IdPs ) ), N_IdPs ),
      [ { NId, Spread } ];
    Acc ->
      Acc
  end.

choose_randomNIdPs( [ { NId, Spread } | N_IdPs ], MutationP, Acc ) ->
  U_Acc = case rand:uniform() < MutationP of
    true ->
      [ { NId, Spread } | Acc ];
    false ->
      Acc
  end,
  choose_randomNIdPs( N_IdPs, MutationP, U_Acc );
choose_randomNIdPs( [], _MutationP, Acc ) ->
  Acc.

% active/4 selection algorithm composes a neuron id pool from all neurons who are younger than 3
% generations.
active( N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter ) ->
  extract_CurGenNIdPs( N_Ids, AgentGeneration, 3, PerturbationRange, AnnealingParameter, [] ).

% active_random/4 is a selection algorithm that composes an id pool by first creating a list of all
% neurons who are younger than 3 generations, and then composing a sub list from it by randomly
% choosing elements from this list with a probability of 1/math:sqrt(Tot_Neurons).
active_random( N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter ) ->
  ChosenN_IdPs = case
    extract_CurGenNIdPs(
      N_Ids,
      AgentGeneration,
      3,
      PerturbationRange,
      AnnealingParameter,
      []
    )
  of
    [] ->
      [ N_Id | _ ] = N_Ids,
      [ { N_Id, PerturbationRange * math:pi() } ];
    ExtractedN_IdPs ->
      ExtractedN_IdPs
  end,
  Tot_Neurons = length( ChosenN_IdPs ),
  MutationP = 1 / math:sqrt( Tot_Neurons ),
  choose_randomNIdPs( MutationP, ChosenN_IdPs ).

% current/4 is a selection algorithm that returns a list of all neurons which have been added to the
% NN, or affected by mutation, during the last generation.
current( N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter ) ->
  case extract_CurGenNIdPs( N_Ids, AgentGeneration, 0, PerturbationRange, AnnealingParameter, [] ) of
    [] ->
      [ N_Id | _ ] = N_Ids,
      [ { N_Id, PerturbationRange * math:pi() } ];
    IdPs ->
      IdPs
  end.

% current_random/4 composes the list of tuples in the same way as current/4 does, but then composes a
% sublist by randomly selecting elements from that list with a probability of
% 1/math:sqrt(Tot_Neurons), and returning that to the caller.
current_random( N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter ) ->
  ChosenN_IdPs = case
    extract_CurGenNIdPs(
      N_Ids,
      AgentGeneration,
      0,
      PerturbationRange,
      AnnealingParameter,
      []
    )
  of
    [] ->
      [ N_Id | _ ] = N_Ids,
      [ { N_Id, PerturbationRange * math:pi() } ];
    IdPs ->
      IdPs
  end,
  Tot_Neurons = length( ChosenN_IdPs ),
  MutationP = 1 / math:sqrt( Tot_Neurons ),
  choose_randomNIdPs( MutationP, ChosenN_IdPs ).

% all/4 returns a list of tuples composed of all ids (and their spread values) belonging to the NN,
% to the caller.
all( N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter ) ->
  extract_CurGenNIdPs(
    N_Ids,
    AgentGeneration,
    AgentGeneration,
    PerturbationRange,
    AnnealingParameter,
    []
  ).

% all_random/4 first composes a list of tuples from nids and their spreads, and then creates a
% sublist by choosing each element with a probability of 1/math:sqrt(Tot_neurons).
all_random( N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter ) ->
  ChosenN_IdPs = extract_CurGenNIdPs(
    N_Ids,
    AgentGeneration,
    AgentGeneration,
    PerturbationRange,
    AnnealingParameter,
    []
  ),
  Tot_Neurons = length( ChosenN_IdPs ),
  MutationP = 1 / math:sqrt( Tot_Neurons ),
  choose_randomNIdPs( MutationP, ChosenN_IdPs ).

