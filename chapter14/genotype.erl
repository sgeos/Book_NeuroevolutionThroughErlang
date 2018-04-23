%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module( genotype ).
%-compile( export_all ).
-export(
  [
    sync/0,
    construct_Agent/3,
    construct_Cortex/4,
    construct_InitialNeuroLayer/7,
    construct_InitialNeurons/6,
    construct_Neuron/6,
    create_InputIdPs/2,
    create_NeuralWeights/2,
    generate_NeuronAF/1,
    generate_NeuronPF/1,
    generate_NeuronAggrF/1,
    calculate_ROIds/3,
    generate_ids/2,
    generate_UniqueId/0,
    random_element/1,
    update_fingerprint/1,
    generalize_EvoHist/2,
    update_NNTopologySummary/1,
    get_NodeSummary/1,
    get_NodeSummary/5,
    read/1,
    dirty_read/1,
    write/1,
    delete/1,
    print/1,
    delete_Agent/1,
    delete_Agent/2,
    clone_Agent/1,
    clone_Agent/2,
    map_ids/3,
    clone_sensors/2,
    clone_actuators/2,
    clone_neurons/2,
    map_EvoHist/2,
    map_EvoHist/3,
    speciate/1,
    test/0,
    create_test/0
  ]
).
-include( "records.hrl" ).

sync() ->
  make:all( [ load ] ).

% !!! Using cryptographically strong seed instead of erlang:now() and random:seed().
% !!! reference: (see How to generate cryptographically strong seeds with the new rand library)
% https://hashrocket.com/blog/posts/the-adventures-of-generating-random-numbers-in-erlang-and-elixir
%
% The population monitor should have all the information with regards to the morphologies and specie
% constraint under which the agent's genotype should be created. Thus the construct_Agent/3 is run
% with the Specie_Id to which this NN based system will belong, the Agent_Id that this NN based
% intelligent agent will have, and the SpecCon (specie constraint) that will define the list of
% activation functions and other parameters from which the seed agent can choose its parameters.
% First the generation is set to 0, since the agent is just created, then the construct_Cortex/3 is
% ran, which creates the NN and returns its Cx_Id. Once the NN is created and the the cortex's id is
% returned, we can fill out the information needed by the agent record, and write it to the mnesia
% database
construct_Agent( Specie_Id, Agent_Id, SpecCon ) ->
  << I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer >> =
    crypto:strong_rand_bytes( 12 ),
  rand:seed( exsplus, { I1, I2, I3 } ),
  Generation = 0,
  Encoding_Type = random_element( SpecCon#constraint.agent_encoding_types ),
  { Cx_Id, Pattern } = construct_Cortex( Agent_Id, Generation, SpecCon, Encoding_Type ),
  Agent = #agent{
    id = Agent_Id,
    encoding_type = Encoding_Type,
    cx_id = Cx_Id,
    specie_id = Specie_Id,
    constraint = SpecCon,
    generation = Generation,
    pattern = Pattern,
    tuning_selection_f = random_element( SpecCon#constraint.tuning_selection_fs ),
    annealing_parameter = random_element( SpecCon#constraint.annealing_parameters ),
    tuning_duration_f = SpecCon#constraint.tuning_duration_f,
    perturbation_range = random_element( SpecCon#constraint.perturbation_ranges ),
    mutation_operators = SpecCon#constraint.mutation_operators,
    tot_topological_mutations_f = random_element( SpecCon#constraint.tot_topological_mutations_fs ),
    evo_hist = []
  },
  write( Agent ),
  update_fingerprint( Agent_Id ).

% construct_Cortex/3 generates a new Cx_Id, extracts the morphology from the Constraint record passed
% to it in SpecCon, and then extracts the initial sensors and actuators for that morphology. After
% the sensors and actuators are extracted, the function calls construct_InitialNeuroLayer/7, which
% creates a single layer of neurons connected to the specified sensors and actuators, and returns the
% ids of the created neurons. Finally, the sensors and actuator ids are extracted from the sensors
% and actuators, and the cortex record is composed and stored to the database.
construct_Cortex( Agent_Id, Generation, SpecCon, Encoding_Type ) ->
  Cx_Id = { { origin, generate_UniqueId() }, cortex },
  Morphology = SpecCon#constraint.morphology,
  case Encoding_Type of
    neural ->
      Sensors = [
        S#sensor{
          id = { { -1, generate_UniqueId() }, sensor },
          cx_id = Cx_Id,
          generation = Generation
        }
        || S <- morphology:get_InitSensors( Morphology )
      ],
      Actuators = [
        A#actuator{
          id = { { +1, generate_UniqueId() }, actuator },
          cx_id = Cx_Id,
          generation = Generation
        }
        || A <- morphology:get_InitActuators( Morphology )
      ],
      N_Ids = construct_InitialNeuroLayer( Cx_Id, Generation, SpecCon, Sensors, Actuators, [], [] ),
      S_Ids = [ S#sensor.id || S <- Sensors ],
      A_Ids = [ A#actuator.id || A <- Actuators ],
      Cortex = #cortex{
        id = Cx_Id,
        agent_id = Agent_Id,
        neuron_ids = N_Ids,
        sensor_ids = S_Ids,
        actuator_ids = A_Ids
      }
  end,
  write( Cortex ),
  { Cx_Id, [ { 0, N_Ids } ] }.

% construct_InitialNeuroLayer/7 creates a set of neurons for each Actuator in the actuator list. The
% neurons are initialized in the construct_InitialNeurons/6, where they are connected to the
% actuator, and from a random subset of the sensors passed to the function. The
% construct_InitialNeurons/6 function returns the updated sensors, some of which have now an updated
% set of fanout_ids which includes the new neuron ids they were connected to. The actuator's
% fanin_ids are then updated to include the neuron ids that were connected to it. Once all the
% actuators have been connected to, the sensors and the actuators are written to the database, and
% the set of neuron ids created within the function is returned to the caller.
construct_InitialNeuroLayer( Cx_Id, Generation, SpecCon, Sensors, [ A | Actuators ], AAcc, NIdAcc) ->
  N_Ids = [ { { 0, Unique_Id }, neuron } || Unique_Id <- generate_ids( A#actuator.vl, [] ) ],
  U_Sensors = construct_InitialNeurons( Cx_Id, Generation, SpecCon, N_Ids, Sensors, A ),
  U_A = A#actuator{ fanin_ids=N_Ids },
  construct_InitialNeuroLayer(
    Cx_Id,
    Generation,
    SpecCon,
    U_Sensors,
    Actuators,
    [ U_A | AAcc ],
    lists:append( N_Ids, NIdAcc )
  );
construct_InitialNeuroLayer( _Cx_Id, _Generation, _SpecCon, Sensors, [], AAcc, NIdAcc ) ->
  [ write(S) || S <- Sensors ],
  [ write(A) || A <- AAcc ],
  NIdAcc.

% construct_InitialNeurons/6 accepts the list of sensors and a single actuator, connects each neuron
% to the actuator, and randomly chooses whether to connect it from all the sensors or a subset of the
% given sensors. Once all the neurons have been connected to the actuator and from the sensors, the
% updated sensors, whose fanout_ids have been updated with the ids of the neurons, are returned to
% the caller.
construct_InitialNeurons( Cx_Id, Generation, SpecCon, [ N_Id | N_Ids ], Sensors, Actuator ) ->
  case 0.5 =< rand:uniform() of
    true ->
      S = lists:nth( rand:uniform( length(Sensors) ), Sensors ),
      U_Sensors = lists:keyreplace(
        S#sensor.id,
        2,
        Sensors,
        S#sensor{ fanout_ids=[ N_Id | S#sensor.fanout_ids ] }
      ),
      Input_Specs = [ { S#sensor.id, S#sensor.vl } ];
    false ->
      U_Sensors = [ S#sensor{ fanout_ids=[ N_Id | S#sensor.fanout_ids ] } || S <- Sensors ],
      Input_Specs = [ { S#sensor.id, S#sensor.vl } || S <- Sensors ]
  end,
  construct_Neuron( Cx_Id, Generation, SpecCon, N_Id, Input_Specs, [ Actuator#actuator.id ] ),
  construct_InitialNeurons( Cx_Id, Generation, SpecCon, N_Ids, U_Sensors, Actuator );
construct_InitialNeurons( _Cx_Id, _Generation, _SpecCon, [], Sensors, _Actuator ) ->
  Sensors.

% Each neuron record is composed by the construct_Neuron/6 function. The construct_Neuron/6 creates
% the Input list from the tuples [ { Id, Weights } ... ] using the vector lengths specified in the
% Input_Specs list. The create_InputIdPs/3 function uses create_NeuralWeights/2 to generate the
% random weights in the range of -0.5 to 0.5, adding the bias to the end of the list. The activation
% function that the neuron uses is chosen randomly from the neural_afs list within the constraint
% record passed to the construct_Neuron/6 function. construct_Neuron uses calculate_ROIds/3 to
% extract the list of recursive connection ids from the Output_Ids passed to it. Once the neuron
% record is filled in, it is saved to the database.
%
%-record( neuron, { id, generation, cx_id, af, input_idps, output_ids, ro_ids } ).
construct_Neuron( Cx_Id, Generation, SpecCon, N_Id, Input_Specs, Output_Ids ) ->
  Input_IdPs = create_InputIdPs( Input_Specs, [] ),
  Neuron = #neuron{
    id = N_Id,
    cx_id = Cx_Id,
    generation = Generation,
    af = generate_NeuronAF( SpecCon#constraint.neural_afs ),
    pf = generate_NeuronPF( SpecCon#constraint.neural_pfs ),
    aggr_f = generate_NeuronAggrF( SpecCon#constraint.neural_aggr_fs ),
    input_idps = Input_IdPs,
    output_ids = Output_Ids,
    ro_ids = calculate_ROIds( N_Id, Output_Ids, [] )
  },
  write( Neuron ).

create_InputIdPs( [ { Input_Id, Input_VL } | Input_IdPs ], Acc ) ->
  Weights = create_NeuralWeights( Input_VL, [] ),
  create_InputIdPs( Input_IdPs, [ { Input_Id, Weights } | Acc ] );
create_InputIdPs( [], Acc ) ->
  Acc.

create_NeuralWeights( 0, Acc ) ->
  Acc;
create_NeuralWeights( Index, Acc ) ->
  W = rand:uniform() - 0.5,
  create_NeuralWeights( Index - 1, [ W | Acc ] ).

% The generate_NeuronAF/1 accepts a list of activation function tags, and returns a randomly chosen
% one. If an empty list was passed as the parameter, the function returns the standard tanh tag.
generate_NeuronAF( [] ) ->
  tanh;
generate_NeuronAF( ActivationFunctions ) ->
  lists:nth( rand:uniform( length( ActivationFunctions ) ), ActivationFunctions ).

% The generate_NeuronPF/1 accepts a list of plasticity function tags, and returns a randomly chosen
% one. If an empty list was passed as the parameter, the function returns the default none tag.
generate_NeuronPF( [] ) ->
  none;
generate_NeuronPF( PlasticityFunctions ) ->
  lists:nth( rand:uniform( length( PlasticityFunctions ) ), PlasticityFunctions ).

% The generate_NeuronAggrF/1 accepts a list of aggregation function tags, and returns a randomly
% chosen one. If an empty list was passed as the parameter, the function returns the default
% dot_product tag.
generate_NeuronAggrF( [] ) ->
  none;
generate_NeuronAggrF( AggregationFunctions ) ->
  lists:nth( rand:uniform( length( AggregationFunctions ) ), AggregationFunctions ).

% The function calculate_ROIds/3 accepts as input the Self_Id of the neuron, and the Output_Ids of
% the elements the neuron connects to. Since each element specifies its type and, in the case of
% neurons, specifies the layer index it belongs to, the function checks if the Output_Id's layer
% index is lower than the Self_Id's layer index, if it is, the output connection is recursive and the
% Output_Id is added to the recursive output list. Once the recursive connection ids have been
% extracted from the Output_Ids, the extracted id list is returned to the caller.
calculate_ROIds( Self_Id, [ Output_Id | Ids ], Acc ) ->
  case Output_Id of
    { _, actuator } ->
      calculate_ROIds( Self_Id, Ids, Acc );
    Output_Id ->
      { { TLI, _ }, _NodeType } = Self_Id,
      { { LI, _ }, _ } = Output_Id,
      case LI =< TLI of
        true ->
          calculate_ROIds( Self_Id, Ids, [ Output_Id | Acc ] );
        false ->
          calculate_ROIds( Self_Id, Ids, Acc )
      end
  end;
calculate_ROIds( _Self_Id, [], Acc ) ->
  lists:reverse( Acc ).

generate_ids( 0, Acc ) ->
  Acc;
generate_ids( Index, Acc ) ->
  Id = generate_UniqueId(),
  generate_ids( Index - 1, [ Id | Acc ] ).

% !!! Consider using something like erlang:unique_integer([monotonic]) instead of erlang:now().
% !!! Alternatively, a variation on the following may be good:
% !!!   Time = erlang:monotonic_time(),
% !!!   UMI = erlang:unique_integer( [ monotonic ] ),
% !!!   UniqueId = { Time, UMI }
% !!! reference: http://erlang.org/doc/apps/erts/time_correction.html (How to Work with the New API)
%
% The generate_UniqueId/0 creates a unique Id using current time, the Id is a floating point value.
% The generate_ids/2 function creates a list of unique Ids.
generate_UniqueId() ->
  % Must use erlang:now() instead of erlang:timestamp() because erlang:now() is monotonic.
  % Identifiers require unique values!
  % Reference:
  %   http://erlang.org/pipermail/erlang-questions/2013-April/073153.html
  %{ MegaSeconds, Seconds, MicroSeconds } = erlang:now(), % depreciated
  %{ MegaSeconds, Seconds, MicroSeconds } = erlang:timestamp(), % broken
  %1 / ( MegaSeconds * 1000000 + Seconds + MicroSeconds / 1000000 ).

  %UniqueInteger = erlang:unique_integer( [ positive, monotonic ] ),
  %% 1 bit sign, 11 bit exponent, 52 bit fraction
  %<< UniqueFloatId / float >> = << 0 : 1, 1023 : 11, UniqueInteger : 52 >>, 
  %UniqueFloatId.

  %erlang:unique_integer( [ positive, monotonic ] ) * 1.0.

  1.0 / erlang:unique_integer( [ positive, monotonic ] ).

% The random_element/1 function accepts a list as input, and returns a single, randomly chosen
% element as output.
random_element( List ) ->
  lists:nth( rand:uniform( length( List ) ), List ).

% update_fingerprint/1 calculates the fingerprint of the agent, where the fingerprint is just a tuple
% of the various general features of the NN based system, a list of features that play some role in
% distinguishing its genotype's general properties from those of other NN systems. The fingerprint
% here is composed of the generalized pattern (pattern minus the unique ids), generalized
% evolutionary history (evolutionary history minus the unique ids of the elements), a generalized
% sensor set, and a generalized actuator set.
update_fingerprint( Agent_Id ) ->
  A = read( { agent, Agent_Id } ),
  Cx = read( { cortex, A#agent.cx_id } ),
  GeneralizedSensors = [
    ( read( { sensor, S_Id } ) )#sensor{ id=undefined, cx_id=undefined, fanout_ids=[] }
      || S_Id <- Cx#cortex.sensor_ids
  ],
  GeneralizedActuators = [
    ( read( { actuator, A_Id } ) )#actuator{ id=undefined, cx_id=undefined, fanin_ids=[] }
      || A_Id <- Cx#cortex.actuator_ids
  ],
  GeneralizedPattern = [
    { LayerIndex, length( LNIds ) }
      || { LayerIndex, LNIds } <- A#agent.pattern
  ],
  GeneralizedEvoHist = generalize_EvoHist( A#agent.evo_hist, [] ),
  N_Ids = Cx#cortex.neuron_ids,
  { Tot_Neuron_ILs, Tot_Neuron_OLs, Tot_Neuron_ROs, AF_Distribution } = get_NodeSummary( N_Ids ),
  Type = A#agent.encoding_type,


  TopologySummary = #topology_summary{
    type = Type,
    tot_neurons = length( N_Ids ),
    tot_n_ils = Tot_Neuron_ILs,
    tot_n_ols = Tot_Neuron_OLs,
    tot_n_ros = Tot_Neuron_ROs,
    af_distribution = AF_Distribution
  },
  Fingerprint = {
    GeneralizedPattern,
    GeneralizedEvoHist,
    GeneralizedSensors,
    GeneralizedActuators,
    TopologySummary
  },
  U_A = A#agent{ fingerprint=Fingerprint },
  write( U_A ).

% generalize_EvoHist/2 generalizes the evolutionary history tuples by removing the unique element
% ids. Two neurons which are using exactly the same activation function, located exactly in the same
% layer, and using exactly the same weights will still have different unique ids, thus these ids must
% be removed to produce a more general set of tuples. There are 3 types of tuples in evo_hist list,
% with 3, 2 and 1 element ids. Once the evolutionary history list is generalized, it is returned to
% the caller.
generalize_EvoHist(
  [
    { MO, { { ALI, _AUId }, AType }, { { BLI, _BUId }, BType }, { { CLI, _CUId }, CType } }
    | EvoHist
  ],
  Acc
) ->
  generalize_EvoHist( EvoHist, [ { MO, { ALI, AType }, { BLI, BType }, { CLI, CType } } | Acc ] );
generalize_EvoHist(
  [ { MO, { { ALI, _AUId }, AType }, { { BLI, _BUId }, BType } } | EvoHist ], Acc
) ->
  generalize_EvoHist( EvoHist, [ { MO, { ALI, AType }, { BLI, BType } } | Acc ] );
generalize_EvoHist( [ { MO, { { ALI, _AUId }, AType } } | EvoHist ], Acc ) ->
  generalize_EvoHist( EvoHist, [ { MO, { ALI, AType } } | Acc ] );
generalize_EvoHist( [ { MO, _EId } | EvoHist ], Acc ) ->
  generalize_EvoHist( EvoHist, [ { MO } | Acc ] );
generalize_EvoHist( [], Acc ) ->
  lists:reverse( Acc ).

update_NNTopologySummary( Agent_Id ) ->
  A = mnesia:read( { agent, Agent_Id } ),
  Cx_Id = A#agent.cx_id,
  Cx = mnesia:read( { cortex, Cx_Id } ),
  N_Ids = Cx#cortex.neuron_ids,
  { Tot_Neuron_ILs, Tot_Neuron_OLs, Tot_Neuron_ROs, AF_Distribution } = get_NodeSummary( N_Ids ),
  Type = A#agent.encoding_type,
  Topology_Summary = #topology_summary{
    type = Type,
    tot_neurons = length( N_Ids ),
    tot_n_ils = Tot_Neuron_ILs,
    tot_n_ols = Tot_Neuron_OLs,
    tot_n_ros = Tot_Neuron_ROs,
    af_distribution = AF_Distribution
  },
  Topology_Summary.

get_NodeSummary( N_Ids ) ->
  get_NodeSummary( N_Ids, 0, 0, 0, { 0, 0, 0, 0, 0, 0, 0, 0, 0 } ).

get_NodeSummary( [ N_Id | N_Ids ], ILAcc, OLAcc, ROAcc, FunctionDistribution ) ->
  N = genotype:read( { neuron, N_Id } ),
  IL_Count = length( N#neuron.input_idps ),
  OL_Count = length( N#neuron.output_ids ),
  RO_Count = length( N#neuron.ro_ids ),
  AF = N#neuron.af,
  { TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin } =
    FunctionDistribution,
  U_FunctionDistribution = case AF of
    tanh ->
      { TotTanh + 1, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin };
    sin ->
      { TotTanh, TotSin + 1, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin };
    cos ->
      { TotTanh, TotSin, TotCos + 1, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin };
    gaussian->
      { TotTanh, TotSin, TotCos, TotGaussian + 1, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin };
    absolute->
      { TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute + 1, TotSgn, TotLog, TotSqrt, TotLin };
    sgn ->
      { TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn + 1, TotLog, TotSqrt, TotLin };
    log ->
      { TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog + 1, TotSqrt, TotLin };
    sqrt ->
      { TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt + 1, TotLin };
    linear ->
      { TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin + 1 };
    Other ->
      io:format( "Unknown AF, please update AF_Distribution tuple with: ~p~n.", [ Other ] )
  end,
  get_NodeSummary(
    N_Ids,
    IL_Count + ILAcc,
    OL_Count + OLAcc,
    RO_Count + ROAcc,
    U_FunctionDistribution
  );
get_NodeSummary( [], ILAcc, OLAcc, ROAcc, FunctionDistribution ) ->
  { ILAcc, OLAcc, ROAcc, FunctionDistribution }.

% read/1 accepts the tuple composed of a table name and a key: { TableName, Key }, which it then uses
% to read from the mnesia database and return the record to the caller. write/1 accepts a record and
% writes it to the database. delete/1 accepts a the tuple { TableName, Key }, and deletes the
% associated record from the table.
read( TnK ) ->
  case mnesia:read( TnK ) of
    [] ->
      undefined;
    [ R ] ->
      R
  end.

dirty_read( TnK ) ->
  case mnesia:dirty_read( TnK ) of
    [] ->
      undefined;
    [ R ] ->
      R
  end.

write( R ) ->
  F = fun() ->
    mnesia:write( R )
  end,
  mnesia:transaction( F ).

delete( TnK ) ->
  F = fun() ->
    mnesia:delete( TnK )
  end,
  mnesia:transaction( F ).

% print/1 accepts an agent's id, and prints out the complete genotype of that agent.
print( Agent_Id ) ->
  F = fun() ->
    A = read( { agent, Agent_Id } ),
    Cx = read( { cortex, A#agent.cx_id } ),
    io:format( "~p~n", [ A ] ),
    io:format( "~p~n", [ Cx ] ),
    [ io:format( "~p~n", [ read( { sensor, Id } ) ] ) || Id <- Cx#cortex.sensor_ids ],
    [ io:format( "~p~n", [ read( { neuron, Id } ) ] ) || Id <- Cx#cortex.neuron_ids ],
    [ io:format( "~p~n", [ read( { actuator, Id } ) ] ) || Id <- Cx#cortex.actuator_ids ]
  end,
  mnesia:transaction( F ).

% delete_Agent/1 accepts the id of an agent, and then delets that agent's genotype. This function
% assumes that the id of the agent will be removed from the specie's agent_ids list, and any other
% clean up procedures, by the calling function.
delete_Agent( Agent_Id ) ->
  A = read( { agent, Agent_Id } ),
  Cx = read( { cortex, A#agent.cx_id } ),
  [ delete( { neuron, Id } ) || Id <- Cx#cortex.neuron_ids ],
  [ delete( { sensor, Id } ) || Id <- Cx#cortex.sensor_ids ],
  [ delete( { actuator, Id } ) || Id <- Cx#cortex.actuator_ids ],
  delete( { cortex, A#agent.cx_id } ),
  delete( { agent, Agent_Id } ).

% delete_Agent/2 accepts the id of an agent, and then delets that agent's genotype, but ensures that
% the specie to which the agent belongs, has its agent_ids element updated. Unlinke delete_Agent/1,
% this function updates the specie record.
delete_Agent( Agent_Id, safe ) ->
  F = fun() ->
    A = genotype:read( { agent, Agent_Id } ),
    S = genotype:read( { specie, A#agent.specie_id } ),
    Agent_Ids = S#specie.agent_ids,
    %DeadPool = S#specie.dead_pool,
    write( S#specie{ agent_ids=lists:delete( Agent_Id, Agent_Ids ) } ),
    delete_Agent( Agent_Id )
  end,
  _Result = mnesia:transaction( F ),
  %io:format( "delete_Agent( Agent_Id, safe ): ~p  Result: ~p~n", [ Agent_Id, Result ] ),
  ok.

% clone_Agent/2 accepts Agent_Id, and CloneAgent_Id, and then clones the agent, giving the clone
% CloneAgent_Id. The function first creates an ETS table to which it writes the ids of all the
% elements of the genotype, and their corresponding clone ids. Once all ids and clone ids have been
% generated, the function then begins to clone the actual elements. clone_Agent/2 first clones the
% neurons using clone_neurons/2, then the sensors using clone_sensonrs/2, and finally the actuators
% using clone_actuators. Once these elements are cloned, the function writes to database the clone
% versions of the cortex and the agent records, by writing to databse the original records with
% updated ids.
clone_Agent( Agent_Id ) ->
  CloneAgent_Id = { generate_UniqueId(), agent },
  clone_Agent( Agent_Id, CloneAgent_Id ).
clone_Agent( Agent_Id, CloneAgent_Id ) ->
  F = fun() ->
    A = read( { agent, Agent_Id } ),
    Cx = read( { cortex, A#agent.cx_id } ),
    IdsNCloneIds = ets:new( idsNcloneids, [ set, private ] ),
    ets:insert( IdsNCloneIds, { bias, bias } ),
    ets:insert( IdsNCloneIds, { Agent_Id, CloneAgent_Id } ),
    [ CloneCx_Id ] = map_ids( IdsNCloneIds, [ A#agent.cx_id ], [] ),
    CloneN_Ids = map_ids( IdsNCloneIds, Cx#cortex.neuron_ids, [] ),
    CloneS_Ids = map_ids( IdsNCloneIds, Cx#cortex.sensor_ids, [] ),
    CloneA_Ids = map_ids( IdsNCloneIds, Cx#cortex.actuator_ids, [] ),
    clone_neurons( IdsNCloneIds, Cx#cortex.neuron_ids ),
    clone_sensors( IdsNCloneIds, Cx#cortex.sensor_ids ),
    clone_actuators( IdsNCloneIds, Cx#cortex.actuator_ids ),
    U_EvoHist = map_EvoHist( IdsNCloneIds, A#agent.evo_hist ),
    U_Cx = Cx#cortex{
      id = CloneCx_Id,
      agent_id = CloneAgent_Id,
      sensor_ids = CloneS_Ids,
      actuator_ids = CloneA_Ids,
      neuron_ids = CloneN_Ids
    },
    U_A = A#agent{
      id = CloneAgent_Id,
      cx_id = CloneCx_Id,
      evo_hist = U_EvoHist
    },
    write( U_Cx ),
    write( U_A ),
    ets:delete( IdsNCloneIds )
  end,
  mnesia:transaction( F ),
  CloneAgent_Id.

% map_ids/3 accepts the name of the ets table, and a list of ids. It then goes through every id and
% creates a clone version of the id by generating a new unique id. The function is able to generate
% new id structures for neuron, cortex, sensor, and actuator id types.
map_ids( TableName, [ Id | Ids ], Acc ) ->
  CloneId = case Id of
    { { LayerIndex, _NumId }, Type } -> % maps neuron and cortex ids.
      { { LayerIndex, generate_UniqueId() }, Type };
    { _NumId, Type } -> % maps sensor and actuator ids.
      { generate_UniqueId(), Type }
  end,
  ets:insert( TableName, { Id, CloneId } ),
  map_ids( TableName, Ids, [ CloneId | Acc ] );
map_ids( _TableName, [], Acc ) ->
  Acc.

% clone_sensors/2 accepts as input the name of the ets table and the list of sensor ids. It then goes
% through every sensor id, reads the sensor from the database, and updates all the ids (id, cx_id,
% and fanout_ids) from their original values, to their clone values stored in the ets table. Then the
% new version of the sensor is written to the database.
clone_sensors( TableName, [ S_Id | S_Ids ] ) ->
  S = read( { sensor, S_Id } ),
  CloneS_Id = ets:lookup_element( TableName, S_Id, 2 ),
  CloneCx_Id = ets:lookup_element( TableName, S#sensor.cx_id, 2 ),
  CloneFanout_Ids = [
    ets:lookup_element( TableName, Fanout_Id, 2 )
    || Fanout_Id <- S#sensor.fanout_ids
  ],
  U_S = S#sensor{
    id = CloneS_Id,
    cx_id = CloneCx_Id,
    fanout_ids = CloneFanout_Ids
  },
  write( U_S ),
  clone_sensors( TableName, S_Ids );
clone_sensors( _TableName, [] ) ->
  done.

% clone_actuators/2 accepts as input the name of the ets table and the list of actuator ids. It then
% goes through every actuator id, reads the actuator from the database, and updates all the ids (id,
% cx_id, and fanin_ids) from their original values, to their clone values stored in the ets table.
% Then the new version of the actuator is written to the database.
clone_actuators( TableName, [ A_Id | A_Ids ] ) ->
  A = read( { actuator, A_Id } ),
  CloneA_Id = ets:lookup_element( TableName, A_Id, 2 ),
  CloneCx_Id = ets:lookup_element( TableName, A#actuator.cx_id, 2 ),
  CloneFanin_Ids = [
    ets:lookup_element( TableName, Fanin_Id, 2 )
    || Fanin_Id <- A#actuator.fanin_ids
  ],
  U_A = A#actuator{
    id = CloneA_Id,
    cx_id = CloneCx_Id,
    fanin_ids = CloneFanin_Ids
  },
  write( U_A ),
  clone_actuators( TableName, A_Ids );
clone_actuators( _TableName, [] ) ->
  done.

% clone_neuron/2 accepts as input the name of the ets table and the list of neuron ids. It then goes
% through every neuron id, reads the neuron from the database, and updates all the ids (id, cx_id,
% output_ids, ro_ids) and input_idps from their original values, to their clone values stored in the
% ets table. Once the everything is updated, the new (clone) version of the neuron is written to the
% database.
clone_neurons( TableName, [ N_Id | N_Ids ] ) ->
  N = read( { neuron, N_Id } ),
  CloneN_Id = ets:lookup_element( TableName, N_Id, 2 ),
  CloneCx_Id = ets:lookup_element( TableName, N#neuron.cx_id, 2 ),
  CloneInput_IdPs = [
    { ets:lookup_element( TableName, I_Id, 2 ), Weights }
    || { I_Id, Weights } <- N#neuron.input_idps
  ],
  CloneOutput_Ids = [
    ets:lookup_element( TableName, O_Id, 2 )
    || O_Id <- N#neuron.output_ids
  ],
  CloneRO_Ids = [
    ets:lookup_element( TableName, RO_Id, 2 )
    || RO_Id <- N#neuron.ro_ids
  ],
  U_N = N#neuron{
    id = CloneN_Id,
    cx_id = CloneCx_Id,
    input_idps = CloneInput_IdPs,
    output_ids = CloneOutput_Ids,
    ro_ids = CloneRO_Ids
  },
  write( U_N ),
  clone_neurons( TableName, N_Ids );
clone_neurons( _TableName, [] ) ->
  done.

% map_EvoHist/2 is a wrapper for map_EvoHist/3, which in turn accepts the evo_hist list containing
% the mutation operator tuples that have been appplied to the NN system. The function is used when a
% clone of a NN system is created. The function updates the original Ids of the elements the mutation
% operators have been applied to, to the clone's Ids, so that the updated evo_hist can reflect the
% clone's topology, as if the mutation operators have been applied to it, and that it is not a clone.
% Once all the tuples in the evo_hist have been updated with the clone element ids, the list is
% reverted to its proper order, and the updated list is returned to the caller.
map_EvoHist( TableName, EvoHist ) ->
  map_EvoHist( TableName, EvoHist, [] ).

map_EvoHist( TableName, [ { MO, E1Id, E2Id, E3Id } | EvoHist ], Acc ) ->
  Clone_E1Id = ets:lookup_element( TableName, E1Id, 2 ),
  Clone_E2Id = ets:lookup_element( TableName, E2Id, 2 ),
  Clone_E3Id = ets:lookup_element( TableName, E3Id, 2 ),
  map_EvoHist( TableName, EvoHist, [ { MO, Clone_E1Id, Clone_E2Id, Clone_E3Id } | Acc ] );
map_EvoHist( TableName, [ { MO, E1Id, E2Id } | EvoHist ], Acc ) ->
  Clone_E1Id = ets:lookup_element( TableName, E1Id, 2 ),
  Clone_E2Id = ets:lookup_element( TableName, E2Id, 2 ),
  map_EvoHist( TableName, EvoHist, [ { MO, Clone_E1Id, Clone_E2Id } | Acc ] );
map_EvoHist( TableName, [ { MO, E1Id } | EvoHist ], Acc ) ->
  Clone_E1Id = ets:lookup_element( TableName, E1Id, 2 ),
  map_EvoHist( TableName, EvoHist, [ { MO, Clone_E1Id } | Acc ] );
map_EvoHist( _TableName, [], Acc ) ->
  lists:reverse( Acc ).

% The function speciate/1 reads a newly created agent record, calculates that agent's fingerprint,
% and then based on that fingerprint either inserts it into an already existing specie, or creates a
% new specie of which the agent is the first of a kind. The function first creates the fingerprint of
% the agent using the genotype:create_fingerprint/1 function. Then the function checks whether this
% is a test agent, in which case it is only used for testing, and does not belong to any specie or
% population. If the agent is not a test agent, then the specie and population to which its parent
% belonged is retreived from the database (the specie and population ids are conserved in the
% offspring during mutation, so the agent already holds his parent's specie and population ids).
% Afterwards, a specie is found which has the same fingerprint as the agent. If there is no such
% specie, then a new specie is created, a specie that belongs to the same population as the agent,
% and has the same constriants and fingerprint as the agent fathering the specie. Then the agent's id
% is entered into the specie, and the updated specie and agent are written to database. If on the
% other hand a specie already exists with the same fingerprint as the agent, then the agent's id is
% added to the existing specie, and the updated specie and agent are written to database.
speciate( Agent_Id ) ->
  update_fingerprint( Agent_Id ),
  A = read( { agent, Agent_Id } ),
  case A#agent.id of
    test -> % Test agent belongs to no specie and no population.
      write( A#agent{ fitness = undefined } );
    _ ->
      Parent_S = read( { specie, A#agent.specie_id } ),
      P = read( { population, Parent_S#specie.population_id } ),
      case
        [
          Id || Id <- P#population.specie_ids,
            ( read( { specie, Id } ) )#specie.fingerprint == A#agent.fingerprint
        ]
      of
        [] ->
          Specie_Id = population_monitor:create_specie(
            P#population.id,
            A#agent.constraint,
            A#agent.fingerprint
          ),
          S = read( { specie, Specie_Id } ),
          U_A = A#agent{ specie_id = Specie_Id, fitness = undefined },
          U_S = S#specie{ agent_ids = [ Agent_Id ] },
          write( U_A ),
          write( U_S );
        [ Specie_Id ] ->
          S = read( { specie, Specie_Id } ),
          U_A = A#agent{ specie_id = Specie_Id, fitness = undefined },
          U_S = S#specie{ agent_ids = [ Agent_Id | S#specie.agent_ids ] },
          write( U_A ),
          write( U_S )
      end
  end.

% test/0 performs a test of the standard functions of the genotype module, by first creating a new
% agent, then cloning that agent, then printing the genotype of the original agent and its clone, and
% then finally deleting both of the agents.
test() ->
  Specie_Id = test,
  Agent_Id = test,
  CloneAgent_Id = test_clone,
  SpecCon = #constraint{},
  F = fun() ->
    construct_Agent( Specie_Id, Agent_Id, SpecCon ),
    clone_Agent( Specie_Id, CloneAgent_Id ),
    print( Agent_Id ),
    print( CloneAgent_Id ),
    delete_Agent( Agent_Id ),
    delete_Agent( CloneAgent_Id )
  end,
  mnesia:transaction( F ).

% create_test/0 creates a simple NN based agent using the default constraint record. The function
% first checks if an agent with the id 'test' already exists, if it does, the function deletes that
% agent and creates a new one. Otherwise, the function just creates a brand new agent with the 'test'
% id.
create_test() ->
  Specie_Id = test,
  Agent_Id = test,
  SpecCon = #constraint{ morphology = discrete_tmaze },
  F = fun() ->
    case genotype:read( { agent, test } ) of
      undefined ->
        construct_Agent( Specie_Id, Agent_Id, SpecCon ),
        print( Agent_Id );
      _ ->
        delete_Agent( Agent_Id ),
        construct_Agent( Specie_Id, Agent_Id, SpecCon ),
        print( Agent_Id )
    end
  end,
  mnesia:transaction( F ).

