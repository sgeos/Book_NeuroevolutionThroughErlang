%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module( fitness_postprocessor ).
%-compile( export_all ).
-export(
  [
    none/1,
    size_proportional/1
  ]
).
-include( "records.hrl" ).
-define( EFF, 0.1 ). % Efficiency.

% !!! Function comment blocks are in the book but not in the official repository.

% Sort results without postprocessing.
none( Agent_Summaries ) ->
  lists:reverse( lists:sort( Agent_Summaries ) ).

% Decrease fitness scores in a way that is proportional to the NN size.
size_proportional( Agent_Summaries ) ->
  SDX = lists:reverse(
    lists:sort(
      [
        { Fitness / math:pow( TotN, ?EFF ), { Fitness, TotN, Agent_Id } }
        || { Fitness, TotN, Agent_Id } <- Agent_Summaries
      ]
    )
  ),
  ProperlySorted_AgentSummaries = [ Val || { _, Val } <- SDX ],
  ProperlySorted_AgentSummaries.

