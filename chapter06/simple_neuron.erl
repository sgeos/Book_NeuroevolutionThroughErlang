%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module(simple_neuron).
%-compile(export_all).
-export([create/0, loop/1, dot/3, sense/1]).

% The create function spawns a single neuron, where the weights and the bias are generated randomly
% to be between -0.5 and 0.5.
create() ->
  Weights = [rand:uniform()-0.5, rand:uniform()-0.5, rand:uniform()-0.5],
  register(neuron, spawn(?MODULE, loop, [Weights])).

% The spawned neuron process accepts an input vector, prints it and the weight vector to the screen,
% calculates the output, and then sends the output to the contacting process.  The output is also a
% vector of length one.
loop(Weights) ->
  receive
    {From, Input} ->
      io:format("****Processing**** ~n Input:~p ~n Using Weights:~p ~n", [Input, Weights]),
      Dot_Product = dot(Input, Weights, 0),
      Output = [math:tanh(Dot_Product)],
      From ! {result, Output},
      loop(Weights)
  end.

% The dot product function that we use works on the assumption that the bias is incorporated into the
% weight list as the last value in that list.  After calculating the dot product, the input list will
% empty out while the weight list will still have the single bias value remaining, which we then add
% to the accumulator.
dot([I|Input],[W|Weights],Acc) ->
  dot(Input,Weights,I*W + Acc);
dot([],[Bias],Acc) ->
  Acc + Bias.

% We use the sense function to contact the neuron and send it an input vector.  The sense function
% ensures that the signal we are sending is a vector of length 2.
sense(Signal) ->
  case is_list(Signal) and (2 == length(Signal)) of
    true ->
      neuron ! {self(), Signal},
      receive
        {result, Output} ->
        io:format("Output: ~p ~n", [Output])
      end;
    false ->
      io:format("The Signal must be a list of length 2. ~n")
end.

