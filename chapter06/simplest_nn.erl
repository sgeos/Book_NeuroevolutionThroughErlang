% 0       1         2         3         4         5         6         7         8         9         A
% 234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

-module(simplest_nn).
%-compile(export_all).
-export(
  [
    create/0,
    neuron/3,
    sensor/1,
    actuator/1,
    %pts/1,
    cortex/3,
    dot/3,
    randomlist/1,
    randomlist/2
  ]
).

% The create function first generates 3 weights, with the 3rd weight being the Bias. The Neuron is
% spawned first, and is then sent the PIds of the Sensor and Actuator that it's connected with. Then
% the Cortex element is registered and provided with the PIds of all the elements in the NN system.
create() ->
  %Weights = [rand:uniform()-0.5, rand:uniform()-0.5, rand:uniform()-0.5],
  Weights = randomlist(3, -0.5),
  Neuron_PId = spawn(?MODULE, neuron, [Weights, undefined, undefined]),
  Sensor_PId = spawn(?MODULE, sensor, [Neuron_PId]),
  Actuator_PId = spawn(?MODULE, actuator, [Neuron_PId]),
  Neuron_PId ! {init, Sensor_PId, Actuator_PId},
  register(cortex, spawn(?MODULE, cortex, [Sensor_PId, Neuron_PId, Actuator_PId])).

% After the neuron finishes setting its SPId and APId to that of the Sensor and Actuator
% respectively, it starts waiting for the incoming signals. The neuron expects a vector of length 2
% as input, and as soon as the input arrives, the neuron processes the signal and passes the output
% vector to the outgoing APId.
neuron(Weights, Sensor_PId, Actuator_PId) ->
  receive
    {Sensor_PId, forward, Input} ->
      io:format("****Thinking****~n Input: ~p~n with Weights: ~p~n", [Input, Weights]),
      Dot_Product = dot(Input, Weights, 0),
      Output = [math:tanh(Dot_Product)],
      Actuator_PId ! {self(), forward, Output},
      neuron(Weights, Sensor_PId, Actuator_PId);
    {init, New_Sensor_PId, New_Actuator_PId} ->
      neuron(Weights, New_Sensor_PId, New_Actuator_PId);
    terminate ->
      ok
  end.

% The Sensor function waits to be triggered by the Cortex element, and then produces a random vector
% of length 2, which it passes to the connected neuron. In a proper system the sensory signal would
% not be a random vector but instead would be produced by a function associated with the sensor, a
% function that for example reads and vector-encodes a signal coming from a GPS attached to a robot.
sensor(Neuron_PId) ->
  receive
    sync ->
      %Sensory_Signal = [rand:uniform(), rand:uniform()],
      Sensory_Signal = randomlist(2),
      io:format("****Sensing****~n Signal from the enfivronment ~p~n", [Sensory_Signal]),
      Neuron_PId ! {self(), forward, Sensory_Signal},
      sensor(Neuron_PId);
    terminate ->
      ok
  end.

% The Actuator function waits for a control signal coming from a Neuron. As soon as the signal
% arrives, the actuator executes its function, pts/1, which prints the value to the screen. 
actuator(Neuron_PId) ->
  receive
    {Neuron_PId, forward, Control_Signal} ->
      pts(Control_Signal),
      actuator(Neuron_PId);
    terminate ->
      ok
  end.

pts(Control_Signal) ->
  io:format("****Acting****~n Using: ~p to act on environment.~n", [Control_Signal]).

% The Cortex function triggers the sensor to action when commanded by the user. This process also has
% all the PIds of the elements in the NN system, so that it can terminate the whole system when
% requested.
cortex(Sensor_PId, Neuron_PId, Actuator_PId) ->
  receive
    sense_think_act ->
      Sensor_PId ! sync,
      cortex(Sensor_PId, Neuron_PId, Actuator_PId);
    terminate ->
      Sensor_PId ! terminate,
      Neuron_PId ! terminate,
      Actuator_PId ! terminate,
      ok
  end.

% The dot function takes a dot product of two vectors, it can operate on a weight vector with and
% without a bias. When there is no bias in the weight list, both the Input vector and the Weight
% vector are of the same length. When Bias is present, then when the Input list empties out, the
% Weights list still has 1 value remaining, its Bias.
dot([I|Input],[W|Weights],Acc) ->
  dot(Input,Weights,I*W + Acc);
dot([],[],Acc) ->
  Acc;
dot([],[Bias],Acc) ->
  Acc + Bias.

% Create a list of random values with an optional offset.
randomlist(Length) ->
  randomlist(Length, 0).
randomlist(Length, Offset) ->
  lists:map(
    fun(ItemOffset) ->
      rand:uniform() + ItemOffset
    end,
    lists:duplicate(Length, Offset)
  ).

