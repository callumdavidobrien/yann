-module(yann_neuron).
-import(lists, [zipwith/3, sum/1]).
-export([init/0, neuron/3, activation/3]).

init() ->
   spawn_link(?MODULE, neuron, [0, [], []]).

neuron(Bias, Weights, Inputs) ->
   receive
      {Sender, update, NewInputs} ->
         Sender ! ok,
         neuron(Bias, Weights, NewInputs);

      {Sender, train, BiasChange, WeightChanges} ->
         Sender ! ok,
         NewBias = Bias + BiasChange,
         NewWeights = lists:zipWith(fun(X, Y) -> X + Y end, WeightChanges, Weights),
         neuron(NewBias, NewWeights, Inputs);

      {Sender, get} ->
         Activation = activation(Bias, Weights, Inputs),
         Sender ! {ok, Activation},
         neuron(Bias, Weights, Inputs)
   end.

activation(Bias, Weights, Inputs) ->
   sigmoid(raw_activation(Bias, Weights, Inputs)).
         
raw_activation(Bias, Weights, Inputs) ->
   Activations = lists:zipwith(fun(X, Y) -> X * Y end, Weights, Inputs),
   lists:sum(Activations) + Bias.

sigmoid(X) ->
   1 / (1 + math:exp(-X)).

