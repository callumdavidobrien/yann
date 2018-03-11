-module(yann_neuron).
-import(lists, [zipwith/3, sum/1]).
-export([init/1, neuron/3]).

init(InputSize) ->
   Weights = lists:duplicate(InputSize, 0),
   Inputs = lists:duplicate(InputSize, 0),
   spawn_link(?MODULE, neuron, [0, Weights, Inputs]).

neuron(Bias, Weights, Inputs) ->
   receive
      {Sender, update, NewInputs} ->
         Sender ! ok,
         neuron(Bias, Weights, NewInputs);

      {Sender, train, BiasChange, WeightChanges} ->
         Sender ! ok,
         NewBias = Bias + BiasChange,
         NewWeights = lists:zipwith(fun(X, Y) -> X + Y end, WeightChanges, Weights),
         neuron(NewBias, NewWeights, Inputs);

      {Sender, get} ->
         Activation = activation(Bias, Weights, Inputs),
         Sender ! {ok, Activation},
         neuron(Bias, Weights, Inputs);

   end.

activation(Bias, Weights, Inputs) ->
   sigmoid(raw_activation(Bias, Weights, Inputs)).
         
raw_activation(Bias, Weights, Inputs) ->
   Activations = lists:zipwith(fun(X, Y) -> X * Y end, Weights, Inputs),
   lists:sum(Activations) + Bias.

sigmoid(X) ->
   1 / (1 + math:exp(-X)).

