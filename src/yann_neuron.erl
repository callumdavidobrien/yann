-module(yann_neuron).

activation(Bias, Weights, Inputs) ->
   Activations = zipwith3(fun(X, Y) -> X * Y end, Weights, Inputs),
   sigmoid(sum(Activations) + Bias).

sigmoid(X) ->
   1 / (1 + math:exp(-X)).

