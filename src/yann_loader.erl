-module(yann_loader).

-export([init/0, loader/1, activation/3]).

init() ->
   spawn_link(?MODULE, neuron, [0, [], []]).



loader(File_Location) -> %{ok, Binary} | {error, Reason}.
   File = file:read_file(File_Liocation), 
   [File] = [File] -- [0,0,28,28],
    

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
