-module(yann_layer).

-import(lists, [map/2, zipwith/3]).

-import(yann_neuron, [init/1]).

-export([init/2, init/3, layer/3]).

init(Size, InputSize) ->
   init(Size, InputSize, none).

init(Size, InputSize, OutputLayer) ->
   NeuronSizes = lists:duplicate(Size, InputSize),
   Inputs = lists:duplicate(InputSize, 0),
   Neurons = lists:map(fun(X) -> yann_neuron:init(X) end, NeuronSizes),
   layer(Neurons, Inputs, OutputLayer).

layer(Neurons, Inputs, OutputLayer) ->
   receive
      {Sender, update, NewInputs} ->
         Sender ! ok,
         update_layer(Neurons, NewInputs, OutputLayer),
         layer(Neurons, NewInputs, OutputLayer);

      {Sender, train, Changes} ->
         Sender ! ok,
         zipwith(fun(X, Y) -> train_neuron(X, Y) end, Neurons, Changes),
         update_layer(Neurons, Inputs, OutputLayer),
         layer(Neurons, Inputs, OutputLayer);

      {Sender, get} ->
         Activations = get_neurons(Neurons),
         Sender ! {ok, Activations},
         layer(Neurons, Inputs, OutputLayer);

   end.

update_layer(Neurons, Inputs, none) ->
   lists:map(fun(X) -> update_neuron(X, Inputs) end, Neurons);
update_layer(Neurons, Inputs, OutputLayer) ->
   lists:map(fun(X) -> update_neuron(X, Inputs) end, Neurons),
   Activations = get_neurons(Neurons),
   OutputLayer ! {self(), update, Activations},
   receive
      ok -> ok
   after 1000 ->
      io:format("layer : dead layer"),
      erlang:error(dead_layer)
   end.

get_neurons(Neurons) ->
   lists:map(fun get_neuron/1, Neurons).

update_neuron(Neuron, Inputs) ->
   Neuron ! {self(), update, Inputs},
   receive
      ok -> ok
   after 1000 ->
      io:format("layer : dead neuron"),
      erlang:error(dead_neuron)
   end.

train_neuron(Neuron, {BiasChange, WeightChanges}) ->
   Neuron ! {self(), train, BiasChange, WeightChanges},
   receive
      ok -> ok
   after 1000 ->
      io:format("layer : dead neuron"),
      erlang:error(dead_neuron)
   end.

get_neuron(Neuron) ->
   Neuron ! {self(), get},
   receive
      {ok, Activation} -> Activation
   after 1000 ->
      io:format("layer : dead neuron"),
      erlang:error(dead_neuron)
   end.

