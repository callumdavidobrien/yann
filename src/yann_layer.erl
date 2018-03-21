-module(yann_layer).

-import(array, [get/2, new/1, map/2]).
-import(yann_neuron, [init/1, get_activation/1, get_bias/1, get_weight/2, change_bias/2, change_weight/3]).

-export([init/2, init/3, kill/1, update/2, get_activations/1, get_activation/2, get_bias/2, get_weight/3, change_bias/3, change_weight/4, layer/3]).

init(LayerSize, InputSize) ->
   init(LayerSize, InputSize, none).

init(Size, InputSize, OutputLayer) ->
   NeuronPids = map(fun(_, _) -> init(InputSize) end, new(Size)),
   Inputs = map(fun(_, _) -> 0 end, new(Size)),
   LayerPid = spawn(?MODULE, layer, [NeuronPids, Inputs, OutputLayer]),
   update(LayerPid, Inputs),
   LayerPid.

kill(LayerPid) ->
   LayerPid ! {self(), die},
   receive
      ok -> ok
   after 2000 -> erlang:error(timeout)
   end.

update(LayerPid, Inputs) ->
   LayerPid ! {self(), update, Inputs},
   receive
      ok -> ok
   after 2000 -> erlang:error(timeout)
   end.

get_activations(LayerPid) ->
   LayerPid ! {self(), get_activations},
   receive
      {ok, Activations} -> Activations
   after 2000 -> erlang:error(timeout)
   end.

get_activation(LayerPid, NeuronIndex) ->
   LayerPid ! {self(), get_activation, NeuronIndex},
   receive
      {ok, Activation} -> Activation
   after 2000 -> erlang:error(timeout)
   end.

get_bias(LayerPid, NeuronIndex) ->
   LayerPid ! {self(), get_bias, NeuronIndex},
   receive
      {ok, Bias} -> Bias
   after 2000 -> erlang:error(timeout)
   end.

get_weight(LayerPid, NeuronIndex, InputIndex) ->
   LayerPid ! {self(), get_weight, NeuronIndex, InputIndex},
   receive
      {ok, Weight} -> Weight
   after 2000 -> erlang:error(timeout)
   end.

change_bias(LayerPid, NeuronIndex, Change) ->
   LayerPid ! {self(), change_bias, NeuronIndex, Change},
   receive
      ok -> ok
   after 2000 -> erlang:error(timeout)
   end.

change_weight(LayerPid, NeuronIndex, InputIndex, Change) ->
   LayerPid ! {self(), change_weight, NeuronIndex, InputIndex, Change},
   receive
      ok -> ok
   after 2000 -> erlang:error(timeout)
   end.

layer(NeuronPids, Inputs, OutputLayer) ->
   receive
      {Sender, die} ->
         Sender ! ok;

      {Sender, update, NewInputs} ->
         update_output_layer(NeuronPids, OutputLayer),
         Sender ! ok,
         layer(NeuronPids, NewInputs, OutputLayer);

      {Sender, get_activations} ->
         Activations = map(
            fun(_, NeuronPid) -> get_activation(NeuronPid) end,
            NeuronPids),
         Sender ! {ok, Activations},
         layer(NeuronPids, Inputs, OutputLayer);

      {Sender, get_activation, NeuronIndex} ->
         NeuronPid = get(NeuronIndex, NeuronPids),
         Activation = get_activation(NeuronPid),
         Sender ! {ok, Activation},
         layer(NeuronPids, Inputs, OutputLayer);

      {Sender, get_bias, NeuronIndex} ->
         NeuronPid = get(NeuronIndex, NeuronPids),
         Bias = get_bias(NeuronPid),
         Sender ! {ok, Bias},
         layer(NeuronPids, Inputs, OutputLayer);

      {Sender, get_weight, NeuronIndex, InputIndex} ->
         NeuronPid = get(NeuronIndex, NeuronPids),
         Weight = get_weight(NeuronPid, InputIndex),
         Sender ! {ok, Weight},
         layer(NeuronPids, Inputs, OutputLayer);

      {Sender, change_bias, NeuronIndex, Change} ->
         NeuronPid = get(NeuronIndex, NeuronPids),
         change_bias(NeuronPid, Change),
         update_output_layer(NeuronPids, OutputLayer),
         Sender ! ok,
         layer(NeuronPids, Inputs, OutputLayer);

      {Sender, change_weight, NeuronIndex, InputIndex, Change} ->
         NeuronPid = get(NeuronIndex, NeuronPids),
         change_weight(NeuronPid, InputIndex, Change),
         update_output_layer(NeuronPids, OutputLayer),
         Sender ! ok,
         layer(NeuronPids, Inputs, OutputLayer);
   end.

update_output_layer(NeuronPids, OutputLayer) when OutputLayer == none ->
   ok;
update_output_layer(NeuronPids, OutputLayer) ->
   Activations = map(
      fun(_, NeuronPid) -> get_activation(NeuronPid) end,
      NeuronPids),
   update(OutputLayer, Activations).

