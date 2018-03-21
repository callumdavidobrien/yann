-module(yann_layer).

-import(array, [new/1, map/2]).
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

% TODO
get_activation(_, _) ->
   undefined.

% TODO
get_bias(_, _) ->
   undefined.

% TODO
get_weight(_, _, _) ->
   undefined.

% TODO
change_bias(_, _, _) ->
   undefined.

% TODO
change_weight(_, _, _, _) ->
   undefined.

layer(NeuronPids, Inputs, OutputLayer) ->
   receive
      {Sender, die} ->
         Sender ! ok;

      {Sender, update, NewInputs} ->
         Sender ! ok,
         layer(NeuronPids, NewInputs, OutputLayer);

      {Sender, get_activations} ->
         Activations = get_neuron_activations(NeuronPids),
         Sender ! {ok, Activations},
         layer(NeuronPids, Inputs, OutputLayer);

      {Sender, _} ->
         Sender ! bad_message,
         layer(NeuronPids, Inputs, OutputLayer)
   end.

get_neuron_activations(NeuronPids) ->
   GetActivation = fun(_, NeuronPid) ->
      get_activation(NeuronPid)
   end,
   map(GetActivation, NeuronPids).

