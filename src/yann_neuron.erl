-module(yann_neuron).

-compile({no_auto_import, [size/1]}).

-import(array, [get/2, new/1, map/2, set/3, size/1, to_list/1]).
-import(list, [sum/1, zipwith/3]).
-import(math, [exp/1]).
-import(rand, [uniform/0]).

-export([init/1, update/2, change_bias/2, change_weight/3, get_activation/1, get_bias/1, get_weight/2, kill/1, neuron/3]).

init(InputSize) ->
   Weights = uniform_randoms(InputSize, {-1, 1}),
   Init = fun(_, _) -> 0 end,
   Inputs = map(Init, new(InputSize)),
   spawn_link(?MODULE, neuron, [0, Weights, Inputs]).

uniform_randoms(Size, {Min, Max}) ->
   Random = fun(_, _) ->
      (Max - Min) * uniform() + Min
   end,
   Array = new(Size),
   map(Random, Array).

update(NeuronPid, Inputs) ->
   NeuronPid ! {self(), update, Inputs},
   receive
      ok -> ok;
      bad_input_size -> erlang:error(bad_neuron_input_size)
   after 1000 -> erlang:error(timeout)
   end.

change_bias(NeuronPid, Change) ->
   NeuronPid ! {self(), change_bias, Change},
   receive
      ok -> ok
   after 1000 -> erlang:error(timeout)
   end.

change_weight(NeuronPid, Index, Change) ->
   NeuronPid ! {self(), change_weight, Index, Change},
   receive
      ok -> ok;
      bad_index -> erlang:error(bad_neuron_weight_index)
   after 1000 -> erlang:error(timeout)
   end.

get_activation(NeuronPid) ->
   NeuronPid ! {self(), get_activation},
   receive
      {ok, Activation} -> Activation
   after 1000 -> erlang:error(timeout)
   end.

get_bias(NeuronPid) ->
   NeuronPid ! {self(), get_bias},
   receive
      {ok, Bias} -> Bias
   after 1000 -> erlang:error(timeout)
   end.

get_weight(NeuronPid, Index) ->
   NeuronPid ! {self(), get_weight, Index},
   receive
      {ok, Weight} -> Weight;
      bad_index -> erlang:error(bad_neuron_weight_index)
   after 1000 -> erlang:error(timeout)
   end.

kill(NeuronPid) ->
   NeuronPid ! {self(), die},
   receive
      ok -> ok
   after 1000 -> erlang:error(timeout)
   end.

neuron(Bias, Weights, Inputs) ->
   receive
      {Sender, update, NewInputs} ->
         ValidInputSize = size(NewInputs) =:= size(Inputs),
         if ValidInputSize ->
               Sender ! ok,
               neuron(Bias, Weights, NewInputs);
            not ValidInputSize ->
               Sender ! bad_input_size,
               neuron(Bias, Weights, Inputs)
         end;

      {Sender, change_bias, Change} ->
         Sender ! ok,
         NewBias = Bias + Change,
         neuron(NewBias, Weights, Inputs);

      {Sender, change_weight, Index, Change} ->
         InRangeIndex = (Index >= 1) and (Index =< size(Weights)),
         if InRangeIndex ->
               Sender ! ok,
               OldWeight = get(Index, Weights),
               NewWeight = OldWeight + Change,
               NewWeights = set(Index, NewWeight, Weights),
               neuron(Bias, NewWeights, Inputs);
            not InRangeIndex ->
               Sender ! bad_index,
               neuron(Bias, Weights, Inputs)
         end;

      {Sender, get_activation} ->
         Activation = activation(Bias, Weights, Inputs),
         Sender ! {ok, Activation},
         neuron(Bias, Weights, Inputs);

      {Sender, get_bias} ->
         Sender ! {ok, Bias},
         neuron(Bias, Weights, Inputs);

      {Sender, get_weight, Index} ->
         InRangeIndex = (Index >= 1) and (Index =< size(Weights)),
         if InRangeIndex ->
               Weight = get(Index, Weights),
               Sender ! {ok, Weight},
               neuron(Bias, Weights, Inputs);
            not InRangeIndex ->
               Sender ! bad_index,
               neuron(Bias, Weights, Inputs)
         end;

      {Sender, die} ->
         Sender ! ok;

      {Sender, _} ->
         Sender ! bad_message
   end.

activation(Bias, Weights, Inputs) ->
   sigmoid(raw_activation(Bias, Weights, Inputs)).
         
raw_activation(Bias, Weights, Inputs) ->
   WeightList = to_list(Weights),
   InputList = to_list(Inputs),
   Activations = zipwith(fun(X, Y) -> X * Y end, WeightList, InputList),
   sum(Activations) + Bias.

sigmoid(X) ->
   1 / (1 + math:exp(-X)).

