-module(yann_neuron_tests).

-include_lib("eqc/include/eqc.hrl").

-compile({no_auto_import, [size/1]}).

-import(array, [foldl/3, from_list/1, map/2, new/1, size/1, to_list/1]).
-import(rand, [uniform/0]).

-import(yann_neuron, [init/1, update/2, change_bias/2, change_weight/3, get_activation/1, get_bias/1, get_weight/2, kill/1, neuron/3]).

-export([run_all_tests/0, a_neuron_can_be_created_with_input_size/1, activation_of_neuron_can_be_got/1, bias_of_neuron_can_be_got/1, weight_of_neuron_for_nth_input_can_be_got/2, bias_of_neuron_can_be_changed/2, weight_of_neuron_for_nth_input_can_be_changed/3, inputs_of_neuron_can_be_updated_to/2]).

run_all_tests() ->
   quickcheck(property_any_neuron_can_be_created_with_input_size()),
   quickcheck(property_any_neurons_activation_can_be_got()),
   quickcheck(property_any_neurons_bias_can_be_got()),
   quickcheck(property_any_neurons_weight_for_any_of_its_inputs_can_be_got()),
   quickcheck(property_any_neuron_can_be_killed()),
   quickcheck(property_any_neurons_bias_can_be_changed()),
   quickcheck(property_any_neurons_weight_for_any_of_its_inputs_can_be_changed()),
   quickcheck(property_the_inputs_of_any_neuron_can_be_updated()).

property_any_neuron_can_be_created_with_input_size() ->
   ?FORALL(
      InputSize,
      nat(),
      a_neuron_can_be_created_with_input_size(InputSize)).

property_any_neurons_activation_can_be_got() ->
   ?FORALL(
      NeuronPid,
      neuron_pid(),
      activation_of_neuron_can_be_got(NeuronPid)).

property_any_neurons_bias_can_be_got() ->
   ?FORALL(
      NeuronPid,
      neuron_pid(),
      bias_of_neuron_can_be_got(NeuronPid)).

property_any_neurons_weight_for_any_of_its_inputs_can_be_got() ->
   ?FORALL(
      {NeuronPid, Index},
      neuron_index_pair(),
      weight_of_neuron_for_nth_input_can_be_got(NeuronPid, Index)).

property_any_neuron_can_be_killed() ->
   ?FORALL(
      NeuronPid,
      neuron_pid(),
      neuron_can_be_killed(NeuronPid)).

property_any_neurons_bias_can_be_changed() ->
   ?FORALL(
      {NeuronPid, Change},
      {neuron_pid(), real()},
      bias_of_neuron_can_be_changed(NeuronPid, Change)).

property_any_neurons_weight_for_any_of_its_inputs_can_be_changed() ->
   ?FORALL(
      {{NeuronPid, Index}, Change},
      {neuron_index_pair(), real()},
      weight_of_neuron_for_nth_input_can_be_changed(NeuronPid, Index, Change)).

property_the_inputs_of_any_neuron_can_be_updated() ->
   ?FORALL(
      {NeuronPid, Inputs},
      neuron_inputs_pair(),
      inputs_of_neuron_can_be_updated_to(NeuronPid, Inputs)).

% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a_neuron_can_be_created_with_input_size(InputSize) ->
   NeuronPid = init(InputSize),
   NeuronPid ! {self(), invalid_message},
   receive
      bad_message -> true
   after 2000 -> false
   end.

activation_of_neuron_can_be_got(NeuronPid) ->
   Activation = get_activation(NeuronPid),
   is_float(Activation).

bias_of_neuron_can_be_got(NeuronPid) ->
   Bias = get_bias(NeuronPid),
   is_float(Bias).

weight_of_neuron_for_nth_input_can_be_got(NeuronPid, Index) ->
   Weight = get_weight(NeuronPid, Index),
   is_float(Weight).

neuron_can_be_killed(NeuronPid) ->
   Response = kill(NeuronPid),
   NeuronPid ! {self(), foo},
   receive
      _ -> false
   after 10 ->
      Response == ok
   end.

bias_of_neuron_can_be_changed(NeuronPid, Change) ->
   Response = change_bias(NeuronPid, Change),
   Response == ok.

weight_of_neuron_for_nth_input_can_be_changed(NeuronPid, Index, Change) ->
   Response = change_weight(NeuronPid, Index, Change),
   Response == ok.

inputs_of_neuron_can_be_updated_to(NeuronPid, Inputs) ->
   Response = update(NeuronPid, Inputs),
   Response == ok.

% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neuron_pid() ->
   ?LET(
      InputSize,
      nat(),
      init(InputSize)).

neuron_index_pair() ->
   ?LET(
      {InputSize, Index},
      input_size_index_pair(),
      {init(InputSize), Index}).

input_size_index_pair() ->
   ?SUCHTHAT(
      {InputSize, Index},
      {nat(), nat()},
      Index < InputSize).

neuron_inputs_pair() ->
   ?LET(
      InputSize,
      nat(),
      {init(InputSize), neuron_inputs(InputSize)}).

neuron_inputs(InputSize) ->
   RandomFloat = fun(_, _) ->
      uniform()
   end,
   Array = new(InputSize),
   map(RandomFloat, Array).

