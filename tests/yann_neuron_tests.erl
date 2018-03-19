-module(yann_neuron_tests).

-include_lib("eqc/include/eqc.hrl").

-compile({no_auto_import, [size/1]}).

-import(array, [foldl/3, from_list/1, map/2, new/1, size/1, to_list/1]).
-import(rand, [uniform/0]).

-import(yann_neuron, [init/1, update/2, change_bias/2, change_weight/3, get_activation/1, get_bias/1, get_weight/2, kill/1, neuron/3]).

-export([yann_neuron_tests/0, a_neuron_can_be_created_with_input_size/1]).

yann_neuron_tests() ->
   quickcheck(a_neuron_can_be_created_with_input_size_property()).

% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a_neuron_can_be_created_with_input_size(InputSize) ->
   NeuronPid = init(InputSize),
   NeuronPid ! {self(), invalid_message},
   receive
      bad_message -> true
   after 2000 -> false
   end.

a_neuron_can_be_created_with_input_size_property() ->
   ?FORALL(
      InputSize,
      nat(),
      a_neuron_can_be_created_with_input_size(InputSize)).

