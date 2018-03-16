-module(yann_neuron_tests).

-compile({no_auto_import, [size/1]}).

-import(array, [foldl/3, from_list/1, size/1])
-import(yann_neuron, [init/1, update/2, change_bias/2, change_weight/3, get_activation/1, get_bias/1, get_weight/2, kill/1, neuron/3]).

-include_lib("eqc/include/eqc.hrl").

