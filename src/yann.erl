-module(yann).

-import(lists, []).
-import(rand, []).

-import(yann_network, []).
-import(yann_trainer, []).

-export([init/0, yann/3, random_network_changes/0]).

init() ->
   Network = yann_network:init([10, 16, 16], 768),
   RandomChanges = random_network_changes(),
   Network ! {self(), train, RandomChanges},
   receive
      ok -> ok
   after 2000 ->
      io:format("trainer : dead network"),
      erlang:error(dead_network)
   end,
   Trainer = yann_trainer:init(Network),
   spawn_link(?MODULE, yann, [Network, Trainer, false]).

yann(Network, Trainer, Training) ->
   receive
      {Sender, start_training} when not Training ->
         Sender ! ok,
         Trainer ! {self(), start},
         yann(Network, Trainer, true);
      {Sender, start_training} ->
         Sender ! training,
         yann(Network, Trainer, Training);

      {Sender, stop_training} when Training ->
         Sender ! ok,
         Trainer ! {self(), stop},
         receive
            stopped -> 
               NewNetwork = get_network(Trainer),
               yann(NewNetwork, Trainer, false)
         end;
      {Sender, stop_training} ->
         Sender ! not_training,
         yann(Network, Trainer, Training)
   end.

random_network_changes() ->
   FirstLayerChanges = random_layer_changes(16, 768),
   SecondLayerChanges = random_layer_changes(16, 16),
   ThirdLayerChanges = random_layer_changes(10, 16),
   [FirstLayerChanges, SecondLayerChanges, ThirdLayerChanges].

random_layer_changes(0, _) -> [];
random_layer_changes(LayerSize, InputSize) ->
   NeuronChange = random_neuron_change(InputSize),
   [NeuronChange|random_layer_changes(LayerSize - 1, InputSize)].

random_neuron_change(InputSize) ->
   BiasChange = rand:uniform(),
   WeightChanges = randoms(InputSize),
   {BiasChange, WeightChanges}.

randoms(0) -> [];
randoms(Size) ->
   [rand:uniform()|randoms(Size - 1)].

get_network(Trainer) ->
   Trainer ! {self(), get},
   receive
      {ok, Network} -> Network
   after 1000 ->
      io:format("yann : dead trainer"),
      erlang:error(dead_trainer)
   end.

