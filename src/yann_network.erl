-module(yann_network).

-import(lists, []).

-import(yann_layer, []).

-export([init/2, network/2]).

init(LayerSizes, InputSize) ->
   {Layers, _} = lists:foldl(fun make_layer/2, {[], InputSize}, LayerSizes),
   Inputs = lists:duplicate(InputSize, 0),
   update_layers(Layers, Inputs),
   spawn_link(?MODULE, network, [Layers, Inputs]).

network(Layers, Inputs) ->
   receive
      {Sender, update, NewInputs} ->
         Sender ! ok,
         update_layers(Layers, NewInputs),
         network(Layers, NewInputs);

      {Sender, train, Changes} ->
         Sender ! ok,
         train_layers(Layers, Changes),
         update_layers(Layers, Inputs),
         network(Layers, Inputs);

      {Sender, get} ->
         Output = get_output(Layers),
         Sender ! {ok, Output},
         network(Layers, Inputs)
   end.

update_layers([Layer|_], Inputs) ->
   Layer ! {self(), update, Inputs},
   receive
      ok -> ok
   after 1000 ->
      io:format("network : dead layer"),
      erlang:error(dead_layer)
   end.

train_layers(Layers, Changes) ->
   lists:zipwith(fun train_layer/2, Layers, Changes).

get_output(Layers) ->
   OutputLayer = lists:last(Layers),
   OutputLayer ! {self(), get},
   receive
      {ok, Output} -> Output
   after 1000 ->
      io:format("network : dead layer"),
      erlang:error(dead_layer)
   end.

make_layer(Size, {[], InputSize}) ->
   Layer = yann_layer:init(Size, InputSize),
   {[Layer], Size};
make_layer(Size, {[OutputLayer|Layers], InputSize}) ->
   Layer = yann_layer:init(Size, InputSize, OutputLayer),
   {[Layer|[OutputLayer|Layers]], Size}.

train_layer(Layer, Changes) ->
   Layer ! {self(), train, Changes},
   receive
      ok -> ok
   after 1000 ->
      io:format("network : dead layer"),
      erlang:error(dead_layer)
   end.

