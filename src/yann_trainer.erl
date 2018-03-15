-module(yann_trainer).

-import(lists, []).
-import(rand, []).

-import(yann_loader, []).
-import(yann_network, []).

-export([init/1, trainer/3, training_run/3]).

init(Network) ->
   Loader = yann_loader:init(),
   Loader ! {self(), load, "data/"},
   spawn_link(?MODULE, trainer, [Network, Loader, none]).

trainer(Network, Loader, TrainingRun) ->
   receive
      {Sender, start} when TrainingRun =:= none ->
         Sender ! ok,
         NewTrainingRun = spawn_training_run(Network, Loader, 100),
         trainer(Network, Loader, NewTrainingRun);
      {Sender, start} ->
         Sender ! training,
         trainer(Network, Loader, TrainingRun);

      {Sender, stop} when TrainingRun =/= none ->
         Sender ! ok,
         TrainingRun ! stop,
         receive
            ok -> Sender ! stopped
         end,
         trainer(Network, Loader, none);
      {Sender, stop} ->
         Sender ! not_training,
         trainer(Network, Loader, TrainingRun);

      {Sender, get} when TrainingRun =:= none ->
         Sender ! {ok, Network},
         trainer(Network, Loader, TrainingRun);
      {Sender, get} ->
         Sender ! training,
         trainer(Network, Loader, TrainingRun)
   end.

spawn_training_run(Network, Loader, BatchSize) ->
   spawn_link(?MODULE, training_run, [Network, Loader, BatchSize]).

training_run(Network, Loader, BatchSize) ->
   receive
      {Sender, stop} ->
         Sender ! ok
   after 0 ->
      Batch = get_batch(Loader, BatchSize),
      if Batch =/= none -> 
            run_batch(Network, Batch),
            training_run(Network, Loader, BatchSize);
         Batch =:= none ->
            out_of_data
      end
   end.

get_batch(Loader, Size) ->
   Loader ! {self(), get, Size},
   receive
      {ok, Batch} -> Batch;
      fail -> none
   after 5000 ->
      io:format("training_run : loader is dead"),
      erlang:error(dead_loader)
   end.

run_batch(Network, Batch) ->
   undefined.

