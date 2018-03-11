-module(yann_loader).

-import(lists, []).
-import(file, []).

-export([init/0, loader/2]).

init() ->
   loader("", []).

loader(Path, DataSet) ->
   receive
      {Sender, load, Path} ->
         try load_data(Path) of
            LoadedData ->
               Sender ! ok,
               loader(Path, DataSet ++ LoadedData)
         catch _ ->
            Sender ! {error, "couldn't read"},
            loader(Path, DataSet)
         end;

      {Sender, get, Count} when Count =< length(DataSet) ->
         Sender ! {ok, lists:sublist(DataSet, Count)},
         NewDataSet = lists:nthtail(Count, DataSet),
         loader(Path, NewDataSet);
      {Sender, get, Count} when Count > length(DataSet) ->
         Sender ! fail,
         loader(Path, DataSet)
   end.

load_data(Path) ->
   PathToLabelsFile = Path ++ "labels.bin",
   {ok, <<_:64, LabelsBinary>>} = file:read_file(PathToLabelsFile),
   Labels = parse_labels_binary(LabelsBinary),
   PathToImagesFile = Path ++ "images.bin",
   {ok, <<_:128, ImagesBinary>>} = file:read_file(PathToImagesFile),
   Images = parse_images_binary(ImagesBinary),
   lists:zip(Labels, Images).

parse_labels_binary(<<>>) -> [];
parse_labels_binary(<<LabelByte:8, LabelsBinary/binary>>) ->
   <<Label/integer-unsigned-big>> = LabelByte,
   [Label|parse_labels_binary(LabelsBinary)].

parse_images_binary(<<>>) -> [];
parse_images_binary(<<ImageBinary:6272, ImagesBinary/binary>>) ->
   Image = parse_image_binary(ImageBinary),
   [Image|parse_images_binary(ImagesBinary)].

parse_image_binary(<<>>) -> [];
parse_image_binary(<<PixelByte:8, ImageBinary/binary>>) ->
   <<Pixel/integer-unsigned-big>> = PixelByte,
   Brightness = Pixel / 255.0,
   [Brightness|parse_image_binary(ImageBinary)].

