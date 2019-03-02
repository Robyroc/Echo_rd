-module(stub).
-author("robyroc").

%% API
-compile(export_all).

startup() ->
  chord:init(self(), self()).