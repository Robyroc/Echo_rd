-module(cmanager).
-author("robyroc").

%% API
-export([init/1]).

init(API) ->
  io:format("a"),
  receive
    {API, create} -> API ! {self(), creationResult, {success, API}};        %TODO: create new network and notify it accordingly
    {API, join, NODE} -> API ! {self(), creationResult, {success, API}};     %TODO: join the network
    {API, exit} -> ok
  end,
  ROUT = spawn(?MODULE, init, [self(), API]),
  BMANAGER = spawn(?MODULE, init, [self(), API]),
  API ! {self(), notifyRouterCreation, ROUT},
  API ! {self(), notifyBMCreation, BMANAGER},
  connected(API, ROUT, BMANAGER).

connected(API, ROUTER, BMANAGER) ->
  io:format("b"),
  ok.