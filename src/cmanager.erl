-module(cmanager).
-author("robyroc").

%% API
-export([init/1]).

init(API) ->
  receive
    {API, create} -> API ! {self(), creationResult, {success, API}};        %TODO: create new network and notify it accordingly
    {API, join, NODE} -> API ! {self(), creationResult, {success, API}};     %TODO: join the network
    {API, exit} -> ok
  end,
  ROUT = spawn(?MODULE, init, [self(), API]),         %TODO change init
  BMANAGER = spawn(?MODULE, init, [self(), API]),
  API ! {self(), notifyRouterCreation, ROUT},
  API ! {self(), notifyBMCreation, BMANAGER},
  connected(API, ROUT, BMANAGER, noStabilizer, noFixer, noChecker, 3).

connected(A,R,B,S,F,C,0) -> operating(A,R,B,S,F,C);

connected(API, ROUTER, BMANAGER, STABILIZER, FIXER, CHECKER, REMAINING) ->
  receive
    {ROUTER, notifyStabilizerCreation, S} ->
      connected(API, ROUTER, BMANAGER, S, FIXER, CHECKER,
        case(STABILIZER) of
          noStabilizer -> REMAINING - 1;
          _ -> REMAINING
        end);
    {ROUTER, notifyFixerCreation, F} ->
      connected(API, ROUTER, BMANAGER, STABILIZER, F, CHECKER,
        case(FIXER) of
          noFixer -> REMAINING - 1;
          _ -> REMAINING
        end);
    {ROUTER, notifyCheckerCreation, C} ->
      connected(API, ROUTER, BMANAGER, STABILIZER, FIXER, C,
        case(CHECKER) of
          noChecker -> REMAINING - 1;
          _ -> REMAINING
        end);
    _ -> connected(API, ROUTER, BMANAGER, STABILIZER, FIXER, CHECKER, REMAINING)
  end.

operating(API, ROUTER, BMANAGER, STABILIZER, FIXER, CHECKER) ->
  ok.           %TODO fill this method