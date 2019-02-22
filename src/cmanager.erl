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
  Rout = spawn(?MODULE, init, [self(), API]),         %TODO change init
  BManager = spawn(?MODULE, init, [self(), API]),     %TODO change init
  API ! {self(), notifyRouterCreation, Rout},
  API ! {self(), notifyBMCreation, BManager},
  connected(API, Rout, BManager, noStabilizer, noFixer, noChecker, 3).

connected(A,R,B,S,F,C,0) -> operating(A,R,B,S,F,C);

connected(API, Router, BManager, Stabilizer, Fixer, Checker, Remaining) ->
  receive
    {Router, notifyStabilizerCreation, S} ->
      connected(API, Router, BManager, S, Fixer, Checker,
        case(Stabilizer) of
          noStabilizer -> Remaining - 1;
          _ -> Remaining
        end);
    {Router, notifyFixerCreation, F} ->
      connected(API, Router, BManager, Stabilizer, F, Checker,
        case(Fixer) of
          noFixer -> Remaining - 1;
          _ -> Remaining
        end);
    {Router, notifyCheckerCreation, C} ->
      connected(API, Router, BManager, Stabilizer, Fixer, C,
        case(Checker) of
          noChecker -> Remaining - 1;
          _ -> Remaining
        end);
    _ -> connected(API, Router, BManager, Stabilizer, Fixer, Checker, Remaining)
  end.

operating(API, Router, BManager, Stabilizer, Fixer, Checker) ->
  ok.           %TODO fill this method