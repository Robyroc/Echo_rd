-module(chord).

%% API exports
-export([init/1, justCreated/1]).
%%====================================================================
%% API functions
%%====================================================================

justCreated(PID) ->
  CMANAGER = spawn(cmanager, init, [self()]),
  noNetwork(PID, CMANAGER).

init(PID) ->
  spawn(?MODULE, justCreated, [PID]).

%%====================================================================
%% Internal functions
%%====================================================================

noNetwork(CREATOR, CMANAGER) ->
  receive
    {CREATOR, create} ->
      CMANAGER ! {self(), create},
      notifyResult(CREATOR, CMANAGER);
    {CREATOR, join, NODE} ->
      CMANAGER ! {self(), join, NODE},
      notifyResult(CREATOR, CMANAGER);
    {CREATOR, exit} ->
      CMANAGER ! {self(), exit},
      ok;
    _ -> noNetwork(CREATOR, CMANAGER)
  end.

notifyResult(CREATOR, CMANAGER) ->
  receive
    {CMANAGER, creationResult, {success, INFO}} ->
      CREATOR ! {self(), {success, INFO}},
      settingUp(CREATOR, CMANAGER, noRouter, noBManager);
    {CMANAGER, creationResult, {fail, INFO}} ->
      CREATOR ! {self(), {fail, INFO}},
      noNetwork(CREATOR, CMANAGER)
  end.

settingUp(C, CMANAGER, noRouter, noBManager) ->
  receive
    {CMANAGER, notifyRouter, R} -> settingUp(C, CMANAGER, R, noBManager);
    {CMANAGER, notifyBMananger, B} -> settingUp(C, CMANAGER, noRouter, B)
  end;

settingUp(C, CMANAGER, R, noBManager) ->
  receive
    {CMANAGER, notifyBMananger, B} -> started(C, CMANAGER, R, B)
  end;

settingUp(C, CMANAGER, noRouter, B) ->
  receive
    {CMANAGER, notifyRouter, R} -> started(C, CMANAGER, R, B)
  end.

started(CREATOR, CMANAGER, ROUTER, BMANAGER) ->
  ok.   %%TODO: to be filled

