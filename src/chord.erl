-module(chord).

%% API exports
-export([init/1, justCreated/1]).
%%====================================================================
%% API functions
%%====================================================================

justCreated(PID) ->
  CManager = spawn(cmanager, init, [self()]),
  noNetwork(PID, CManager).

init(PID) ->
  spawn(?MODULE, justCreated, [PID]).

%%====================================================================
%% Internal functions
%%====================================================================

noNetwork(Creator, CManager) ->
  receive
    {Creator, create} ->
      CManager ! {self(), create},
      notifyResult(Creator, CManager);
    {Creator, join, NODE} ->
      CManager ! {self(), join, NODE},
      notifyResult(Creator, CManager);
    {Creator, exit} ->
      CManager ! {self(), exit},
      ok;
    _ -> noNetwork(Creator, CManager)
  end.

notifyResult(Creator, CManager) ->
  receive
    {CManager, creationResult, {success, INFO}} ->
      Creator ! {self(), {success, INFO}},
      settingUp(Creator, CManager, noRouter, noBManager);
    {CManager, creationResult, {fail, INFO}} ->
      Creator ! {self(), {fail, INFO}},
      noNetwork(Creator, CManager)
  end.

settingUp(C, CManager, noRouter, noBManager) ->
  receive
    {CManager, notifyRouter, R} -> settingUp(C, CManager, R, noBManager);
    {CManager, notifyBMananger, B} -> settingUp(C, CManager, noRouter, B)
  end;

settingUp(C, CManager, R, noBManager) ->
  receive
    {CManager, notifyBMananger, B} -> started(C, CManager, R, B)
  end;

settingUp(C, CManager, noRouter, B) ->
  receive
    {CManager, notifyRouter, R} -> started(C, CManager, R, B)
  end.

started(Creator, CManager, Router, BManager) ->
  ok.   %%TODO: to be filled

