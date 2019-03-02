-module(chord).

%% API exports
-export([init/2]).
%%====================================================================
%% API functions
%%====================================================================

init(PID, BlockManagerPID) ->
  spawn(fun() -> just_created(PID, BlockManagerPID) end).

%%====================================================================
%% Internal functions
%%====================================================================

just_created(PID, BlockManagerPID) ->
  CManager = spawn(cmanager, init, [self()]),
  no_network(PID, CManager, BlockManagerPID).

no_network(Creator, CManager, BlockManagerPID) ->
  receive
    {Creator, create, Nbits} ->
      CManager ! {self(), create_info, Nbits},
      notify_result(Creator, CManager, BlockManagerPID);
    {Creator, join, Address} ->
      CManager ! {self(), join, Address},
      notify_result(Creator, CManager, BlockManagerPID);
    {Creator, exit} ->
      CManager ! {self(), exit},
      ok;
    {Creator, service_command, _, _} ->
      Creator ! {self(), network_not_ready},
      no_network(Creator, CManager, BlockManagerPID);
    _ -> no_network(Creator, CManager, BlockManagerPID)
  end.

notify_result(Creator, CManager, BlockManagerPid) ->
  receive
    {CManager, creation_result, {success, INFO}} ->
      Creator ! {self(), {success, INFO}},
      started(Creator, CManager, BlockManagerPid);
    {CManager, creation_result, {fail, INFO}} ->
      Creator ! {self(), {fail, INFO}},
      no_network(Creator, CManager, BlockManagerPid)
  end.

started(Creator, CManager, BManager) ->
  ok.   %%TODO: to be filled

