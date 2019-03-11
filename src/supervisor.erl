-module(supervisor).
-author("mrbo9").

%% API
-export([create_static/1]).

create_static(F) ->
  spawn(fun() -> init_static(F) end).

init_static(F) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(F),
  monitoring_static(Pid, F).

monitoring_static(Pid, F) ->
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown); % will kill the child too
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
      init_static(F);
    M ->
      Pid ! M
  end.