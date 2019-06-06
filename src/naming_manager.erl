-module(naming_manager).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/1, wait_for_handler/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
wait_for_handler() ->
  case whereis(naming_handler) of
    undefined ->
      timer:sleep(5),
      wait_for_handler();
    Pid -> Pid
  end.


start_link(Supervisor) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Supervisor], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Supervisor]) ->
  case whereis(naming_handler) of
    undefined ->
      self() ! {startup, Supervisor};
    Pid  ->
      naming_handler:reheir(Pid, self())
  end,
  {ok, #state{}}.


handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("NAMING MANAGER: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("NAMING MANAGER: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("NAMING MANAGER: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("NAMING MANAGER: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("NAMING MANAGER: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("NAMING MANAGER: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.


handle_info({startup, Supervisor}, State) ->
  ETSHandler = {naming_handler, {naming_handler, start_link, []},
    permanent, 2000, worker, [naming_handler]},
  Ret = supervisor:start_child(Supervisor, ETSHandler),
  case Ret of
    {ok, Handler} ->
      TableId = ets:new(naming_db, [set, public, named_table, {heir, self(), naming_db}, {read_concurrency, true}]),
      ets:give_away(TableId, Handler, naming_db);
    {ok, Handler, _} ->
      TableId = ets:new(naming_db, [set, public, named_table, {heir, self(), naming_db}, {read_concurrency, true}]),
      ets:give_away(TableId, Handler, naming_db)
  end,
  {noreply, State#state{}};

handle_info({'ETS-TRANSFER', TableId, Pid, Data}, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, able} ->
      lager:warning("Warning TableId: ~p HandlerPid: ~p is dying\n"
      "Table is returning to Manager, in order to be passed to the new Handler\n", [TableId, Pid]);
    {lager_only, able} ->
      lager:warning("Warning TableId: ~p HandlerPid: ~p is dying\n"
      "Table is returning to Manager, in order to be passed to the new Handler\n", [TableId, Pid]);
    {lager_off, able} ->
      io:format("Warning TableId: ~p HandlerPid: ~p is dying\n"
      "Table is returning to Manager, in order to be passed to the new Handler\n", [TableId, Pid]);
    _ -> ok
  end,
  ets:delete(naming_db, naming_handler),
  Handler = wait_for_handler(),
  ets:give_away(TableId, Handler, Data),
  {noreply, State#state{}};

handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("NAMING MANAGER: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("NAMING MANAGER: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("NAMING MANAGER: Unexpected ! message: ~p\n", [Info]);
    _ -> ok
  end,
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

