-module(naming_handler).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/0, notify_identity/2, get_identity/1, wait_service/1,
  get_maybe_identity/1, reheir/2, delete_comm_tree/0]).

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

notify_identity(PID, Identity) ->
  try
    %TODO check lager and policy
    case logging_policies:check_lager_policy(?MODULE) of
      {lager_on, able} ->
        lager:info("=== NEW ENTRY === Name:~p ===\n", [Identity]);
      {lager_only, able} ->
        lager:info("=== NEW ENTRY === Name:~p ===\n", [Identity]);
      {lager_off, able} ->
        io:format("=== NEW ENTRY === Name:~p ===\n", [Identity]);
      _ -> ok
    end,
    Naming = get_identity(naming_handler),
    gen_server:call(Naming, {notify, Identity, PID})          %TODO check if timeout is needed
  of
    A -> A
  catch
    _:_ ->
      timer:sleep(100),
      notify_identity(PID, Identity)
  end.

get_identity(Identity) ->
  Results = ets:lookup(naming_db, Identity),
  {Identity, PID} = hd(Results),
  PID.

wait_service(Name) ->
  wait_for_srv(Name).

reheir(PID, NewManager) ->
  gen_server:call(PID, {reheir, NewManager}).

get_maybe_identity(Identity) ->
  try get_identity(Identity) of
    PID -> PID
  catch
    error:badarg -> no_name_registered
  end.

delete_comm_tree() ->
  Naming = get_identity(naming_handler),
  gen_server:call(Naming, delete).          %TODO check if timeout is needed

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({notify, Identity, PID}, _From, State) ->
  ets:insert(naming_db, {Identity, PID}),
  {reply, ok, State};

handle_call(delete, _From, State) ->
  ets:delete(naming_db, params_handler),
  ets:delete(naming_db, hash_f),
  ets:delete(naming_db, stabilizer),
  ets:delete(naming_db, router),
  ets:delete(naming_db, port),
  ets:delete(naming_db, lager_sinks_handler),
  ets:delete(naming_db, link_manager),
  ets:delete(naming_db, listener),
  {reply, ok, State};

handle_call({reheir, NewManager}, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:info("Naming Handler: Changing Heir options\n");
    {lager_only, _} ->
      lager:info("Naming Handler: Changing Heir options\n");
    {lager_off, _} ->
      io:format("Naming Handler: Changing Heir options\n");
    _ -> ok
  end,
  ets:setopts(naming_db, {heir, NewManager, naming_db}),
  {reply, ok, State};

handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Naming Handler: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("Naming Handler: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("Naming Handler: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Naming Handler: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("Naming Handler: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("Naming Handler: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'ETS-TRANSFER', TableId, Pid, _Data}, State) ->
  ets:insert(naming_db, {naming_handler, self()}),
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, able} ->
      lager:info("Manager(~p) -> Handler(~p) getting TableId: ~p\n", [Pid, self(), TableId]);
    {lager_only, able} ->
      lager:info("Manager(~p) -> Handler(~p) getting TableId: ~p\n", [Pid, self(), TableId]);
    {lager_off, able} ->
      io:format("Manager(~p) -> Handler(~p) getting TableId: ~p\n", [Pid, self(), TableId]);
    _ -> ok
  end,
  {noreply, State};

handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Naming Handler: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("Naming Handler: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("Naming Handler: Unexpected ! message: ~p\n", [Info]);
    _ -> ok
  end,
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:info("Naming Handler is terminating\n");
    {lager_only, _} ->
      lager:info("Naming Handler is terminating\n");
    {lager_off, _} ->
      io:format("Naming Handler is terminating\n");
    _ -> ok
  end,
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

wait_for_srv(Name) ->
  case get_maybe_identity(Name) of
    no_name_registered ->
      timer:sleep(100),
      wait_for_srv(Name);
    _ -> ok
  end.