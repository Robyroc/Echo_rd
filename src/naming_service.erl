-module(naming_service).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, notify_identity/2, get_identity/1, wait_service/1, get_maybe_identity/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pending_requests}).

%%%===================================================================
%%% API
%%%===================================================================

notify_identity(PID, Identity) ->
  gen_server:call(naming_service, {notify, Identity, PID}).           %TODO check if timeout is needed

get_identity(Identity) ->
  Results = ets:lookup(naming_db, Identity),
  {Identity, PID} = hd(Results),
  PID.

wait_service(Name) ->
  wait_for_srv(Name).

%TODO communication message, remove it
% link_manager:send_message({6543, {192, 168, 43, 209}}, {no_alias, 17, []}).

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
  %register(self(), naming_service),
  ets:new(naming_db, [set, public, named_table]),
  {ok, #state{pending_requests = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({check_id, Identity}, From, State) ->
  Result = ets:lookup(naming_db, Identity),
  case Result of
    [] ->
      {noreply, #state{pending_requests = [{From, Identity} | State#state.pending_requests]}};
    [A] ->
      {reply, A, State}
  end;

handle_call({notify, Identity, PID}, _From, State) ->
  ets:insert(naming_db, {Identity, PID}),
  [gen_server:reply(A, {Id, PID}) || {A, Id} <- State#state.pending_requests, Id =:= Identity],
  NewState = #state{pending_requests = [{A, Id} || {A, Id} <- State#state.pending_requests, Id =/= Identity]},
  {reply, ok, NewState};

handle_call(Request, _From, State) ->
  io:format("NS: Unexpected call message: ~p~n", [Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast(Request, State) ->
  io:format("NS: Unexpected cast message: ~p~n", [Request]),
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
handle_info(Info, State) ->
  io:format("NS: Unexpected ! message: ~p~n", [Info]),
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
terminate(_Reason, State) ->
  ets:delete(naming_db),
  [gen_server:reply(A, {error, terminated}) || {A, _} <- State#state.pending_requests],
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

get_maybe_identity(Identity) ->
  try get_identity(Identity) of
    PID -> PID
  catch
      error:badarg -> no_name_registered
  end.