-module(naming_service).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, notify_identity/2, get_identity/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {table, pending_requests}).

%%%===================================================================
%%% API
%%%===================================================================

notify_identity(PID, Identity) ->
  gen_server:cast(name_service, {notify, Identity, PID}).           %TODO check if timeout is needed

get_identity(Identity) ->
  Results = ets:lookup(naming_db, Identity),
  {Identity, PID} = hd(Results),
  PID.

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
  register(self(), name_service),
  Table = ets:new(naming_db, [read_concurrency, true]),
  {ok, #state{table = Table, pending_requests = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({check_id, Identity}, From, State) ->
  Result = ets:lookup(State#state.table, Identity),
  case Result of
    [] ->
      {noreply, #state{table = State#state.table,
        pending_requests = [{From, Identity} | State#state.pending_requests]}};
    [A] ->
      {reply, A, State}
  end;

handle_call(Request, _From, State) ->
  io:format("NS: Unexpected call message: ~p~n", Request),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({notify, Identity, PID}, State) ->
  ets:insert(State#state.table, {Identity, PID}),
  [gen_server:reply(A, {Identity, PID}) || {A, Identity} <- State#state.pending_requests],
  NewState = #state{table = State#state.table,
    pending_requests = [{A, Id} || {A, Id} <- State#state.pending_requests, Id =/= Identity]},
  {noreply, NewState};

handle_cast(Request, State) ->
  io:format("NS: Unexpected cast message: ~p~n", Request),
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
  io:format("NS: Unexpected ! message: ~p~n", Info),
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
  ets:delete(State#state.table),
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
