-module(checker).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/4, get_pred/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 30000).

-record(state, {pred, pred_id, own_id, n_bits}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Predecessor, PredID, OwnID, NBits) ->            %%TODO decide how to pass these parameters
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Predecessor, PredID, OwnID, NBits], []).

get_pred(PID, Address) ->
  gen_server:call(PID, {pred_find, Address}).

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
init([Predecessor, PredID, OwnID, NBits]) ->
  naming_service:notify_identity(self(), checker),
  {ok, #state{pred = Predecessor, pred_id = PredID, own_id = OwnID, n_bits = NBits}};

init(_) ->
  {stop, incorrect_params}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({pred_find, local_address}, _From, State) ->
  {reply, State#state.pred , State, ?INTERVAL};

handle_call({pred_find, Address}, _From, State) ->
  case State#state.pred of
    nil ->
      Index = hash_f:get_hashed_addr(Address),
      case Index of
        _ when Index =< State#state.own_id ->
          {reply, Address, #state{pred = Address, pred_id = Index}, ?INTERVAL};               %TODO check warning here: constructed but never used
        _ when Index > State#state.own_id ->
          CorrectIndex = Index - round(math:pow(2, State#state.n_bits)),
          {reply, Address, #state{pred = Address, pred_id = CorrectIndex}, ?INTERVAL}         %TODO check warning here: constructed but never used
      end,
      {reply, Address, #state{pred = Address, pred_id = no_ID}, ?INTERVAL};                   %TODO the problem is here!
    Predecessor ->
      Index = hash_f:get_hashed_addr(Address),
      #state{pred = Predecessor, pred_id = PredID, own_id = OwnID, n_bits = NBits} = State,
      case Index of
        _ when Index =< OwnID ->
          {Addr, PredecessorID} = predecessor_chooser(Address, Index, Predecessor, PredID),
          {reply, Addr, #state{pred = Addr, pred_id = PredecessorID}, ?INTERVAL};
        _ when Index > OwnID ->
          CorrectIndex = Index - round(math:pow(2, NBits)),
          {Addr, PredecessorID} = predecessor_chooser(Address, CorrectIndex, Predecessor, PredID),
          {reply, Addr, #state{pred = Addr, pred_id = PredecessorID}, ?INTERVAL}
      end
  end;

handle_call(Request, _From, State) ->
  io:format("CHECKER: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  io:format("CHECKER: Unexpected cast message: ~p~n", [Request]),
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
handle_info(timeout, _State) ->
  {noreply, #state{pred = nil, pred_id = nil}};

handle_info(Info, State) ->
  io:format("CHECKER: Unexpected ! message: ~p~n", [Info]),
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

predecessor_chooser(Address, AddressID, Predecessor, PredecessorID) ->
  case AddressID of
    _ when AddressID > PredecessorID ->
      {Address, AddressID};
    _ when AddressID =< PredecessorID ->
      {Predecessor, PredecessorID}
  end.