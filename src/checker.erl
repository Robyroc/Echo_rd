-module(checker).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/0, get_pred/1, get_pred_id/0, notify_lost_node/1, set_pred/1]).

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
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_pred(local_address) ->
  PID = naming_handler:get_identity(checker),
  gen_server:call(PID, {pred_find, local_address});

get_pred(Address) ->
  PID = naming_handler:get_identity(checker),
  gen_server:cast(PID, {pred_find, Address}).

get_pred_id() ->
  PID = naming_handler:get_identity(checker),
  gen_server:call(PID, pred_id).

set_pred(Address) ->
  PID = naming_handler:get_identity(checker),
  gen_server:call(PID, {set_pred, Address}).

notify_lost_node(Address) ->
  PID = naming_handler:get_identity(checker),
  gen_server:call(PID, {lost, Address}).

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
  self() ! startup,
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({pred_find, local_address}, _From, State) ->
  {reply, State#state.pred , State, ?INTERVAL};

handle_call({lost, Address}, _From, State) ->
  Pred = State#state.pred,
  case Address of
    Pred -> {reply, ok , State#state{pred = nil, pred_id = nil}, ?INTERVAL};
    _ -> {reply, ok, State, ?INTERVAL}
  end;

handle_call(pred_id, _From, State) ->
  {reply, State#state.pred_id , State, ?INTERVAL};

handle_call({set_pred, Address}, _From, State) ->
  ID = normalizer:normalize_as_predecessor(hash_f:get_hashed_addr(Address)),
  {reply, ok, State#state{pred = Address, pred_id = ID}};

handle_call(Request, _From, State) ->
  unexpected:error("CHECKER: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({pred_find, Address}, State) ->
  SuccList = stabilizer:get_successor_list(),
  case State#state.pred of
    nil ->
      Index = hash_f:get_hashed_addr(Address),
      case Index of
        _ when Index < State#state.own_id ->
          communication_manager:send_message_async(pred_reply, [Address, SuccList], Address, no_alias),
          {noreply, State#state{pred = Address, pred_id = Index}, ?INTERVAL};
        _ when Index >= State#state.own_id ->
          CorrectIndex = Index - round(math:pow(2, State#state.n_bits)),
          communication_manager:send_message_async(pred_reply, [Address, SuccList], Address, no_alias),
          {noreply, State#state{pred = Address, pred_id = CorrectIndex}, ?INTERVAL}
      end;
    Predecessor ->
      Index = hash_f:get_hashed_addr(Address),
      #state{pred = Predecessor, pred_id = PredID, own_id = OwnID, n_bits = NBits} = State,
      case logging_policies:check_policy(?MODULE) of
        able ->
          lagerConsole:info("]]] CHECKER [[[: ~p~n", [PredID]),
          checkerLager:info("]]] CHECKER [[[: ~p~n", [PredID]);
        able_lager -> checkerLager:info("]]] CHECKER [[[: ~p~n", [PredID]);
        unable -> ok
      end,
      case Index of
        _ when Index < OwnID ->
          {Addr, PredecessorID} = predecessor_chooser(Address, Index, Predecessor, PredID),
          communication_manager:send_message_async(pred_reply, [Addr, SuccList], Address, no_alias),
          {noreply, State#state{pred = Addr, pred_id = PredecessorID}, ?INTERVAL};
        _ when Index >= OwnID ->
          CorrectIndex = Index - round(math:pow(2, NBits)),
          {Addr, PredecessorID} = predecessor_chooser(Address, CorrectIndex, Predecessor, PredID),
          communication_manager:send_message_async(pred_reply, [Addr, SuccList], Address, no_alias),
          {noreply, State#state{pred = Addr, pred_id = PredecessorID}, ?INTERVAL}
      end
  end;

handle_cast(Request, State) ->
  unexpected:error("CHECKER: Unexpected cast message: ~p~n", [Request]),
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
handle_info(startup, _State) ->
  naming_handler:wait_service(hash_f),
  OwnAddress = link_manager:get_own_address(),
  OwnID = hash_f:get_hashed_addr(OwnAddress),
  Predecessor = nil,
  PredID = nil,
  NBits = params_handler:get_param(nbits),
  naming_handler:notify_identity(self(), checker),
  {noreply, #state{pred = Predecessor, pred_id = PredID, own_id = OwnID, n_bits = NBits}};

handle_info(timeout, State) ->
  {noreply, State#state{pred = nil, pred_id = nil}};

handle_info(Info, State) ->
  unexpected:error("CHECKER: Unexpected ! message: ~p~n", [Info]),
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