-module(checker).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/0, get_pred/1, clear_pred/0]).

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

clear_pred() ->
  PID = naming_handler:get_identity(checker),
  PID ! timeout.

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
handle_cast({pred_find, Address}, State) ->
  SuccList = stabilizer:get_successor_list(),
  case State#state.pred of
    nil ->
      Index = hash_f:get_hashed_addr(Address),
      case Index of
        _ when Index =< State#state.own_id ->
          communication_manager:send_message(pred_reply, [Address, SuccList], Address, no_alias),
          {noreply, #state{pred = Address, pred_id = Index}, ?INTERVAL};
        _ when Index > State#state.own_id ->
          CorrectIndex = Index - round(math:pow(2, State#state.n_bits)),
          communication_manager:send_message(pred_reply, [Address, SuccList], Address, no_alias),
          {noreply, #state{pred = Address, pred_id = CorrectIndex}, ?INTERVAL}
      end;
    Predecessor ->
      Index = hash_f:get_hashed_addr(Address),
      #state{pred = Predecessor, pred_id = PredID, own_id = OwnID, n_bits = NBits} = State,
      case Index of
        _ when Index =< OwnID ->
          {Addr, PredecessorID} = predecessor_chooser(Address, Index, Predecessor, PredID),
          communication_manager:send_message(pred_reply, [Addr, SuccList], Address, no_alias),
          {noreply, #state{pred = Addr, pred_id = PredecessorID}, ?INTERVAL};
        _ when Index > OwnID ->
          CorrectIndex = Index - round(math:pow(2, NBits)),
          {Addr, PredecessorID} = predecessor_chooser(Address, CorrectIndex, Predecessor, PredID),
          communication_manager:send_message(pred_reply, [Addr, SuccList], Address, no_alias),
          {noreply, #state{pred = Addr, pred_id = PredecessorID}, ?INTERVAL}
      end
  end;

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
handle_info(startup, _State) ->
  naming_handler:wait_service(hash_f),
  OwnAddress = link_manager:get_own_address(),
  OwnID = hash_f:get_hashed_addr(OwnAddress),
  Predecessor = params_handler:get_param(predecessor),
  PredID = hash_f:get_hashed_addr(Predecessor),
  NBits = params_handler:get_param(nbits),
  naming_handler:notify_identity(self(), checker),
  {ok, #state{pred = Predecessor, pred_id = PredID, own_id = OwnID, n_bits = NBits}};

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