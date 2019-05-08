-module(stabilizer).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/0, get_successor/0, get_successor_list/0, notify_successor/2,
  notify_lost_node/1, turn_off/0, turn_on/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 5000).

-record(state, {succ_list, id, nbits, op}).

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

get_successor() ->
  PID = naming_handler:get_identity(stabilizer),
  gen_server:call(PID, get_succ).

get_successor_list() ->
  PID = naming_handler:get_identity(stabilizer),
  gen_server:call(PID, get_succ_list).

notify_successor(Predecessor, SuccessorList) ->
  PID = naming_handler:get_identity(stabilizer),
  gen_server:cast(PID, {stabilize_response, Predecessor, SuccessorList}).

notify_lost_node(Address) ->
  PID = naming_handler:get_identity(stabilizer),
  gen_server:call(PID, {lost, Address}).

turn_off() ->
  PID = naming_handler:get_identity(stabilizer),
  gen_server:call(PID, turn_off).

turn_on(PID) ->
  gen_server:call(PID, turn_on).

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
  {ok, #state{op = no_operating}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(get_succ, _From, State) ->
  {Index, Address} = hd(State#state.succ_list),
  {reply, {Index, Address}, State};

handle_call(get_succ_list, _From, State) ->
  {reply, State#state.succ_list, State};

handle_call({lost, Address}, _From, State) ->
  NewList = [{I, A} || {I, A} <- State#state.succ_list, A =/= Address],
  case NewList of
    [] ->                                                               %The corner case is handled by splitting the network in half
      OwnAddress = link_manager:get_own_address(),
      AdjOwnId = adjust_successor(State#state.id, State#state.id, State#state.nbits),
      {reply, ok, State#state{succ_list = [{AdjOwnId, OwnAddress}]}};
    _ ->
      {SuccID, SuccAddr} = hd(NewList),
      AdjSuccID = adjust_successor(SuccID, State#state.id, State#state.nbits),
      AdjNewList = [{AdjSuccID, SuccAddr} | tl(NewList)],
      {reply, ok, State#state{succ_list = AdjNewList}}
  end;

handle_call(turn_off, _From, _State) ->
  {reply, ok, #state{succ_list = undefined, nbits = undefined, id = undefined, op = no_operating}};

handle_call(turn_on, _From, _State) ->
  self() ! startup,
  {reply, ok, #state{op = no_operating}};

handle_call(Request, _From, State) ->
  unexpected:error("STABILIZER: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({stabilize_response, Predecessor, NewSuccList}, State) ->
  PredIndex = hash_f:get_hashed_addr(Predecessor),
  HeadIndex = hd([I || {I, _} <- State#state.succ_list]),
  #state{succ_list = OwnSuccessorList, id = ID, nbits = NBits} = State,
  NewSuccessorList = handle_pred_tell(PredIndex, ID, HeadIndex, NewSuccList, OwnSuccessorList, Predecessor, NBits),
  {noreply, State#state{succ_list = NewSuccessorList}};

handle_cast(Request, State) ->
  unexpected:error("STABILIZER: Unexpected cast message: ~p~n", [Request]),
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
  ID = hash_f:get_hashed_addr(OwnAddress),
  SuccessorList = params_handler:get_param(succ_list),
  NBits = params_handler:get_param(nbits),
  Successor = params_handler:get_param(successor),
  SuccID = adjust_successor(hash_f:get_hashed_addr(Successor), ID, NBits),
  CutList = cut_last_element(SuccessorList, NBits),
  Smaller = [{I + round(math:pow(2, NBits)), A} || {I, A} <- CutList, I =< ID],
  Corrected = [{I, A} || {I, A} <- CutList, I > ID] ++ Smaller,
  NewList = update_successor_list(lists:sort(Corrected), {SuccID, Successor}, NBits),
  naming_handler:notify_identity(self(), stabilizer),
  erlang:send_after(?INTERVAL, self(), stabilize),
  {noreply, #state{succ_list = NewList, id = ID, nbits = NBits, op = operating}};

handle_info(stabilize, State) ->
  case State#state.op of
    operating ->
      Successor = hd([Addr || {_, Addr} <- State#state.succ_list]),
      communication_manager:send_message_async(ask_pred, [], Successor, no_alias),
      erlang:send_after(?INTERVAL, self(), stabilize),
      {noreply, State};
    no_operating ->
      {noreply, State}
  end;

handle_info(Info, State) ->
  unexpected:error("STABILIZER: Unexpected ! message: ~p~n", [Info]),
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

cut_last_element(SuccessorList, NBits) ->
  Length = length(SuccessorList),
  case Length of
    Length when Length > NBits ->
      [_ | T] = lists:reverse(SuccessorList),
      lists:reverse(T);
    Length when Length =< NBits ->
      SuccessorList
  end.

handle_pred_tell(PredID, ID, HeadID, _NewSuccList, OwnSuccList, Pred, NBits) when ((PredID > ID) and not(PredID >= HeadID)) ->
  update_successor_list(OwnSuccList, {PredID, Pred}, NBits);

handle_pred_tell(PredID, ID, _HeadID, NewSuccList, OwnSuccList, _Pred, NBits) when PredID =:= ID ->
  Succ = hd(OwnSuccList),
  update_successor_list(NewSuccList, Succ, NBits);

handle_pred_tell(PredID, ID, HeadID, SuccList, OwnSuccList, Pred, NBits) when PredID < ID ->
  handle_pred_tell(PredID + round(math:pow(2, NBits)), ID, HeadID, SuccList, OwnSuccList, Pred, NBits).

update_successor_list(SuccessorList, NewElem, NBits) ->
  Cut = cut_last_element(SuccessorList, NBits),
  [NewElem | Cut].

adjust_successor(ID, OwnId, _NBits) when ID > OwnId -> ID;
adjust_successor(ID, OwnId, NBits) -> adjust_successor(ID + round(math:pow(2, NBits)), OwnId, NBits).