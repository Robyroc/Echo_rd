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


init([]) ->
  self() ! startup,
  {ok, #state{}}.


handle_call({pred_find, local_address}, _From, State) ->
  {reply, State#state.pred , State, get_timeout()};

handle_call({lost, Address}, _From, State) ->
  Pred = State#state.pred,
  case Address of
    Pred -> {reply, ok , State#state{pred = nil, pred_id = nil}, get_timeout()};
    _ -> {reply, ok, State, get_timeout()}
  end;

handle_call(pred_id, _From, State) ->
  {reply, State#state.pred_id , State, get_timeout()};

handle_call({set_pred, Address}, _From, State) ->
  ID = normalizer:normalize_as_predecessor(hash_f:get_hashed_addr(Address)),
  {reply, ok, State#state{pred = Address, pred_id = ID}};

handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("CHECKER: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("CHECKER: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("CHECKER: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast({pred_find, Address}, State) ->
  SuccList = stabilizer:get_successor_list(),
  case State#state.pred of
    nil ->
      Index = hash_f:get_hashed_addr(Address),
      case Index of
        _ when Index < State#state.own_id ->
          communication_manager:send_message_async(pred_reply, [Address, SuccList], Address, no_alias),
          {noreply, State#state{pred = Address, pred_id = Index}, get_timeout()};
        _ when Index >= State#state.own_id ->
          CorrectIndex = Index - round(math:pow(2, State#state.n_bits)),
          communication_manager:send_message_async(pred_reply, [Address, SuccList], Address, no_alias),
          {noreply, State#state{pred = Address, pred_id = CorrectIndex}, get_timeout()}
      end;
    Predecessor ->
      Index = hash_f:get_hashed_addr(Address),
      #state{pred = Predecessor, pred_id = PredID, own_id = OwnID, n_bits = NBits} = State,
      case logging_policies:check_lager_policy(?MODULE) of
        {lager_on, able} ->
          lagerConsole:info("]]] CHECKER [[[: ~p\n", [PredID]),
          checkerLager:info("]]] CHECKER [[[: ~p\n", [PredID]);
        {lager_only, able} ->
          checkerLager:info("]]] CHECKER [[[: ~p\n", [PredID]);
        {lager_off, able} ->
          io:format("]]] CHECKER [[[: ~p\n", [PredID]);
        _ -> ok
      end,
      case Index of
        _ when Index < OwnID ->
          {Addr, PredecessorID} = predecessor_chooser(Address, Index, Predecessor, PredID),
          communication_manager:send_message_async(pred_reply, [Addr, SuccList], Address, no_alias),
          {noreply, State#state{pred = Addr, pred_id = PredecessorID}, get_timeout()};
        _ when Index >= OwnID ->
          CorrectIndex = Index - round(math:pow(2, NBits)),
          {Addr, PredecessorID} = predecessor_chooser(Address, CorrectIndex, Predecessor, PredID),
          communication_manager:send_message_async(pred_reply, [Addr, SuccList], Address, no_alias),
          {noreply, State#state{pred = Addr, pred_id = PredecessorID}, get_timeout()}
      end
  end;

handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("CHECKER: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("CHECKER: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("CHECKER: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.


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
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("CHECKER: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("CHECKER: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("CHECKER: Unexpected ! message: ~p\n", [Info]);
    _ -> ok
  end,
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.


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

get_timeout() ->
  Delay = ceil(rand:normal(?INTERVAL, 250000)),
  case Delay of
    _ when Delay < 0 -> 0;
    _ -> Delay
  end.