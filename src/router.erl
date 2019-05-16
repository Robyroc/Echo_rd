-module(router).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, show_table/0, local_lookup/1, update_finger_table/2, remote_lookup/2, lookup_for_join/1, notify_lost_node/1, show_id/0]).
-export([normalize_id/2, normalize_as_successor/1, normalize_as_predecessor/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {finger_table, nbits, id, time}).

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

local_lookup(ID) ->
  PID = naming_handler:get_identity(router),
  Nbits = params_handler:get_param(nbits),
  gen_server:call(PID, {lookup, ID}, Nbits*2000).                   %TODO tune timeout here accordingly

update_finger_table(Address, Theoretical) ->
  PID = naming_handler:get_identity(router),
  gen_server:cast(PID, {update, Address, Theoretical}).

remote_lookup(Requested, Alias) ->
  PID = naming_handler:get_identity(router),
  gen_server:cast(PID, {lookup, Alias, Requested}).

lookup_for_join(Address) ->
  remote_lookup(hash_f:get_hashed_addr(Address), Address).

show_table() ->
  PID = naming_handler:get_identity(router),
  gen_server:call(PID, show_table).

show_id() ->
  PID = naming_handler:get_identity(router),
  gen_server:call(PID, show_id).

normalize_id(ID, NBits) ->
  ActualID = ID rem round(math:pow(2, NBits)),
  case ActualID of
    _ when ActualID >= 0 -> ActualID;
    _ when ActualID < 0 -> ActualID + round(math:pow(2, NBits))
  end.

notify_lost_node(Address) ->
  PID = naming_handler:get_identity(router),
  gen_server:call(PID, {lost, Address}).

normalize_as_successor(ID) ->
  PID = naming_handler:get_identity(router),
  gen_server:call(PID, {normalize_succ, ID}).

normalize_as_predecessor(ID) ->
  PID = naming_handler:get_identity(router),
  gen_server:call(PID, {normalize_pred, ID}).

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
handle_call(show_table, _From, State) ->
  show_finger_table(State),
  {reply, ok, State};

handle_call(show_id, _From, State) ->
  {reply, State#state.id, State};

handle_call({lookup, Requested}, From, State) ->
  ActualRequested = normalize_id(Requested, State#state.nbits),
  {SuccID, Succ} = stabilizer:get_successor(),
  case logging_policies:check_policy(?MODULE) of
    able ->
      lagerConsole:info("$$$ ROUTER $$$:~p~n", [ActualRequested]),
      routerLager:info("$$$ ROUTER $$$:~p~n", [ActualRequested]);
    able_lager -> routerLager:info("$$$ ROUTER $$$:~p~n", [ActualRequested]);
    unable -> ok
  end,
  Next = check_if_next(ActualRequested, State#state.id, SuccID, State#state.nbits),
  case Next of
    next -> {reply, {found, Succ}, State};
    _ ->
      List = lookup(ActualRequested, State#state.id, State#state.finger_table, State#state.nbits),

      request_gateway:add_request(ActualRequested, From, List),
      {noreply, State}
  end;

handle_call({lost, Address}, _From, State) ->
  Table = State#state.finger_table,
  Fun =
    fun({T, R, A}) ->
      case A of
        Address -> {T, no_real, no_address};
        _ -> {T, R, A}
      end
    end,
  NewTable = lists:map(Fun, Table),
  Time = erlang:timestamp(),
  {reply, ok, State#state{finger_table = NewTable, time = Time}};

handle_call({normalize_succ, ID}, _From, State) ->
  {reply, adjust_successor(ID, State#state.id, State#state.nbits), State};

handle_call({normalize_pred, ID}, _From, State) ->
  {reply, adjust_predecessor(ID, State#state.id, State#state.nbits), State};

handle_call(Request, _From, State) ->
  unexpected:error("Router: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast({update, Address, Theoretical}, State) ->
  #state{id = ID, nbits = NBits, finger_table = Table} = State,
  Less = [{Theo, R, A} || {Theo, R, A} <- Table, Theo < Theoretical],
  Greater = [{Theo, R, A} || {Theo, R, A} <- Table, Theo > Theoretical],
  AdjustedID = adjust_successor(hash_f:get_hashed_addr(Address), ID, NBits),
  NewTable = lists:flatten([lists:reverse([{Theoretical, AdjustedID, Address} | lists:reverse(Less)]) | Greater]),
  EmptyLinesBefore = [Theo || {Theo, _, no_address} <- Table],
  EmptyLinesAfter = [Theo || {Theo, _, no_address} <- NewTable],
  case EmptyLinesAfter of
    [] ->
      case EmptyLinesBefore of
        [_|_] ->
          Time = erlang:timestamp(),
          Diff = timer:now_diff(Time, State#state.time) div 1000,
          statistics:notify_finger_table_completion(Diff);
        _ -> ok
      end;
    _ -> ok
  end,
  {noreply, State#state{finger_table = NewTable, nbits = State#state.nbits, id = State#state.id}};

handle_cast({lookup, Alias, Requested}, State) ->
  ActualRequested = adjust_successor(Requested, State#state.id, State#state.nbits),
  {SuccID, Succ} = stabilizer:get_successor(),
  case logging_policies:check_policy(?MODULE) of
    able ->
      lagerConsole:info("$$$ ROUTER $$$:~p~n", [ActualRequested]),
      routerLager:info("$$$ ROUTER $$$:~p~n", [ActualRequested]);
    able_lager -> routerLager:info("$$$ ROUTER $$$:~p~n", [ActualRequested]);
    unable -> ok
  end,
  Next = check_if_next(ActualRequested, State#state.id, SuccID, State#state.nbits),
  case Next of
    next ->
      communication_manager:send_message_async(lookup_response, [ActualRequested, Succ], Alias, no_alias),
      {noreply, State};
    _ ->
      Destinations = lookup(ActualRequested, State#state.id, State#state.finger_table, State#state.nbits),
      case Destinations of
        [] ->
          communication_manager:send_message_async(lookup, [ActualRequested], Succ, Alias),
          {noreply, State};
        [X | _] ->
          communication_manager:send_message_async(lookup, [ActualRequested], X, Alias),
          {noreply, State}
      end
  end;

handle_cast(Request, State) ->
  unexpected:error("Router: Unexpected cast message: ~p~n", [Request]),
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
  naming_handler:wait_service(stabilizer),
  {SuccId, Succ} = stabilizer:get_successor(),
  ID = hash_f:get_hashed_addr(link_manager:get_own_address()),
  Nbits = params_handler:get_param(nbits),
  TailRoutingTable = [{ID + round(math:pow(2, Exp)), no_real, no_address} || Exp <- lists:seq(1, Nbits - 1)],
  HeadRoutingTable = {ID + 1, SuccId, Succ},
  naming_handler:notify_identity(self(), router),
  Time = erlang:timestamp(),
  {noreply, #state{finger_table = [HeadRoutingTable | TailRoutingTable], nbits = Nbits, id = ID, time = Time}};

handle_info(Info, State) ->
  unexpected:error("Router: Unexpected ! message: ~p~n", [Info]),
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

show_finger_table(State) ->
  %TODO check if the finger table has to be in the .log file or printed in console
  routerLager:info("Finger Table: \n
  Theo\t| Real\t| Address~n"),
  [routerLager:info("~p\t| ~p\t| ~p~n", [T, R, A]) || {T, R, A} <- State#state.finger_table],  %TODO: check formatting
  ok.

lookup(Searched, ID, Table, NBits) when Searched < ID ->
  lookup(Searched + round(math:pow(2, NBits)), ID, Table, NBits);

lookup(Searched, _ID, Table, _NBits) ->
  RevTable = lists:reverse(Table),
  [Address || {_, ID, Address} <- lists:filter(fun({_, Id, _}) -> Id =/= no_real end, RevTable), ID < Searched].

check_if_next(Requested, ID, SuccId, NBits) when Requested =< ID ->
  check_if_next(Requested + round(math:pow(2, NBits)), ID, SuccId, NBits);

check_if_next(Requested, ID, SuccId, _NBits) when ((Requested > ID) and not (Requested > SuccId)) ->
  next;

check_if_next(_, _, _, _) ->
  not_next.

adjust_successor(ID, OwnId, _NBits) when ID > OwnId -> ID;
adjust_successor(ID, OwnId, NBits) -> adjust_successor(ID + round(math:pow(2, NBits)), OwnId, NBits).

adjust_predecessor(ID, OwnId, _NBits) when not (ID > OwnId) -> ID;
adjust_predecessor(ID, OwnId, NBits) -> adjust_predecessor(ID - round(math:pow(2, NBits)), OwnId, NBits).