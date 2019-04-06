-module(router).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, show_table/0, local_lookup/1, update_finger_table/2, remote_lookup/2, lookup_for_join/1, normalize_id/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {finger_table, nbits, id}).

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
  gen_server:call(PID, {lookup, ID}).

update_finger_table(Address, Theoretical) ->
  PID = naming_handler:get_identity(router),
  gen_server:cast(PID, {update, Address, Theoretical}).

remote_lookup(Alias, Requested) ->
  PID = naming_handler:get_identity(router),
  gen_server:cast(PID, {lookup, Alias, Requested}).

lookup_for_join(Address) ->
  remote_lookup(Address, hash_f:get_hashed_addr(Address)).

show_table() ->
  PID = naming_handler:get_identity(router),
  gen_server:call(PID, show_table).

normalize_id(ID, NBits) ->
  ActualID = ID rem round(math:pow(2, NBits)),
  case ActualID of
    _ when ActualID >= 0 -> ActualID;
    _ when ActualID < 0 -> ActualID + round(math:pow(2, NBits))
  end.

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

handle_call({lookup, Requested}, From, State) ->
  {_, SuccID, Succ} = hd(State#state.finger_table),
  Next = check_if_next(Requested, State#state.id, SuccID, State#state.nbits),
  case Next of
    next -> {reply, {found, Succ}, State};
    _ ->
      List = lookup(Requested, State#state.id, State#state.finger_table, State#state.nbits),

      request_gateway:add_request(Requested, From, List),
      {noreply, State}
  end;

handle_call(Request, _From, State) ->
  io:format("Router: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast({update, Address, Theoretical}, State) ->
  Table = State#state.finger_table,
  Less = [{Theo, R, A} || {Theo, R, A} <- Table, Theo < Theoretical],
  Greater = [{Theo, R, A} || {Theo, R, A} <- Table, Theo > Theoretical],
  NewTable = [lists:reverse([{Theoretical, hash_f:get_hashed_addr(Address), Address} | lists:reverse(Less)]) | Greater],
  {noreply, #state{finger_table = NewTable, nbits = State#state.nbits, id = State#state.id}};

handle_cast({lookup, Alias, Requested}, State) ->
  {_, SuccID, Succ} = hd(State#state.finger_table),
  Next = check_if_next(Requested, State#state.id, SuccID, State#state.nbits),
  case Next of
    next ->
      communication_manager:send_message(lookup_response, [Requested, Succ], Alias, no_alias),
      {noreply, State};
    _ ->
      {_, _, Destination} = hd(lookup(Requested, State#state.id, State#state.finger_table, State#state.nbits)),
      communication_manager:send_message(lookup, [Requested], Destination, Alias),
      {noreply, State}
  end;

handle_cast(Request, State) ->
  io:format("Router: Unexpected cast message: ~p~n", [Request]),
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
  {ok, #state{finger_table = [HeadRoutingTable | TailRoutingTable], nbits = Nbits, id = ID}};

handle_info(Info, State) ->
  io:format("Router: Unexpected ! message: ~p~n", [Info]),
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
  io:format("Finger Table:~n
  Theo | Real | Address~n"),
  [io:format("~p|~p|~p", [T, R, A]) || {T, R, A} <- State#state.finger_table],  %TODO: handle formatting
  ok.

lookup(Searched, ID, Table, NBits) when Searched < ID ->
  lookup(Searched + round(math:pow(2, NBits)), ID, Table, NBits);

lookup(Searched, _ID, Table, _NBits) ->
  RevTable = lists:reverse(Table),
  [Address || {_, ID, Address} <- lists:filter(fun({_, Id, _}) -> Id =/= no_real end, RevTable), ID =< Searched].

check_if_next(Requested, ID, SuccId, NBits) when Requested =< ID ->
  check_if_next(Requested + round(math:pow(2, NBits)), SuccId, ID, NBits);

check_if_next(Requested, ID, SuccId, _NBits) when (Requested > ID and not (Requested > SuccId)) ->
  next;

check_if_next(_, _, _, _) ->
  not_next.