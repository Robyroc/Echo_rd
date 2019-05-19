-module(lookup_request).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/4, respond/2, notify_lost_node/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(TIMEOUT, 3000).           %TODO tune this parameter accordingly
-define(SERVER, ?MODULE).
-define(MIN_WAIT_TIME, 10000).

-record(state, {requested, from, list, type, time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Requested, From, List, ListType) ->
  gen_server:start_link(?MODULE, [Requested, From, List, ListType], []).

respond(PID, Address) ->
  gen_server:cast(PID, {response, Address}).

notify_lost_node(PID, Address) ->
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
init([Requested, From, List, ListType]) ->
  Time = erlang:timestamp(),
  erlang:send_after(10, self(), next),
  {ok, #state{requested = Requested, from = From, list = List, type = ListType, time = Time}};

init(_) ->
  {stop, badarg}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({lost, Address}, _From, State) ->
  List = State#state.list,
  CleanedList = [X || X <- List, X =/= Address],
  {reply, ok, State#state{list = CleanedList}};

handle_call(Request, _From, State) ->
  unexpected:error("Request: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({response, Address}, State) ->
  Time = erlang:timestamp(),
  Diff = timer:now_diff(Time, State#state.time) div 1000,
  statistics:notify_lookup_time(Diff),
  gen_server:reply(State#state.from, {found, Address}),
  {stop, normal, State};

handle_cast(Request, State) ->
  unexpected:error("Request: Unexpected cast message: ~p~n", [Request]),
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
handle_info(next, State) ->
  NewState = next_message(State),
  case NewState of
    terminate ->
      statistics:notify_lookup_time(timeout),
      {stop, not_reachable, State};
    NS when ((NS#state.list =:= []) and (NS#state.type =:= succ)) ->
      Time = statistics:get_average_lookup_time(),
      erlang:send_after(max(Time,?MIN_WAIT_TIME), self(), next),
      {noreply, NewState};
    _ ->
      erlang:send_after(?TIMEOUT, self(), next),
      {noreply, NewState}
  end;

handle_info(Info, State) ->
  unexpected:error("Request: Unexpected ! message: ~p~n", [Info]),
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


next_message(State) when ((State#state.list =:= []) and (State#state.type =:= succ)) ->
  terminate;

next_message(State) when ((State#state.list =:= []) and (State#state.type =:= finger)) ->
  List = stabilizer:get_successor_list(),
  AList = [X || {_, X} <- List],
  next_message(State#state{list = AList, type = succ});

next_message(State) ->
  Address = hd(State#state.list),
  communication_manager:send_message_async(lookup, [State#state.requested], Address, link_manager:get_own_address()),
  State#state{list = tl(State#state.list)}.