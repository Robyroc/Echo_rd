-module(lookup_request).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/4, respond/3, notify_lost_node/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(TIMEOUT, 3000).
-define(SERVER, ?MODULE).
-define(MIN_WAIT_TIME, 10000).

-record(state, {requested, from, list, type, time, last}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Requested, From, List, ListType) ->
  gen_server:start_link(?MODULE, [Requested, From, List, ListType], []).

respond(PID, Address, Length) ->
  gen_server:cast(PID, {response, Address, Length}).

notify_lost_node(PID, Address) ->
  gen_server:call(PID, {lost, Address}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Requested, From, List, ListType]) ->
  Time = erlang:timestamp(),
  erlang:send_after(10, self(), next),
  {ok, #state{requested = Requested, from = From, list = List, type = ListType, time = Time}};

init(_) ->
  {stop, badarg}.

handle_call({lost, Address}, _From, State) ->
  List = State#state.list,
  CleanedList = [X || X <- List, X =/= Address],
  {reply, ok, State#state{list = CleanedList}};

handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Request: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("Request: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("Request: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast({response, Address, Length}, State) ->
  Time = erlang:timestamp(),
  Diff = timer:now_diff(Time, State#state.time) div 1000,
  statistics:notify_lookup_time(Diff),
  statistics:notify_lookup_length(Length),
  gen_server:reply(State#state.from, {found, Address}),
  {stop, normal, State};

handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Request: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("Request: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("Request: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.

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
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Request: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("Request: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("Request: Unexpected ! message: ~p\n", [Info]);
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

next_message(State) when ((State#state.list =:= []) and (State#state.type =:= succ)) ->
  terminate;

next_message(State) when ((State#state.list =:= []) and (State#state.type =:= finger)) ->
  List = stabilizer:get_successor_list(),
  AList = [X || {_, X} <- List],
  next_message(State#state{list = AList, type = succ});

next_message(State) ->
  Address = hd(State#state.list),
  Last = State#state.last,
  case Address of
    Last ->
      State#state{list = tl(State#state.list)};
    _ ->
      communication_manager:send_message_async(lookup, [State#state.requested], Address, link_manager:get_own_address()),
      State#state{list = tl(State#state.list), last = Address}
  end.