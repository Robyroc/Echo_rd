-module(statistics).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0,
  gather/0,
  incoming_statistics/2,
  get_statistics/2,
  notify_join_time/1,
  notify_lookup_time/1,
  notify_finger_table_completion/1,
  get_average_lookup_time/0,
  notify_lookup_length/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(SIZE, 51).

-record(state, {join_time, max_lookup_time, lookup_drop, ftable_timing, lookup_times, lookup_lengths}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

gather() ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, gather).

incoming_statistics(Address, Stats) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {show_stats, Address, Stats}).

get_statistics(Address, Number) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {get_stats, Address, Number}).

notify_join_time(Time) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {join_time, Time}).

notify_lookup_time(Time) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {lookup_time, Time}).

notify_finger_table_completion(Time) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {finger_completion, Time}).

get_average_lookup_time() ->
  PID = naming_handler:get_identity(statistics),
  gen_server:call(PID, avg_lookup_time).

notify_lookup_length(Length) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {lookup_length, Length}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  self() ! startup,
  {ok, #state{max_lookup_time = 0, lookup_drop = 0, join_time = 0, ftable_timing = 0, lookup_times = [1000], lookup_lengths = [0]}}.


handle_call(avg_lookup_time, _From, State) ->
  Sum = lists:sum(State#state.lookup_times),
  {reply, Sum div length(State#state.lookup_times), State};

handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("STATISTICS: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("STATISTICS: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("STATISTICS: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast(gather, State) ->
  #state{join_time = JT,
    max_lookup_time = MLT,
    lookup_drop = LD,
    ftable_timing = FT,
    lookup_times = LT,
    lookup_lengths = LL} = State,
  {_, Succ} = stabilizer:get_successor(),
  communication_manager:send_message_async(get_stats, [1], Succ, no_alias),
  incoming_statistics(link_manager:get_own_address(), {JT, MLT, get_avg_integer(LT), get_avg_float(LL), LD, FT}),
  {noreply, State};

handle_cast({show_stats, Address, Stats}, State) ->
  {JoinTime, HighLookupTime, AvgLookupTime, AvgLookupLength, LookupDrop, FtableTimings} = Stats,
  ID = hash_f:get_hashed_addr(Address),
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:info("\n^^^^^ STATS ^^^^^\n ID: ~p\n IP: ~p\n Join time: ~p\n Highest lookup time: ~p\n Average lookup time: ~p\n Average lookup length: ~p\n Number of lookup timeouts: ~p\n FTable last refresh timings: ~p\n\n",
        [ID, Address, JoinTime, HighLookupTime, AvgLookupTime, AvgLookupLength, LookupDrop, FtableTimings]);
    {lager_only, _} ->
      lager:info("\n^^^^^ STATS ^^^^^\n ID: ~p\n IP: ~p\n Join time: ~p\n Highest lookup time: ~p\n Average lookup time: ~p\n Average lookup length: ~p\n Number of lookup timeouts: ~p\n FTable last refresh timings: ~p\n\n",
        [ID, Address, JoinTime, HighLookupTime, AvgLookupTime, AvgLookupLength, LookupDrop, FtableTimings]);
    {lager_off, _} ->
      io:format("\n^^^^^ STATS ^^^^^\n ID: ~p\n IP: ~p\n Join time: ~p\n Highest lookup time: ~p\n Average lookup time: ~p\n Average lookup length: ~p\n Number of lookup timeouts: ~p\n FTable last refresh timings: ~p\n\n",
        [ID, Address, JoinTime, HighLookupTime, AvgLookupTime, AvgLookupLength, LookupDrop, FtableTimings]);
    _ ->
      io:format("\n^^^^^ STATS ^^^^^\n ID: ~p\n IP: ~p\n Join time: ~p\n Highest lookup time: ~p\n Average lookup time: ~p\n Average lookup length: ~p\n Number of lookup timeouts: ~p\n FTable last refresh timings: ~p\n\n",
        [ID, Address, JoinTime, HighLookupTime, AvgLookupTime, AvgLookupLength, LookupDrop, FtableTimings])
  end,
  {noreply, State};

handle_cast({get_stats, Address, Number}, State) ->
  NBits = params_handler:get_param(nbits),
  Max = math:pow(2, NBits),
  case Number of
    Max -> {noreply, State};
    _ -> get_stats(Address, Number, State)
  end;

handle_cast({join_time, Time}, State) ->
  {noreply, State#state{join_time = Time}};

handle_cast({lookup_time, timeout}, State) ->
  NewList = lists:map(fun(X) -> X * 2 end, State#state.lookup_times),
  {noreply, State#state{lookup_drop = State#state.lookup_drop + 1, lookup_times = NewList}};

handle_cast({lookup_time, Time}, State) when Time > State#state.max_lookup_time ->
  NewList = add_to_list(Time, State#state.lookup_times),
  {noreply, State#state{max_lookup_time = Time, lookup_times = NewList}};

handle_cast({lookup_time, Time}, State) ->
  NewList = add_to_list(Time, State#state.lookup_times),
  {noreply, State#state{lookup_times = NewList}};

handle_cast({lookup_length, Length}, State) ->
  NewList = add_to_list(Length, State#state.lookup_lengths),
  {noreply, State#state{lookup_lengths = NewList}};

handle_cast({finger_completion, Time}, State) ->
  {noreply, State#state{ftable_timing = Time}};

handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("STATISTICS: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("STATISTICS: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("STATISTICS: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.

handle_info(startup, State) ->
  naming_handler:wait_service(stabilizer),
  naming_handler:notify_identity(self(), statistics),
  {noreply, State};

handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("STATISTICS: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("STATISTICS: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("STATISTICS: Unexpected ! message: ~p\n", [Info]);
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

add_to_list(Elem, List) ->
  case length([Elem | List]) of
    ?SIZE -> [Elem | lists:reverse(tl(lists:reverse(List)))];
    _ -> [Elem | List]
  end.


get_stats(Address, Number, State) ->
  OwnAddress = link_manager:get_own_address(),
  case Address of
    OwnAddress ->
      case logging_policies:check_lager_policy(?MODULE) of
        {lager_on, _} ->
          lager:info("Number of nodes: ~p\n", [Number]);
        {lager_only, _} ->
          lager:info("Number of nodes: ~p\n", [Number]);
        {lager_off, _} ->
          io:format("Number of nodes: ~p\n", [Number]);
        _ -> ok
      end;
    _ ->
      #state{join_time = JT,
        max_lookup_time = MLT,
        lookup_drop = LD,
        ftable_timing = FT,
        lookup_times = LT,
        lookup_lengths = LL} = State,
      communication_manager:send_message_async(stats, [{JT, MLT, get_avg_for_statistics(LT), get_avg_float(LL), LD, FT}], Address, no_alias),
      {_, Succ} = stabilizer:get_successor(),
      communication_manager:send_message_async(get_stats, [Number + 1], Succ, Address)
  end,
  {noreply, State}.

get_avg_for_statistics(TimeList) ->
  case length(TimeList) of
    Length when Length < ?SIZE - 1 ->
      get_avg_integer(tl(lists:reverse(TimeList)));
    _ ->
      get_avg_integer(TimeList)
  end.

get_avg_integer(List) ->
  (lists:sum(List)) div (length(List)).

get_avg_float(List) ->
  (lists:sum(List)) / (length(List)).