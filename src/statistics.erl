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
  get_average_lookup_time/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(SIZE, 51).

-record(state, {join_time, max_lookup_time, lookup_drop, ftable_timing, lookup_times}).

%TODO rethink the statistics algorithm because this one may create problems:
%If a proceess exits the network while the packet is circulating, the packet will loop indefinitely. (HOPS?)

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  self() ! startup,
  {ok, #state{max_lookup_time = 0, lookup_drop = 0, join_time = 0, ftable_timing = 0, lookup_times = [1000]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
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

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(gather, State) ->
  #state{join_time = JT,
    max_lookup_time = MLT,
    lookup_drop = LD,
    ftable_timing = FT} = State,
  {_, Succ} = stabilizer:get_successor(),
  communication_manager:send_message_async(get_stats, [1], Succ, no_alias),
  incoming_statistics(link_manager:get_own_address(), {JT, MLT, LD, FT}),
  {noreply, State};

handle_cast({show_stats, Address, Stats}, State) ->
  {JoinTime, HighLookupTime, LookupDrop, FtableTimings} = Stats,
  ID = hash_f:get_hashed_addr(Address),
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:info("\n^^^^^ STATS ^^^^^\n ID: ~p\n IP: ~p\n Join time: ~p\nHighest lookup time: ~p\n Number of lookup timeouts: ~p\n FTable last refresh timings: ~p\n\n",
        [ID, Address, JoinTime, HighLookupTime, LookupDrop, FtableTimings]);
    {lager_only, _} ->
      lager:info("\n^^^^^ STATS ^^^^^\n ID: ~p\n IP: ~p\n Join time: ~p\nHighest lookup time: ~p\n Number of lookup timeouts: ~p\n FTable last refresh timings: ~p\n\n",
        [ID, Address, JoinTime, HighLookupTime, LookupDrop, FtableTimings]);
    {lager_off, _} ->
      io:format("\n^^^^^ STATS ^^^^^\n ID: ~p\n IP: ~p\n Join time: ~p\nHighest lookup time: ~p\n Number of lookup timeouts: ~p\n FTable last refresh timings: ~p\n\n",
        [ID, Address, JoinTime, HighLookupTime, LookupDrop, FtableTimings]);
    _ ->
      io:format("\n^^^^^ STATS ^^^^^\n ID: ~p\n IP: ~p\n Join time: ~p\nHighest lookup time: ~p\n Number of lookup timeouts: ~p\n FTable last refresh timings: ~p\n\n",
        [ID, Address, JoinTime, HighLookupTime, LookupDrop, FtableTimings])
  end,
  {noreply, State};

handle_cast({get_stats, Address, Number}, State) ->
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
        ftable_timing = FT} = State,
      communication_manager:send_message_async(stats, [{JT, MLT, LD, FT}], Address, no_alias),
      {_, Succ} = stabilizer:get_successor(),
      communication_manager:send_message_async(get_stats, [Number + 1], Succ, Address)
  end,
  {noreply, State};

handle_cast({join_time, Time}, State) ->
  {noreply, State#state{join_time = Time}};

handle_cast({lookup_time, timeout}, State) ->
  NewList = lists:map(fun(X) -> X * 2 end, State#state.lookup_times),
  {noreply, State#state{lookup_drop = State#state.lookup_drop + 1, lookup_times = NewList}};

handle_cast({lookup_time, Time}, State) when Time > State#state.max_lookup_time ->
  NewList = add_time(Time, State#state.lookup_times),
  {noreply, State#state{max_lookup_time = Time, lookup_times = NewList}};

handle_cast({lookup_time, Time}, State) ->
  NewList = add_time(Time, State#state.lookup_times),
  {noreply, State#state{lookup_times = NewList}};

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

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_time(Time, List) ->
  case length([Time | List]) of
    ?SIZE -> [Time | lists:reverse(tl(lists:reverse(List)))];
    _ -> [Time | List]
  end.