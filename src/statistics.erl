-module(statistics).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, gather/0, incoming_statistics/2, get_statistics/1, notify_join_time/1, notify_lookup_time/1, notify_finger_table_completion/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {join_time, max_lookup_time, lookup_drop, ftable_timing}).

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

get_statistics(Address) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {get_stats, Address}).

notify_join_time(Time) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {join_time, Time}).

notify_lookup_time(Time) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {lookup_time, Time}).

notify_finger_table_completion(Time) ->
  PID = naming_handler:get_identity(statistics),
  gen_server:cast(PID, {finger_completion, Time}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  self() ! startup,
  {ok, #state{max_lookup_time = 0, lookup_drop = 0}}.

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
handle_call(Request, _From, State) ->
  case logging_policies:check_policy(?MODULE) of
    lager_on -> lager:error("STATISTICS: Unexpected call message: ~p~n", [Request]);
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
  communication_manager:send_message_async(get_stats, [], Succ, no_alias),
  incoming_statistics(link_manager:get_own_address(), {JT, MLT, LD, FT}),
  {noreply, State};

handle_cast({show_stats, Address, Stats}, State) ->
  {JoinTime, HighLookupTime, LookupDrop, FtableTimings} = Stats,
  ID = hash_f:get_hashed_addr(Address),
  case logging_policies:check_policy(?MODULE) of
    lager_on ->
      lager:info("^^^^^ STATS ^^^^^~n ID: ~p~n IP: ~p~n Join time: ~p~n
      Highest lookup time: ~p~n Number of lookup timeouts: ~p~n FTable last refresh timings: ~p~n~n",
        [ID, Address, JoinTime, HighLookupTime, LookupDrop, FtableTimings]);
    _ -> ok
  end,
  {noreply, State};

handle_cast({get_stats, Address}, State) ->
  OwnAddress = link_manager:get_own_address(),
  case Address of
    OwnAddress -> ok;
    _ ->
      #state{join_time = JT,
        max_lookup_time = MLT,
        lookup_drop = LD,
        ftable_timing = FT} = State,
      communication_manager:send_message_async(stats, [{JT, MLT, LD, FT}], Address, no_alias),
      {_, Succ} = stabilizer:get_successor(),
      communication_manager:send_message_async(get_stats, [], Succ, Address)
  end,
  {noreply, State};

handle_cast({join_time, Time}, State) ->
  {noreply, State#state{join_time = Time}};

handle_cast({lookup_time, timeout}, State) ->
  {noreply, State#state{lookup_drop = State#state.lookup_drop + 1}};

handle_cast({lookup_time, Time}, State) when Time > State#state.max_lookup_time ->
  {noreply, State#state{max_lookup_time = Time}};

handle_cast({lookup_time, _Time}, State) ->
  {noreply, State};

handle_cast({finger_completion, Time}, State) ->
  {noreply, State#state{ftable_timing = Time}};

handle_cast(Request, State) ->
  case logging_policies:check_policy(?MODULE) of
    lager_on -> lager:error("STATISTICS: Unexpected cast message: ~p~n", [Request]);
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
  case logging_policies:check_policy(?MODULE) of
    lager_on -> lager:error("STATISTICS: Unexpected ! message: ~p~n", [Info]);
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
