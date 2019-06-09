-module(fixer).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MIN_FIX_TIME, 6000).
-define(MULT, 6).
-record(state, {id, nbits, index}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  self() ! startup,
  {ok, #state{}}.


handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("FIX: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("FIX: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("FIX: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("FIX: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("FIX: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("FIX: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.


handle_info(startup, _State) ->
  naming_handler:wait_service(router),
  ID = hash_f:get_hashed_addr(link_manager:get_own_address()),
  NBits = params_handler:get_param(nbits),
  naming_handler:notify_identity(self(), fixer),
  erlang:send_after(?MIN_FIX_TIME, self(), fix),
  {noreply, #state{id = ID, nbits = NBits, index = 0}};

handle_info(fix, State) ->
  Theo = (State#state.id + round(math:pow(2, State#state.index))),
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, able} ->
      lagerConsole:info("%%% FIXER %%%: ~p\n", [Theo]),
      fixerLager:info("%%% FIXER %%%: ~p\n", [Theo]);
    {lager_only, able} ->
      fixerLager:info("%%% FIXER %%%: ~p\n", [Theo]);
    {lager_off, able} ->
      io:format("%%% FIXER %%%: ~p\n", [Theo]);
    _ -> ok
  end,
  {found, Address} = router:local_lookup(Theo),
  router:update_finger_table(Address, Theo),
  case application:get_env(echo_rd, fix) of
    {ok, off} -> ok;
    _ -> erlang:send_after(max(?MIN_FIX_TIME, statistics:get_average_lookup_time() * ?MULT), self(), fix)
  end,
  {noreply, iterate_state(State)};

handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("FIX: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("FIX: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("FIX: Unexpected ! message: ~p\n", [Info]);
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

iterate_state(State) ->
  #state{id = ID, nbits = NBits, index = Index} = State,
  MaxIndex = NBits - 1,
  case Index of
    MaxIndex -> #state{id = ID, nbits = NBits, index = 0};
    _ -> State#state{id = ID, nbits = NBits, index = Index + 1}
  end.