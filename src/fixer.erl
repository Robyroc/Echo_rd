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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
handle_call(Request, _From, State) ->
  unexpected:error("FIX: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  unexpected:error("FIX: Unexpected cast message: ~p~n", [Request]),
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
  naming_handler:wait_service(router),
  ID = hash_f:get_hashed_addr(link_manager:get_own_address()),
  NBits = params_handler:get_param(nbits),
  naming_handler:notify_identity(self(), fixer),
  erlang:send_after(?MIN_FIX_TIME, self(), fix),                    %TODO tune parameters accordingly
  {noreply, #state{id = ID, nbits = NBits, index = 0}};

handle_info(fix, State) ->
  Theo = (State#state.id + round(math:pow(2, State#state.index))),
  case logging_policies:check_policy(?MODULE) of
    able ->
      lagerConsole:info("%%% FIXER %%%: ~p~n", [Theo]),
      fixerLager:info("%%% FIXER %%%: ~p~n", [Theo]);
    able_lager -> fixerLager:info("%%% FIXER %%%: ~p~n", [Theo]);
    unable -> ok
  end,
  {found, Address} = router:local_lookup(Theo),
  router:update_finger_table(Address, Theo),
  erlang:send_after(max(?MIN_FIX_TIME, statistics:get_average_lookup_time * ?MULT), self(), fix),
  {noreply, iterate_state(State)};

handle_info(Info, State) ->
  unexpected:error("FIX: Unexpected ! message: ~p~n", [Info]),
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

iterate_state(State) ->
  #state{id = ID, nbits = NBits, index = Index} = State,
  MaxIndex = NBits - 1,
  case Index of
    MaxIndex -> #state{id = ID, nbits = NBits, index = 0};
    _ -> #state{id = ID, nbits = NBits, index = Index + 1}
  end.