%%%-------------------------------------------------------------------
%%% @author mrbo9
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2019 10:42
%%%-------------------------------------------------------------------
-module(normalizer).
-author("mrbo9").

-behaviour(gen_server).

%% API
-export([start_link/0,
  normalize_id/2,
  normalize_as_successor/1,
  normalize_as_successor_including/1,
  normalize_as_predecessor/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nbits, id}).

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

normalize_id(ID, NBits) ->
  ActualID = ID rem round(math:pow(2, NBits)),
  case ActualID of
    _ when ActualID >= 0 -> ActualID;
    _ when ActualID < 0 -> ActualID + round(math:pow(2, NBits))
  end.

normalize_as_successor(ID) ->
  PID = naming_handler:get_identity(normalizer),
  gen_server:call(PID, {normalize_succ, ID}).

normalize_as_successor_including(ID) ->
  PID = naming_handler:get_identity(normalizer),
  gen_server:call(PID, {normalize_succ_including, ID}).

normalize_as_predecessor(ID) ->
  PID = naming_handler:get_identity(normalizer),
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
handle_call({normalize_succ, ID}, _From, State) ->
  {reply, adjust_successor(ID, State#state.id, State#state.nbits), State};

handle_call({normalize_succ_including, ID}, _From, State) ->
  {reply, adjust_successor_including(ID, State#state.id, State#state.nbits), State};

handle_call({normalize_pred, ID}, _From, State) ->
  {reply, adjust_predecessor(ID, State#state.id, State#state.nbits), State};

handle_call(Request, _From, State) ->
  unexpected:error("Normalizer: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  unexpected:error("Normalizer: Unexpected cast message: ~p~n", [Request]),
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
handle_info(startup, State) ->
  naming_handler:wait_service(hash_f),
  ID = hash_f:get_hashed_addr(link_manager:get_own_address()),
  NBits = params_handler:get_param(nbits),
  naming_handler:notify_identity(self(), normalizer),
  {noreply, State#state{nbits = NBits, id = ID}};

handle_info(Info, State) ->
  unexpected:error("Normalizer: Unexpected ! message: ~p~n", [Info]),
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

adjust_successor(ID, OwnId, _NBits) when ID > OwnId -> ID;
adjust_successor(ID, OwnId, NBits) -> adjust_successor(ID + round(math:pow(2, NBits)), OwnId, NBits).

adjust_successor_including(ID, OwnId, _NBits) when ID >= OwnId -> ID;
adjust_successor_including(ID, OwnId, NBits) -> adjust_successor_including(ID + round(math:pow(2, NBits)), OwnId, NBits).

adjust_predecessor(ID, OwnId, _NBits) when not (ID > OwnId) -> ID;
adjust_predecessor(ID, OwnId, NBits) -> adjust_predecessor(ID - round(math:pow(2, NBits)), OwnId, NBits).