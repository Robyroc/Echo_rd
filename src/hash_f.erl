-module(hash_f).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/1, get_hashed_addr/1, get_hashed_res/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nbits}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(NBits) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [NBits], []).

get_hashed_addr(Address) ->
  PID = naming_service:get_identity(hash_f),
  gen_server:call(PID, {a_code, Address}).

get_hashed_res(Resource) ->
  PID = naming_service:get_identity(hash_f),
  gen_server:call(PID, {r_code, Resource}).

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
init([NBits]) ->
  naming_service:notify_identity(self(), hash_f),
  {ok, #state{nbits = NBits}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({a_code, Address}, _From, State) ->
  Compacted = link_manager:compact_address(Address),
  Digest = crypto:bytes_to_integer(crypto:hash(sha, Compacted)),
  Index = Digest rem round(math:pow(2, State#state.nbits)),
  {reply, Index, State};

handle_call({r_code, Resource}, _From, State) ->
  Digest = crypto:bytes_to_integer(crypto:hash(sha, Resource)),
  Index = Digest rem round(math:pow(2, State#state.nbits)),
  {reply, Index, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
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
handle_info(_Info, State) ->
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
