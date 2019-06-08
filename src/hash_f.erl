-module(hash_f).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/0, get_hashed_addr/1, get_hashed_name/1]).

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

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_hashed_addr(Address) ->
  PID = naming_handler:get_identity(hash_f),
  gen_server:call(PID, {a_code, Address}).

get_hashed_name(Resource) ->
  PID = naming_handler:get_identity(hash_f),
  gen_server:call(PID, {n_code, Resource}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  self() ! startup,
  {ok, #state{}}.

handle_call({a_code, Address}, _From, State) ->
  Compacted = link_manager:compact_address(Address),
  Digest = crypto:bytes_to_integer(crypto:hash(sha, Compacted)),
  Index = Digest rem round(math:pow(2, State#state.nbits)),
  {reply, Index, State};

handle_call({n_code, Name}, _From, State) ->
  Digest = crypto:bytes_to_integer(crypto:hash(sha, Name)),
  Index = Digest rem round(math:pow(2, State#state.nbits)),
  {reply, Index, State}.


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(startup, _State) ->
  naming_handler:wait_service(params_handler),
  NBits = params_handler:get_param(nbits),
  naming_handler:notify_identity(self(), hash_f),
  {noreply, #state{nbits = NBits}};

handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

