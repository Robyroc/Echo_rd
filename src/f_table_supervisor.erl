-module(f_table_supervisor).
-author("robyroc").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
  RestartStrategy = rest_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {router, {router, start_link, []},
    Restart, Shutdown, worker, [router]},
  Son2 = {normalizer, {normalizer, start_link, []},
    Restart, Shutdown, worker, [normalizer]},
  Son3 = {request_gateway, {request_gateway, start_link, []},
    Restart, Shutdown, worker, [request_gateway]},
  Son4 = {request_supervisor, {request_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [request_supervisor]},
  Son5 = {fixer, {fixer, start_link, []},
    Restart, Shutdown, worker, [fixer]},

  {ok, {SupFlags, [Son1, Son2, Son3, Son4, Son5]}};

init(_) ->
  {stop, badarg}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
