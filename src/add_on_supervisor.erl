-module(add_on_supervisor).
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

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {statistics, {statistics, start_link, []},
    Restart, Shutdown, worker, [statistics]},
  Son2 = {lager_sinks_handler, {lager_sinks_handler, start_link, []},
    Restart, Shutdown, worker, [lager_sinks_handler]},

  {ok, {SupFlags, [Son1, Son2]}}.

