-module(root_supervisor).
-author("Giacomo").

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
  RestartStrategy = one_for_all,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {naming_supervisor, {naming_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [naming_supervisor]},
  Son2 = {chord_supervisor, {chord_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [chord_supervisor]},

  {ok, {SupFlags, [Son1, Son2]}}.

