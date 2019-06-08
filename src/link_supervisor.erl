-module(link_supervisor).
-author("robyroc").

-behavior(supervisor).

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
  naming_handler:notify_identity(self(), link_supervisor),
  RestartStrategy = rest_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,

  Son1 = {lmanager, {link_manager, start_link, []},
    Restart, Shutdown, worker, [link_manager]},
  Son2 = {link_sup, {internal_link_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [internal_link_supervisor]},

  {ok, {SupFlags, [Son1, Son2]}}.
