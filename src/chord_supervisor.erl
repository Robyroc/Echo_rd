-module(chord_supervisor).
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
  naming_manager:wait_for_handler(),

  RestartStrategy = rest_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {stabilizer, {stabilizer, start_link, []},
    Restart, Shutdown, worker, [stabilizer]},
  Son2 = {application_manager, {application_manager, start_link, []},
    Restart, Shutdown, worker, [application_manager]},
  Son3 = {join_handler, {join_handler, start_link, [self()]},
    Restart, Shutdown, worker, [join_handler]},
  Son4 = {communication_supervisor, {communication_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [communication_supervisor]},

  {ok, {SupFlags, [Son1, Son2, Son3, Son4]}}.

