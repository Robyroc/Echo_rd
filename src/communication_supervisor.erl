-module(communication_supervisor).
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
  naming_handler:notify_identity(self(), communication_supervisor),
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {routing_supervisor, {routing_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [routing_supervisor]},
  Son2 = {communication_manager, {communication_manager, start_link, []},
    Restart, Shutdown, worker, [communication_manager]},
  Son3 = {link_supervisor, {link_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [link_supervisor]},
  Son4 = {add_on_supervisor, {add_on_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [add_on_supervisor]},

  {ok, {SupFlags, [Son1, Son2, Son3, Son4]}}.

