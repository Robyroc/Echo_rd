-module(internal_link_supervisor).
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
  supervisor:start_link(?MODULE, []).

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

  Son1 = {listener, {socket_listener, start_link, []},
    Restart, Shutdown, worker, [socket_listener]},
  Son2 = {h_sup, {handler_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [handler_supervisor]},

  {ok, {SupFlags, [Son1, Son2]}}.

