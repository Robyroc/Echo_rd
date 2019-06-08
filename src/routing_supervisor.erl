-module(routing_supervisor).
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
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {hash_f, {hash_f, start_link, []},
    Restart, Shutdown, worker, [hash_f]},
  Son2 = {f_table_supervisor, {f_table_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [f_table_supervisor]},
  Son3 = {checker, {checker, start_link, []},
    Restart, Shutdown, worker, [checker]},

  {ok, {SupFlags, [Son1, Son2, Son3]}}.

