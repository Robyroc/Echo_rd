-module(internal_routing_supervisor).
-author("Giacomo").

-behaviour(supervisor).

%% API
-export([start_link/2]).

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
start_link(Predecessor, NBits) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Predecessor, NBits]).

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
init([Predecessor, NBits]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {hash_f, {hash_f, start_link, [Predecessor, NBits]},
    Restart, Shutdown, worker, [hash_f]},
  Son2 = {f_table_supervisor, {f_table_supervisor, start_link, [NBits]},
    Restart, Shutdown, supervisor, [f_table_supervisor]},
  Son3 = {checker, {checker, start_link, []},
    Restart, Shutdown, worker, [checker]},

  {ok, {SupFlags, [Son1, Son2, Son3]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
