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
  naming_handler:notify_identity(self(), communication_supervisor),
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {routing_supervisor, {routing_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [routing_supervisor]},
  Son2 = {communication_manager, {communication_manager, start_link, []},      %%TODO make killer process
    Restart, Shutdown, worker, [communication_manager]},
  Son3 = {link_supervisor, {link_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [link_supervisor]},

  {ok, {SupFlags, [Son1, Son2, Son3]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
