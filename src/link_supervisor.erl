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

  Son1 = {lmanager, {link_manager, start_link, []},
    Restart, Shutdown, worker, [link_manager]},
  Son2 = {link_sup, {internal_link_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [internal_link_supervisor]},

  {ok, {SupFlags, [Son1, Son2]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
