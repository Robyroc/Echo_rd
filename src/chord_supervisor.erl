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
  Son3 = {join_handler, {join_handler, start_link, []},
    Restart, Shutdown, worker, [join_handler]},
  Son4 = {communication_supervisor, {communication_supervisor, start_link, []},
    Restart, Shutdown, supervisor, [communication_supervisor]},

  {ok, {SupFlags, [Son1,
  %  Son2,                          %TODO uncomment this line
    Son3, Son4]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
