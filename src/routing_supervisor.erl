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
  naming_handler:wait_service(params_handler),
  %%TODO getting params from params handler
  SuccessorList = successorlist,
  Successor = successor,
  Predecessor = predecessor,
  NBits = nbits,

  RestartStrategy = rest_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  Son1 = {stabilizer, {stabilizer, start_link, [SuccessorList, NBits, Successor]},
    Restart, Shutdown, worker, [stabilizer]},
  Son2 = {killer, {killer, start_link, []},      %%TODO make killer process
    Restart, Shutdown, worker, [killer]},
  Son3 = {internal_routing_supervisor, {internal_routing_supervisor, start_link, [Predecessor, NBits]},
    Restart, Shutdown, supervisor, [internal_routing_supervisor]},

  {ok, {SupFlags, [Son1, Son2, Son3]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
