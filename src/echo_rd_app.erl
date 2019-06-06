%%%-------------------------------------------------------------------
%% @doc echo_rd public API
%% @end
%%%-------------------------------------------------------------------

-module(echo_rd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  logger:remove_handler(default),
  root_supervisor:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

