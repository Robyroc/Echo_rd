-module(params_handler).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/3, get_param/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {successor, succ_list, nbits}).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Successor, SuccessorList, NBits) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Successor, SuccessorList, NBits], []).

get_param(Param) ->
  PID = naming_handler:get_identity(params_handler),
  gen_server:call(PID, {get_param, Param}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Successor, SuccessorList, NBits]) ->
  communication_manager:receive_nbits(NBits),
  naming_handler:notify_identity(self(), params_handler),
  {ok, #state{successor = Successor, succ_list = SuccessorList, nbits = NBits}}.


handle_call({get_param, successor}, _From, State) ->
  {reply, State#state.successor, State};

handle_call({get_param, succ_list}, _From, State) ->
  {reply, State#state.succ_list, State};

handle_call({get_param, nbits}, _From, State) ->
  {reply, State#state.nbits, State};

handle_call({get_param, Invalid}, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("PARAMS HANDLER: Invalid Parameter has been requested: ~p\n", [Invalid]);
    {lager_only, _} ->
      lager:error("PARAMS HANDLER: Invalid Parameter has been requested: ~p\n", [Invalid]);
    {lager_off, _} ->
      io:format("PARAMS HANDLER: Invalid Parameter has been requested: ~p\n", [Invalid]);
    _ -> ok
  end,
  {reply, ok, State};

handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("PARAMS HANDLER: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("PARAMS HANDLER: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("PARAMS HANDLER: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("PARAMS HANDLER: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("PARAMS HANDLER: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("PARAMS HANDLER: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.


handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("PARAMS HANDLER: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("PARAMS HANDLER: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("PARAMS HANDLER: Unexpected ! message: ~p\n", [Info]);
    _ -> ok
  end,
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

