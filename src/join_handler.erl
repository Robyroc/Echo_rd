%%%-------------------------------------------------------------------
%%% @author Antonio
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. mar 2019 13:10
%%%-------------------------------------------------------------------
-module(join_handler).
-author("Antonio").

-behaviour(gen_statem).

%% API
-export([
  start_link/1,
  join/1,
  leave/0,
  look_response/1,
  info/4,
  abort/1,
  ack_join/1,
  ready_for_info/1,
  leave_info/2,
  ack_info/1,
  ack_leave/1,
  create/1]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

%% state-callback
-export([
  init_joiner/3,
  look/3, pre_join/3, j_ready/3, init_provider/3, not_alone/3, leaving/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 10000).
-define(INTERVAL_LEAVING, 30000).
-define(INTERVAL_JOIN, 50000).

-record(session, {provider_addr, succ_addr, res, succ_list, nbits, app_mngr, curr_addr, curr_id, superv}).

%%%===================================================================
%%% API
%%%===================================================================

create(Nbits) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, {create, Nbits}).

join(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, {join,Address}).

leave() ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, {leave}).

look_response(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {look_resp, Address}).

info(Address, Res, Succ, Nbits) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {info,Address, Res, Succ, Nbits}).

abort(Reason) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {abort,Reason}).

ack_join(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {ack_join, Address}).

ready_for_info(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {ready_for_info,Address}).

leave_info(Resources, Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {leave_info,Resources, Address}).

ack_info(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {ack_info,Address}).

ack_leave(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {ack_leave, Address}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Pid) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [Pid], []).



%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Pid]) ->
  naming_handler:notify_identity(self(), join_handler),
  ok = handle(init_joiner),
  {ok, init_joiner, #session{provider_addr = undefined, succ_addr = undefined,
    res = undefined, succ_list = undefined, nbits = undefined, app_mngr = undefined,
    curr_addr = undefined, curr_id = undefined, superv = Pid}, []};

init(_) -> badarg.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
  state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
%TODO all the handle() is for testing, it has to be eliminated
init_joiner({call, From}, {join, Address}, Session) ->
  Reply = postpone,
  ok = handle(init_joiner),
  communication_manager:send_message(lookup_for_join, [], Address, no_alias),
  {next_state, look, Session#session{app_mngr = From, provider_addr = Address},
    [{state_timeout, ?INTERVAL, hard_stop}, Reply]};

init_joiner({call, From}, {create, Nbits}, Session) ->
  ok = handle(init_joiner),
  NewSession = Session#session{nbits = Nbits, succ_list = [], succ_addr = link_manager:get_own_address(),
    res = [], app_mngr = From},
  ok = start(NewSession),
  {next_state, init_provider, NewSession, []};

init_joiner(EventType, EventContent, Session) ->
  handle_generic_event({EventType, EventContent, Session}).


look(cast, {look_resp,Address}, Session) ->
  ok = handle(look),
  communication_manager:send_message(ready_for_info, [], Address, no_alias),
  {next_state, pre_join, Session#session{succ_addr = Address}, [{state_timeout, ?INTERVAL, hard_stop}]};

look(state_timeout, hard_stop, Session) ->
  ok = handle(look),
  gen_statem:reply(Session#session.app_mngr, fail),
  {next_state, init_joiner, reset_session(Session)};

look(EventType, EventContent, Session) ->
  ok = handle(look),
  handle_generic_event({EventType, EventContent, Session}).


pre_join(cast, {info,Address, Res, Succ, Nbits}, Session) ->
  ok = handle(pre_join),
  ProviderAddr = Session#session.provider_addr,
  case Address of
    _ when Address =:= ProviderAddr ->
      communication_manager:send_message(ack_info, [], Address, no_alias),
      {next_state, j_ready, Session#session{res = Res, succ_list = Succ, nbits = Nbits}, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]};
    _ -> {keep_state, Session, [{state_timeout, ?INTERVAL, hard_stop}]}
  end;

pre_join(cast, {abort, Reason}, Session) ->
  ok = handle(pre_join),
  io:format("Reason of abort: ~p~n", [Reason]),
  ProviderAddr = Session#session.provider_addr,
  communication_manager:send_message(lookup_for_join, [], ProviderAddr, no_alias),
  {next_state, look, reset_session(Session), [{state_timeout, ?INTERVAL, hard_stop}]};

pre_join(state_timeout, hard_stop, Session) ->
  ok = handle(pre_join),
  gen_statem:reply(Session#session.app_mngr, fail),
  {next_state, init_joiner, reset_session(Session)};

pre_join(EventType, EventContent, Session) ->
  ok = handle(pre_join),
  handle_generic_event({EventType, EventContent, Session}).


j_ready(cast, {ack_join, _Address}, Session) ->
  ok = handle(j_ready),
  ok = start(Session),
  {next_state, init_provider, Session};

j_ready(cast, {abort, Reason}, Session) ->
  ok = handle(j_ready),
  io:format("Reason of abort: ~p~n", [Reason]),
  ProviderAddr = Session#session.provider_addr,
  communication_manager:send_message(lookup_for_join, [], ProviderAddr, no_alias),
  {next_state, look, reset_session(Session), [{state_timeout, ?INTERVAL, hard_stop}]};

j_ready(state_timeout, hard_stop, Session) ->
  ok = handle(j_ready),
  gen_statem:reply(Session#session.app_mngr, fail),
  {next_state, init_joiner, reset_session(Session)};

j_ready(EventType, EventContent, Session) ->
  ok = handle(j_ready),
  handle_generic_event({EventType, EventContent, Session}).


init_provider(cast, {ready_for_info, Address}, Session) ->
  ok = handle(init_provider),
  PredecessorID = hash_f:get_hashed_addr(checker:get_pred(local_address)),
  JoinerID = hash_f:get_hashed_addr(Address),
  case JoinerID of
    _ when JoinerID =< PredecessorID ->
      io:format("JH: here"),
      communication_manager:send_message(abort, ["Not updated"],Address, no_alias),
      {keep_state, Session};
    _ when JoinerID > PredecessorID ->
      io:format("JH: there"),
      DataInfo={params_handler:get_param(nbits), stabilizer:get_successor_list(),
        application_manager:get_local_resources()},
      ok = communication_manager:send_message(join_info,DataInfo,Address,no_alias),   %TODO check this line
      {next_state, not_alone, Session#session{curr_id = JoinerID, curr_addr = Address}, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]}
  end;

init_provider(cast, {leave_info,Resources, Address}, Session) ->
  ok = handle(init_provider),
  application_manager:add_many_resources(Resources),
  communication_manager:send_message(leave_ack,[],Address,no_alias),
  {keep_state, Session};

init_provider({call,From}, leave, Session) ->
  ok = handle(init_provider),
  Reply = postpone,
  application_manager:get_local_resources(),
  SuccAddr = Session#session.succ_addr,
  communication_manager:send_message(leave_info,resource, SuccAddr, no_alias),
  {next_state, leaving, Session#session{app_mngr = From}, [{state_timeout, ?INTERVAL_LEAVING, hard_stop}, Reply]};

init_provider(EventType, EventContent, Session) ->
  ok = handle(init_provider),
  handle_generic_event({EventType, EventContent, Session}).


not_alone(cast, {ready_for_info, Address}, Session) ->
  ok = handle(not_alone),
  CurrID = Session#session.curr_id,
  JoinerID = hash_f:get_hashed_addr(Address),
  case JoinerID of
    _ when JoinerID =< CurrID ->
      communication_manager:send_message(abort, ["No priority"],Address, no_alias),
      {keep_state, Session, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]};
    _ when JoinerID > CurrID ->
      CurrAddr = Session#session.curr_addr,
      communication_manager:send_message(abort, ["Loss priority"],CurrAddr, no_alias),
      DataInfo = {Session#session.nbits, Session#session.succ_list, Session#session.res},
      communication_manager:send_message(join_info, DataInfo, Address, no_alias),
      {keep_state, Session, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]}
  end;


not_alone({call,From}, leave, Session) ->
  Reply = postpone,
  ok = handle(not_alone),
  communication_manager:send_message(abort, ["Successor is leaving"], Session#session.curr_addr, no_alias),
  communication_manager:send_message(leave_info,application_manager:get_local_resources(),stabilizer:get_successor(),no_alias),
  {next_state, leaving, Session#session{app_mngr = From}, [{state_timeout, ?INTERVAL_LEAVING, hard_stop}, Reply]};

not_alone(cast, {ack_info,Address}, Session) ->
  ok = handle(not_alone),
  communication_manager:send_message(ack_join, [], Address, no_alias),
  application_manager:drop_many_resources(Session#session.curr_id),
  {next_state, init_provider, reset_provider_session(Session)};

not_alone(cast, {leave_info,Resources, Address}, Session) ->
  ok = handle(not_alone),
  communication_manager:send_message(abort,["REASON"], Session#session.curr_addr, no_alias),
  PredecessorAddr = checker:get_pred(Session#session.provider_addr),
  case Address of
    _ when Address =:= PredecessorAddr ->
      communication_manager:send_message(leave_ack,[], Address, no_alias),
      application_manager:add_many_resources(Resources),
      {next_state, init_provider, reset_provider_session(Session)};
    _ -> {keep_state, Session, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]}
  end;

not_alone(state_timeout, hard_stop, Session) ->
  ok = handle(not_alone),
  {next_state, init_provider, reset_provider_session(Session)};

not_alone(EventType, EventContent, Session) ->
  ok = handle(not_alone),
  handle_generic_event({EventType, EventContent, Session}).


leaving(cast, {ack_leave, Address}, Session) ->
  ok = handle(leaving),
  SuccessorAddr = Session#session.succ_addr,
  case Address of
    _ when Address =:= SuccessorAddr ->
      gen_statem:reply(Session#session.app_mngr, ok),
      application_manager:drop_many_resources(all_res),
      exit(naming_handler:get_identity(communication_supervisor), kill),
      {next_state, init_joiner, reset_session(Session)};
    _ -> {keep_state, Session, [{state_timeout, ?INTERVAL_LEAVING, hard_stop}]}
  end;

leaving(state_timeout, hard_stop, Session) ->
  ok = handle(leaving),
  {next_state, init_joiner, reset_session(Session)};

leaving(EventType, EventContent, Session) ->
  ok = handle(leaving),
  handle_generic_event({EventType, EventContent, Session}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


handle(init_joiner) ->
  io:format(user, "*** State initJoiner~n", []),
  ok;
handle(look) ->
  io:format(user, "*** State look~n", []),
  ok;
handle(pre_join) ->
  io:format(user, "*** State preJoin~n", []),
  ok;
handle(j_ready) ->
  io:format(user, "*** State jReady~n", []),
  ok;
handle(init_provider) ->
  io:format(user, "*** State initProvider~n", []),
  ok;
handle(not_alone) ->
  io:format(user, "*** State notAlone~n", []),
  ok;
handle(leaving) ->
  io:format(user, "*** State Leaving~n", []),
  ok.


handle_generic_event({_, _, Session}) ->
  {keep_state, Session}.


start(Session) ->
  #session{nbits = Nbits, succ_list = SuccList, succ_addr = SuccAddr, res = Resources,
    superv = Supervisor, app_mngr = AM} = Session,
  ParamsHandler = {params_handler, {params_handler, start_link, [SuccAddr, SuccList, Nbits]},
    permanent, 2000, worker, [params_handler]},
  supervisor:start_child(Supervisor, ParamsHandler),
  application_manager:add_many_resources(Resources),
  naming_handler:wait_service(hash_f),
  gen_statem:reply(AM, ok).

reset_session(Session) ->
  Session#session{provider_addr = undefined, succ_addr = undefined, res = undefined,
    succ_list = undefined, nbits = undefined, app_mngr = undefined, curr_addr = undefined, curr_id = undefined}.

reset_provider_session(Session) ->
  Session#session{curr_addr = undefined, curr_id = undefined}.