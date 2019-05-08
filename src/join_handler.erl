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
  join/2,
  leave/0,
  look_response/1,
  info/4,
  abort/1,
  ack_join/1,
  ready_for_info/1,
  leave_info/2,
  ack_info/1,
  ack_leave/1,
  create/2]).

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

-define(SLEEP_INTERVAL, 1000).
-define(SERVER, ?MODULE).
-define(INTERVAL, 10000).
-define(INTERVAL_LEAVING, 20000).
-define(INTERVAL_JOIN, 20000).

-record(session, {provider_addr, succ_addr, res, succ_list, nbits, app_mngr, curr_addr, curr_id, superv, stabilizer}).

%%%===================================================================
%%% API
%%%===================================================================

create(OwnPort, Nbits) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, {create, OwnPort, Nbits}).

join(OwnPort, Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, {join, OwnPort, Address}).

leave() ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, leave).

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
  ok = handle(init,init_joiner),
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
init_joiner({call, From}, {join, OwnPort, Address}, Session) ->
  ok = handle(init_joiner, look),
  case socket_listener:check_availability(OwnPort) of
    ok ->
      naming_handler:notify_identity(OwnPort, port),
      naming_handler:wait_service(listener),
      Answer = communication_manager:send_message_sync(lookup_for_join, [], Address, no_alias),
      case Answer of
        ok ->
          {next_state, look, Session#session{app_mngr = From, provider_addr = Address},
            [{state_timeout, ?INTERVAL, hard_stop}]};
        Error ->
          link_shutdown(),
          {keep_state, Session, [{reply, From, Error}]}
      end;
    Error ->
      {keep_state, Session, [{reply, From, Error}]}
  end;

init_joiner({call, From}, {create, OwnPort, Nbits}, Session) ->
  ok = handle(init_joiner, init_provider),
  case socket_listener:check_availability(OwnPort) of
    ok ->
      naming_handler:notify_identity(OwnPort, port),
      naming_handler:wait_service(listener),
      NewSession = Session#session{nbits = Nbits, succ_list = [], succ_addr = link_manager:get_own_address(),
        res = [], app_mngr = From},
      ok = start(NewSession),
      {next_state, init_provider, NewSession, []};
    Error ->
      {keep_state, Session, [{reply, From, Error}]}
  end;

init_joiner(EventType, EventContent, Session) ->
  handle(init_joiner, init_joiner),
  handle_generic_event({EventType, EventContent, Session}).


look(cast, {look_resp,Address}, Session) ->
  ok = handle(look, pre_join),
  communication_manager:send_message_async(ready_for_info, [], Address, no_alias),
  {next_state, pre_join, Session#session{succ_addr = Address}, [{state_timeout, ?INTERVAL, hard_stop}]};

look(state_timeout, hard_stop, Session) ->
  ok = handle(look, init_joiner),
  gen_statem:reply(Session#session.app_mngr, fail),
  {next_state, init_joiner, reset_session(Session)};

look({call, _From}, {join, _Port, _Address}, Session) ->
  {keep_state, Session};

look(EventType, EventContent, Session) ->
  ok = handle(look, look),
  handle_generic_event({EventType, EventContent, Session}).

pre_join(cast, {info,Address, Res, Succ, Nbits}, Session) ->
  ok = handle(pre_join, j_ready),
  SuccAddr = Session#session.succ_addr,
  case Address of
    _ when Address =:= SuccAddr ->
      communication_manager:send_message_async(ack_info, [], Address, no_alias),
      {next_state, j_ready, Session#session{res = Res, succ_list = Succ, nbits = Nbits}, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]};
    _ -> {keep_state, Session, [{state_timeout, ?INTERVAL, hard_stop}]}
  end;

pre_join(cast, {abort, Reason}, Session) ->
  ok = handle(pre_join, look),
  %joinerLager:error(" -- JOIN ABORTED -- Reason of abort: ~p~n", [Reason]),
  lager:error(" -- JOIN ABORTED -- Reason of abort: ~p~n", [Reason]),
  ProviderAddr = Session#session.provider_addr,
  timer:sleep(?SLEEP_INTERVAL),
  communication_manager:send_message_async(lookup_for_join, [], ProviderAddr, no_alias),
  {next_state, look, soft_reset_session(Session), [{state_timeout, ?INTERVAL, hard_stop}]};

pre_join(state_timeout, hard_stop, Session) ->
  ok = handle(pre_join, init_joiner),
  gen_statem:reply(Session#session.app_mngr, fail),
  {next_state, init_joiner, reset_session(Session)};

pre_join(EventType, EventContent, Session) ->
  ok = handle(pre_join, pre_join),
  handle_generic_event({EventType, EventContent, Session}).


j_ready(cast, {ack_join, _Address}, Session) ->
  ok = handle(j_ready, init_provider),
  ok = start(Session),
  {next_state, init_provider, Session};

j_ready(cast, {abort, Reason}, Session) ->
  ok = handle(j_ready, look),
  %joinerLager:error("Reason of abort: ~p~n", [Reason]),
  lager:error("Reason of abort: ~p~n", [Reason]),
  ProviderAddr = Session#session.provider_addr,
  timer:sleep(?SLEEP_INTERVAL),
  communication_manager:send_message_async(lookup_for_join, [], ProviderAddr, no_alias),
  {next_state, look, soft_reset_session(Session), [{state_timeout, ?INTERVAL, hard_stop}]};

j_ready(state_timeout, hard_stop, Session) ->
  ok = handle(j_ready, init_joiner),
  gen_statem:reply(Session#session.app_mngr, fail),
  {next_state, init_joiner, reset_session(Session)};

j_ready(EventType, EventContent, Session) ->
  ok = handle(j_ready, j_ready),
  handle_generic_event({EventType, EventContent, Session}).


init_provider(cast, {ready_for_info, Address}, Session) ->
  PredecessorID = checker:get_pred_id(),
  JoinerID = adjust_predecessor(hash_f:get_hashed_addr(Address), hash_f:get_hashed_addr(link_manager:get_own_address()), Session#session.nbits),
  case JoinerID of
    _ when JoinerID =< PredecessorID ->
      communication_manager:send_message_async(abort, ["Not updated"],Address, no_alias),
      handle(init_provider, init_provider),
      {keep_state, Session};
    _ when JoinerID > PredecessorID ->
      DataInfo=[params_handler:get_param(nbits), stabilizer:get_successor_list(),
        application_manager:get_local_resources(JoinerID)],
      communication_manager:send_message_async(join_info,DataInfo,Address,no_alias),
      handle(init_provider, not_alone),
      {next_state, not_alone, Session#session{curr_id = JoinerID, curr_addr = Address}, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]}
  end;

init_provider(cast, {leave_info,Resources, Address}, Session) ->
  ok = handle(init_provider, init_provider),
  application_manager:add_many_resources(Resources),
  communication_manager:send_message_async(ack_leave,[],Address,no_alias),
  {keep_state, Session};

init_provider({call,From}, leave, Session) ->
  ok = handle(init_provider, leaving),
  Reply = postpone,
  Res = application_manager:get_local_resources(all_res),
  {_, Successor} = stabilizer:get_successor(),
  communication_manager:send_message_async(leave_info, Res, Successor, no_alias),
  {next_state, leaving, Session#session{app_mngr = From}, [{state_timeout, ?INTERVAL_LEAVING, hard_stop}, Reply]};

init_provider(cast, {look_resp,_Address}, Session) ->
  {keep_state, Session};

init_provider(EventType, EventContent, Session) ->
  ok = handle(init_provider, init_provider),
  handle_generic_event({EventType, EventContent, Session}).


not_alone(cast, {ready_for_info, Address}, Session) ->
  ok = handle(not_alone, not_alone),
  CurrID = Session#session.curr_id,
  JoinerID = hash_f:get_hashed_addr(Address),
  case JoinerID of
    _ when JoinerID =< CurrID ->
      communication_manager:send_message_async(abort, ["No priority"],Address, no_alias),
      {keep_state, Session, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]};
    _ when JoinerID > CurrID ->
      CurrAddr = Session#session.curr_addr,
      communication_manager:send_message_async(abort, ["Loss priority"],CurrAddr, no_alias),
      DataInfo = [Session#session.nbits, Session#session.succ_list, Session#session.res],
      communication_manager:send_message_async(join_info, DataInfo, Address, no_alias),
      {keep_state, Session, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]}
  end;


not_alone({call,From}, leave, Session) ->
  Reply = postpone,
  ok = handle(not_alone, leaving),
  communication_manager:send_message_async(abort, ["Successor is leaving"], Session#session.curr_addr, no_alias),
  {_, Successor} = stabilizer:get_successor(),
  communication_manager:send_message_async(leave_info, application_manager:get_local_resources(all_res), Successor, no_alias),
  {next_state, leaving, Session#session{app_mngr = From}, [{state_timeout, ?INTERVAL_LEAVING, hard_stop}, Reply]};

not_alone(cast, {ack_info,Address}, Session) ->
  ok = handle(not_alone, init_provider),
  communication_manager:send_message_async(ack_join, [], Address, no_alias),
  application_manager:drop_many_resources(Session#session.curr_id),
  {next_state, init_provider, reset_provider_session(Session)};

not_alone(cast, {leave_info,Resources, Address}, Session) ->
  ok = handle(not_alone, init_provider),
  communication_manager:send_message_async(abort,["Another leave"], Session#session.curr_addr, no_alias),
  PredecessorAddr = checker:get_pred(Session#session.provider_addr),
  case Address of
    _ when Address =:= PredecessorAddr ->
      communication_manager:send_message_async(ack_leave,[], Address, no_alias),
      application_manager:add_many_resources(Resources),
      {next_state, init_provider, reset_provider_session(Session)};
    _ -> {keep_state, Session, [{state_timeout, ?INTERVAL_JOIN, hard_stop}]}
  end;

not_alone(state_timeout, hard_stop, Session) ->
  ok = handle(not_alone, init_provider),
  {next_state, init_provider, reset_provider_session(Session)};

not_alone(cast, {look_resp,_Address}, Session) ->
  {keep_state, Session};

not_alone(EventType, EventContent, Session) ->
  ok = handle(not_alone, not_alone),
  handle_generic_event({EventType, EventContent, Session}).


leaving(cast, {ack_leave, Address}, Session) ->
  ok = handle(leaving, init_joiner),
  stop(Session, Address);

leaving(cast, {ready_for_info, Address}, Session) ->
  communication_manager:send_message_async(abort, ["Successor is leaving"], Address, no_alias),
  {keep_state, Session};

leaving(state_timeout, hard_stop, Session) ->
  ok = handle(leaving, init_joiner),
  {_, SuccAddress} = stabilizer:get_successor(),
  stop(Session, SuccAddress),
  {next_state, init_joiner, reset_session(Session)};

leaving({call, _From}, leave, Session) ->
  {keep_state, Session};

leaving(cast, {look_resp,_Address}, Session) ->
  {keep_state, Session};

leaving(EventType, EventContent, Session) ->
  ok = handle(leaving, leaving),
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

handle(From, To) ->
  case logging_policies:check_policy(?MODULE) of
    able ->
      lagerConsole:info("+++ JOINER +++ ~p ---> ~p +++~n", [From, To]),
      joinerLager:info("+++ JOINER +++ ~p ---> ~p +++~n", [From, To]);
    able_lager -> joinerLager:info("+++ JOINER +++ ~p ---> ~p +++~n", [From, To]);
    unable -> ok
  end.

handle_generic_event({EventType, EventContent, Session}) ->
  %joinerLager:error("Event abnormal: ~p | ~p~n", [EventType, EventContent]),
  lager:error("Event abnormal: ~p | ~p~n", [EventType, EventContent]),
  {keep_state, Session}.


start(Session) ->
  case Session#session.stabilizer of
    undefined -> ok;
    Pid -> stabilizer:turn_on(Pid)
  end,
  #session{nbits = Nbits, succ_list = SuccList, succ_addr = SuccAddr, res = Resources,
    superv = Supervisor, app_mngr = AM} = Session,
  ParamsHandler = {params_handler, {params_handler, start_link, [SuccAddr, SuccList, Nbits]},
    temporary, 2000, worker, [params_handler]},
  supervisor:start_child(Supervisor, ParamsHandler),
  naming_handler:wait_service(router),
  case Resources of
    [] -> ok;
    _ -> application_manager:add_many_resources(Resources)
  end,
  gen_statem:reply(AM, ok).

reset_session(Session) ->
  Session#session{provider_addr = undefined, succ_addr = undefined, res = undefined,
    succ_list = undefined, nbits = undefined, app_mngr = undefined, curr_addr = undefined, curr_id = undefined}.

soft_reset_session(Session) ->
  Session#session{
    succ_addr = undefined,
    res = undefined,
    succ_list = undefined,
    nbits = undefined,
    curr_addr = undefined,
    curr_id = undefined}.

reset_provider_session(Session) ->
  Session#session{curr_addr = undefined, curr_id = undefined}.

stop(Session, Address) ->
  {_, SuccessorAddress} = stabilizer:get_successor(),
  case Address of
    _ when Address =:= SuccessorAddress ->
      gen_statem:reply(Session#session.app_mngr, ok),
      Stab = naming_handler:get_identity(stabilizer),
      application_manager:drop_many_resources(all_res),
      exit(naming_handler:get_identity(communication_supervisor), kill),
      stabilizer:turn_off(),
      naming_handler:delete_comm_tree(),
      {next_state, init_joiner, reset_session(Session#session{stabilizer = Stab})};
    _ -> {keep_state, Session, [{state_timeout, ?INTERVAL_LEAVING, hard_stop}]}
  end.

link_shutdown() ->
  Pid = naming_handler:get_identity(link_supervisor),
  naming_handler:delete_comm_tree(),
  exit(Pid, kill).

adjust_predecessor(ID, OwnId, _NBits) when ID < OwnId -> ID;
adjust_predecessor(ID, OwnId, NBits) -> adjust_predecessor(ID - round(math:pow(2, NBits)), OwnId, NBits).