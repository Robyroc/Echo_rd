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
  create/0]).

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

-record(session, {address, id}).

%%%===================================================================
%%% API
%%%===================================================================

create() ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, {create}).

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
start_link(ProviderAddress) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [ProviderAddress], []).



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
init([ProviderAddress]) ->
  naming_handler:notify_identity(self(), join_handler),
  ok = handle(init_joiner),                                    %TODO it is for testing, eliminate it
  {ok, init_joiner, #session{address = ProviderAddress, id = not_used}, []}.     %TODO check the 5th parameter


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
  %handle_event_function.       %TODO decide callback_mode

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
%TODO add respnse to AM in case of timeout {reply, From in session, fail}
init_joiner({call, From}, {join, Address}, Session) ->
  Reply = noreply,
  %%save From in Session
  %%save address (request's address)
  ok = handle(init_joiner),
  communication_manager:send_message(lookup_for_join, [], Address, no_alias),
  {next_state, look, Session, [Reply]};

init_joiner({call, From}, create, Session) ->
  Reply = {reply, From, ok},
  ok = handle(init_joiner),
  %TODO start
  {next_state, init_provider, Session, [Reply]};

init_joiner(EventType, EventContent, Session) ->
  handle_generic_event({EventType, EventContent, Session}).


look(cast, {look_resp,Address}, Session) ->
  ok = handle(look),
  %Addres of successor in session
  communication_manager:send_message(ready_for_info, [], Address, no_alias),
  {next_state, pre_join, Session, [{state_timeout, ?INTERVAL, hard_stop}]};

look(state_timeout, hard_stop, Session) ->
  ok = handle(look),
  {next_state, init_joiner, Session};

look(EventType, EventContent, Session) ->
  ok = handle(look),
  handle_generic_event({EventType, EventContent, Session}).


pre_join(cast, {info,Address, Res, Succ, Nbits}, Session) ->
  ok = handle(pre_join),
  %check if Address in Session = Address, otherwise keepState
  %TODO save in Session Res, Succ, Nbits
  communication_manager:send_message(ack_info, [], Address, no_alias),
  {next_state, j_ready, Session, [{state_timeout, ?INTERVAL, hard_stop}]};

pre_join(cast, {abort, Reason}, Session) ->
  ok = handle(pre_join),
  io:format("Reason of abort: ~p~n", [Reason]),
  %address for l4j = Address in session of the initial request
  communication_manager:send_message(lookup_for_join, [], request_address, no_alias),
  {next_state, look, Session, [{state_timeout, ?INTERVAL, hard_stop}]};

pre_join(state_timeout, hard_stop, Session) ->
  ok = handle(pre_join),
  {next_state, init_joiner, Session};

pre_join(EventType, EventContent, Session) ->
  ok = handle(pre_join),
  handle_generic_event({EventType, EventContent, Session}).


j_ready(cast, {ack_join, Address}, Session) ->
  ok = handle(j_ready),
  %TODO manage send the start message
  {next_state, init_provider, Session, [{state_timeout, ?INTERVAL, hard_stop}]};

j_ready(cast, {abort, Reason}, Session) ->
  ok = handle(j_ready),
  %TODO manage Reason (see above)
  communication_manager:send_message(lookup_for_join, [], _Address, no_alias),
  {next_state, pre_join, Session, [{state_timeout, ?INTERVAL, hard_stop}]};

j_ready(state_timeout, hard_stop, Session) ->
  ok = handle(j_ready),
  {next_state, init_joiner, Session};

j_ready(EventType, EventContent, Session) ->
  ok = handle(j_ready),
  handle_generic_event({EventType, EventContent, Session}).


init_provider(cast, {ready_for_info, Address}, Session) ->
  ok = handle(init_provider),
  PredecessorID = hash_f:get_hashed_addr(checker:get_pred(local_address)),
  JoinerID = hash_f:get_hashed_addr(Address),
  case JoinerID of
    _ when JoinerID =< PredecessorID ->
      communication_manager:send_message(abort, ["Not updated"],Address, no_alias),
      {keep_state, Session};
    _ when JoinerID > PredecessorID ->
      DataInfo={res,stabilizer:get_successor_list(),params_handler:get_param(nbits)},   %%TODO Res from AM
      communication_manager:send_message(join_info,DataInfo,Address,no_alias),
      %save currID, currAddress in session
      {next_state, not_alone, Session}
  end;

init_provider(cast, {leave_info,Resources, Address}, Session) ->
  ok = handle(init_provider),
  %TODO send Resources to the AM
  communication_manager:send_message(leave_ack,[],Address,no_alias),
  {keep_state, Session};

init_provider(cast, leave, Session) ->
  ok = handle(init_provider),
  %TODO receive resources from AM
  communication_manager:send_message(leave_info,resource,address_succ,no_alias),
  {next_state, leaving, Session};

init_provider(EventType, EventContent, Session) ->
  ok = handle(init_provider),
  handle_generic_event({EventType, EventContent, Session}).


not_alone(cast, {ready_for_info, Address}, Session) ->
  ok = handle(not_alone),
  %CURR ID from session
  CurrID = Session#session.id,
  JoinerID = hash_f:get_hashed_addr(Address),
  case JoinerID of
    _ when JoinerID =< CurrID ->
      communication_manager:send_message(abort, ["No priority"],Address, no_alias),
      {keep_state, Session, [{state_timeout, ?INTERVAL, hard_stop}]};
    _ when JoinerID > CurrID ->
      communication_manager:send_message(abort, ["Loss priority"],curr_address, no_alias),
      DataInfo={res,succ,nbits},
      communication_manager:send_message(join_info,DataInfo,Address,no_alias),
      {keep_state, Session, [{state_timeout, ?INTERVAL, hard_stop}]}
  end;

%TODO all the leave must be call
not_alone(cast, leave, Session) ->
  ok = handle(not_alone),
  communication_manager:send_message(abort, ["Successor is leaving"],address_curr, no_alias),
  communication_manager:send_message(leave_info,resource,stabilizer:get_successor(),no_alias),
  {next_state, leaving, Session, [{state_timeout, ?INTERVAL, hard_stop}]};

not_alone(cast, {ack_info,Address}, Session) ->
  ok = handle(not_alone),
  communication_manager:send_message(ack_join,[],Address,no_alias),
  %TODO manage drop to the AM
  {next_state, init_provider, Session, [{state_timeout, ?INTERVAL, hard_stop}]};

not_alone(cast, {leave_info,Resources, Address}, Session) ->
  ok = handle(not_alone),
  %%abort(curr), ack_leave
  %chech if ADDRRESS = checker:get pred
  communication_manager:send_message(abort,["REASON"],address_curr,no_alias),
  %TODO send resources to the AM (when ok -> leave_ack)
  communication_manager:send_message(leave_ack,[],Address,no_alias),
  {next_state, init_provider, Session, [{state_timeout, ?INTERVAL, hard_stop}]};

not_alone(state_timeout, hard_stop, Session) ->
  ok = handle(not_alone),
  {next_state, init_provider, Session};

not_alone(EventType, EventContent, Session) ->
  ok = handle(not_alone),
  handle_generic_event({EventType, EventContent, Session}).


leaving(cast, {ack_leave, Address}, Session) ->
  ok = handle(leaving),
  %%TODO if Address = succ_address, then send drop to the AM and kill(self())
  {next_state, init_joiner, Session, [{state_timeout, ?INTERVAL_LEAVING, hard_stop}]};

leaving(state_timeout, hard_stop, Session) ->
  ok = handle(leaving),
  {next_state, init_joiner, Session};

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


start() ->      %TODO send params to params_handler and res to AM and wait for hash_f
  ok.