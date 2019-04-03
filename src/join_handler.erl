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
  look_response/0,
  info/4,
  abort/1,
  ack_join/1,
  ready_for_info/1,
  no_priority/1,
  leave_info/2,
  ack_info/1,
  ack_leave/1,
  create/0]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

%% state-callback
-export([
  init_joiner/3,
  look/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 10000).

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

look_response() ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {look_resp}).

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

no_priority(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {no_priority,Address}).

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
  ok = handle(init_statem),                                    %TODO it is for testing, eliminate it
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
state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

init_joiner({call, From}, join, _State) ->
  Reply = {reply, From, ok},
  ok = handle(init_joiner),                                    %TODO it is for testing, eliminate it
  {next_state, look, _State, [Reply]};
init_joiner({call, From}, create, _State) ->
  Reply = {reply, From, ok},
  ok = handle(init_joiner),                                    %TODO it is for testing, eliminate it
  {next_state, look, _State, [Reply]};
init_joiner(EventType, EventContent, #session{}=State) ->
  handle_generic_event({EventType, EventContent, State});
%TODO this init joiner is only for test, because cast is NEVER done. After checking delete the following init_joiner
init_joiner(cast, look_resp, #session{}=State) ->
  io:format("ascacsajcnasjn~n",[]),
  ok = handle(look),                                    %TODO it is for testing, eliminate it
  {next_stat, pre_join, State, [{state_timeout, ?INTERVAL, hard_stop}]}.

look(cast, look_resp, _State) ->
  ok = handle(look),                                    %TODO it is for testing, eliminate it
  {next_stat, pre_join, _State, [{state_timeout, ?INTERVAL, hard_stop}]};
look(state_timeout, hard_stop, _State) ->
  ok = handle(look),                                    %TODO it is for testing, eliminate it
  {stop, waiting_timed_out, _State};
look(EventType, EventContent, #session{}=State) ->
  ok = handle(look),                                    %TODO it is for testing, eliminate it
  handle_generic_event({EventType, EventContent, State}).



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


handle(init_statem) ->
  io:format(user, "*** Start statem~n", []),
  ok;
handle(look) ->
  io:format(user, "*** State look~n", []),
  ok;
handle(init_joier) ->
  io:format(user, "*** State initJoiner~n", []),
  ok.

handle_generic_event({_, _, Session}) ->
  {keep_state, Session}.