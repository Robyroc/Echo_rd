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
-export([start_link/1, join/1, leave/0, look_response/1, info/4, abort/1, ack_join/0, ready_for_info/1, no_priority/1, leave_info/1, ack_info/1, ack_leave/0]).

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

-define(SERVER, ?MODULE).

-record(state, {address, id}).

%%%===================================================================
%%% API
%%%===================================================================

create() ->
  gen_statem:call().

join(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, {join,Address}).

leave() ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:call(PID, {leave}).

look_response(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {look_response,Address}).

info(Address, Res, Succ, Nbits) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {info,Address, Res, Succ, Nbits}).

abort(Reason) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {abort,Reason}).

ack_join() ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {ack_join}).

ready_for_info(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {ready_for_info,Address}).

no_priority(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {no_priority,Address}).

leave_info(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {leave_info,Address}).

ack_info(Address) ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {ack_info,Address}).

ack_leave() ->
  PID = naming_handler:get_identity(join_handler),
  gen_statem:cast(PID, {ack_leave}).


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
init([ProvideAddress]) ->
  naming_handler:notify_identity(self(), join_handler),
  {ok, joiner_init, #state{address = ProvideAddress, id = not_used}}.


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
  %state_functions.
  handle_event_function.       %TODO decide callback_mode

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
