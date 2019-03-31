-module(communication_manager).
-author("Antonio").

-behaviour(gen_server).

%% API
-export([start_link/0, send_message/4, receive_message/1, create/1, check_params/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%TODO remove comment, it is just for testing
%communication_manager:send_message(lookup,["K20"],{6543,{192,168,1,98}},no_alias).
send_message(Method, Params, Address, Alias) ->
  PID = naming_service:get_identity(communication_manager),
  gen_server:call(PID, {send_msg, Method, Params, Address, Alias}).


receive_message({Method, Address, Params}) ->
  PID = naming_service:get_identity(communication_manager),
  gen_server:call(PID, {rcv_msg, Method, Address, Params}).

%TODO check if create and join have to be here
create(NBits) ->
  ok.
join(Address) -> ok.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------


init([]) ->
  %naming_service:wait_service(),           %TODO check if the CM has to wait for some service
  naming_service:notify_identity(self(), communication_manager),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({send_msg, Method, Params, Address, Alias}, _From, State) ->
  case check_params(Method,Params) of
    ok ->  {reply,{translate(Method), Address, [list_to_binary(Params)], Alias},State};
    _ -> fail             %TODO check
  end;

handle_call({rcv_msg, Method, Address, Params}, _From, State) ->
  {reply,{back_translate(Method), Address, binary_to_list(list_to_binary(Params))},State};

handle_call(Request, _From, State) ->
  io:format("CM: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast(Request, State) ->
  io:format("CM: Unexpected cast message: ~p~n", [Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%TODO has to be developed?
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


translate(join) ->
  1;
translate(lookup) ->
  2;
translate(lookup_response) ->
  3;
translate(leave) ->
  4;
translate(_) ->
  method_not_exists.


back_translate(1) ->
  join;
back_translate(2) ->
  lookup;
back_translate(3) ->
  lookup_response;
back_translate(4) ->
  leave;
back_translate(_) ->
  error_method_code.


check_params(join, Params) ->
  case length(Params) of
    1 -> ok;                 %Address
    _-> unexpected_params
  end;

check_params(lookup, Params) ->
  case length(Params) of
    1 -> ok;                %Requested
    _-> unexpected_params
  end;

check_params(lookup_response, Params) ->
  case length(Params) of
    1 -> ok;                %Successor
    _-> unexpected_params
  end;

check_params(leave, Params) ->
  case length(Params) of
    0 -> ok;
    _-> unexpected_params
  end;

check_params(_, Params) ->
  unexpected_params.          %TODO maybe it can be eliminated