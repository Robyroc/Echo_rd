-module(socket_listener).
-author("robyroc").
-behaviour(gen_server).

%% API
-export([start_link/0,
  check_availability/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check_availability(Port) ->
  case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}])
  of
    {ok, Sock} ->
      ok = gen_tcp:close(Sock);
    {error, Reason} ->
      Reason
end.

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
  self() ! startup,
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
  case logging_policies:check_policy(?MODULE) of
    lager_on -> lager:error("Listen: Unexpected call message: ~p~n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  case logging_policies:check_policy(?MODULE) of
    lager_on -> lager:error("Listen: Unexpected cast message: ~p~n", [Request]);
    _ -> ok
  end,
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
handle_info(loop, State) ->
  {ok, Socket} = gen_tcp:accept(State#state.socket),
  link_manager:move_socket(Socket),
  link_manager:incoming_connection(Socket),
  erlang:send_after(10, self(), loop),
  {noreply, State};

handle_info(startup, _State) ->
  naming_handler:wait_service(link_manager),
  Port = naming_handler:get_identity(port),
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
  case logging_policies:check_policy(?MODULE) of
    lager_on -> lager:info("Listening at port ~p~n", [Port]);
    _ -> io:format("Listening at port ~p~n", [Port])
  end,
  naming_handler:notify_identity(self(), listener),
  erlang:send_after(10, self(), loop),
  {noreply, #state{socket = Listen}};

handle_info(Info, State) ->
  case logging_policies:check_policy(?MODULE) of
    lager_on -> lager:error("Listen: Unexpected ! message: ~p~n", [Info]);
    _ -> ok
  end,
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
terminate(Reason, State) ->
  case logging_policies:check_policy(?MODULE) of
    lager_on -> lager:info("Listen terminate: ~p~n", [Reason]);
    _ -> ok
  end,
  gen_tcp:close(State#state.socket),
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
