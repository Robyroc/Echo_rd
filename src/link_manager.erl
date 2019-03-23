-module(link_manager).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, incoming_connection/2, get_own_address/0, notify_incoming_message/2, send_message/3, compact_address/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(PORT, 6543).      %TODO merge this def with the one on socket_listener
-record(state, {connections}).

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

incoming_connection(PID, Socket) ->
  gen_server:cast(PID, {new_connection, Socket}).

notify_incoming_message(PID, Message) ->
  gen_server:cast(PID, {received, Message}).

send_message(PID, {Port, IP}, Message) ->
  gen_server:call(PID, {send, {Port, IP}, Message}).

compact_address(Address) ->
  {Port, {IPA, IPB, IPC, IPD}} = Address,
  lists:flatten([integer_to_list(IPA), ".", integer_to_list(IPB), ".", integer_to_list(IPC), ".",
    integer_to_list(IPD), ":", integer_to_list(Port)]).

get_own_address() ->
  {ok, Addrs} = inet:getif(),
  IP = hd([Addr || {Addr, _,_} <- Addrs, size(Addr) == 4, Addr =/= {127,0,0,1}]),
  {?PORT, IP}.

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
  naming_service:notify_identity(self(), link_manager),
  {ok, #state{connections = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({send, {Port, IP}, Message}, _From, State) ->
  Present = [X || {X, Addr, _} <- State#state.connections, Addr == {Port, IP}],
  case Present of
    [] ->
      case gen_tcp:connect(IP, Port, [binary, {packet, 0}]) of
        {ok, RequestSocket} ->
          Sup = naming_service:get_identity(handler_supervisor),
          Ret = supervisor:start_child(Sup, [RequestSocket]),
          case Ret of
            {ok, PID} ->
              Monitor = erlang:monitor(process, PID),
              socket_handler:send_message(PID, Message),
              {reply, ok, #state{connections = [{PID, {Port, IP}, Monitor} | State#state.connections]}};
            {ok, PID, _} ->
              Monitor = erlang:monitor(process, PID),
              socket_handler:send_message(PID, Message),
              {reply, ok, #state{connections = [{PID, {Port, IP}, Monitor} | State#state.connections]}}
          end;
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end;
    [H|_] ->
      socket_handler:send_message(H, Message)
  end;

handle_call(Request, _From, State) ->
  io:format("LM: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({received, Message}, State) ->
  %TODO foward message to communication manager
  {noreply, State};

handle_cast({new_connection, Socket}, State) ->
  Sup = naming_service:get_identity(handler_supervisor),
  Ret = supervisor:start_child(Sup, [Socket]),
  case Ret of
    {ok, PID} ->
      {reply, ok, #state{connections = [{PID, incoming, no_monitor} | State#state.connections]}};
    {ok, PID, _} ->
      {reply, ok, #state{connections = [{PID, incoming, no_monitor} | State#state.connections]}}
  end;

handle_cast(Request, State) ->
  io:format("LM: Unexpected cast message: ~p~n", [Request]),
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
handle_info({'DOWN', Monitor, process, _PID, Reason}, State) ->
  Present = [X || {_, X, M} <- State#state.connections, M =:= Monitor],
  io:format("LM: A handler failed: Address: ~p~nReason: ~p~n", [hd(Present), Reason]),
  {noreply, #state{connections = [{P, A, M} || {P, A, M} <- State#state.connections, M =/= Monitor]}};

handle_info(Info, State) ->
  io:format("LM: Unexpected ! message: ~p~n", [Info]),
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
