-module(link_manager).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0,
  incoming_connection/1,
  get_own_address/0,
  notify_incoming_message/1,
  send_message/2,
  compact_address/1,
  move_socket/1,
  address_to_binary/1,
  binary_to_address/1,
  binary_address_size/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(INTERVAL, 4000).
-define(SERVER, ?MODULE).
-record(state, {connections, port}).

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

incoming_connection(Socket) ->
  PID = naming_handler:get_identity(link_manager),
  gen_server:cast(PID, {new_connection, Socket}).

notify_incoming_message(Message) ->
  PID = naming_handler:get_identity(link_manager),
  gen_server:cast(PID, {received, Message}).

send_message({Port, IP}, Message) ->
  PID = naming_handler:get_identity(link_manager),
  gen_server:call(PID, {send, {Port, IP}, Message}, 10000).

compact_address(Address) ->
  {Port, {IPA, IPB, IPC, IPD}} = Address,
  lists:flatten([integer_to_list(IPA), ".", integer_to_list(IPB), ".", integer_to_list(IPC), ".",
    integer_to_list(IPD), ":", integer_to_list(Port)]).

address_to_binary(Address) ->
  {Port, {IPA, IPB, IPC, IPD}} = Address,
  <<Port:16, IPA:8, IPB:8, IPC:8, IPD:8>>.

binary_to_address(Bin) ->
  <<Port:16/integer, IpA:8/integer, IpB:8/integer, IpC:8/integer, IpD:8/integer>> = Bin,
  {Port, {IpA, IpB, IpC, IpD}}.

binary_address_size() ->
  byte_size(address_to_binary({6543, {127, 0, 0, 1}})).

get_own_address() ->
%  local_address().
 {naming_handler:get_identity(port), public_ip:get_public_ip()}.

move_socket(Socket) ->
  PID = naming_handler:get_identity(link_manager),
  gen_tcp:controlling_process(Socket, PID).

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
handle_call({send, {Port, IP}, Message}, _From, State) ->
  Present = [X || {X, Addr, _} <- State#state.connections, Addr == {Port, IP}],
  case Present of
    [] ->
      case gen_tcp:connect(IP, Port, [binary, {packet, 0}], ?INTERVAL) of
        {ok, RequestSocket} ->
          Sup = naming_handler:get_identity(handler_supervisor),
          Ret = supervisor:start_child(Sup, [RequestSocket]),
          case Ret of
            {ok, PID} ->
              gen_tcp:controlling_process(RequestSocket, PID),
              Monitor = erlang:monitor(process, PID),
              socket_handler:send_message(PID, Message),
              {reply, ok, #state{connections = [{PID, {Port, IP}, Monitor} | State#state.connections]}};
            {ok, PID, _} ->
              gen_tcp:controlling_process(RequestSocket, PID),
              Monitor = erlang:monitor(process, PID),
              socket_handler:send_message(PID, Message),
              {reply, ok, #state{connections = [{PID, {Port, IP}, Monitor} | State#state.connections]}}
          end;
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end;
    [H|_] ->
      socket_handler:send_message(H, Message),
      {reply, ok, State}
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
  communication_manager:receive_message(Message),
  {noreply, State};

handle_cast({new_connection, Socket}, State) ->
  Sup = naming_handler:get_identity(handler_supervisor),
  Ret = supervisor:start_child(Sup, [Socket]),
  case Ret of
    {ok, PID} ->
      gen_tcp:controlling_process(Socket, PID),
      {noreply, State};
    {ok, PID, _} ->
      gen_tcp:controlling_process(Socket, PID),
      {noreply, State}
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
handle_info({'DOWN', Monitor, process, _PID, tcp_closed}, State) ->
  Present = [X || {_, X, M} <- State#state.connections, M =:= Monitor],
  Address = hd(Present),
  checker:notify_lost_node(Address),
  stabilizer:notify_lost_node(Address),
  router:notify_lost_node(Address),
  {noreply, #state{connections = [{Pid, Addr, Mon} || {Pid, Addr, Mon} <- State#state.connections, Mon =/= Monitor]}};

handle_info({'DOWN', Monitor, process, _PID, Reason}, State) ->
  Present = [X || {_, X, M} <- State#state.connections, M =:= Monitor],
  io:format("LM: A handler failed: Address: ~p~nReason: ~p~n", [hd(Present), Reason]),
  {noreply, #state{connections = [{P, A, M} || {P, A, M} <- State#state.connections, M =/= Monitor]}};

handle_info({tcp, Socket, Bin}, State) ->
  {ok, Pid} = socket_handler:start(Socket),
  Pid ! {tcp, Socket, Bin},
  timer:sleep(10000),
  exit(Pid, kill),
  {noreply, State};

handle_info(startup, _State) ->
  naming_handler:wait_service(port),
  naming_handler:notify_identity(self(), link_manager),
  {noreply, #state{connections = []}};

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


local_address() ->
  {ok, Addrs} = inet:getif(),
  IP = hd([Addr || {Addr, _,_} <- Addrs, size(Addr) == 4, Addr =/= {127,0,0,1}]),
  {naming_handler:get_identity(port), IP}.