-module(link_manager).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0,
  incoming_connection/1,
  get_own_address/0,
  notify_incoming_message/1,
  send_message_sync/2,
  compact_address/1,
  move_socket/1,
  address_to_binary/1,
  binary_to_address/1,
  binary_address_size/0,
  send_message_async/2]).

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

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

incoming_connection(Socket) ->
  PID = naming_handler:get_identity(link_manager),
  gen_server:cast(PID, {new_connection, Socket}).

notify_incoming_message(Message) ->
  PID = naming_handler:get_identity(link_manager),
  gen_server:cast(PID, {received, Message}).

send_message_sync({Port, IP}, Message) ->
  PID = naming_handler:get_identity(link_manager),
  gen_server:call(PID, {send, {Port, IP}, Message}, 10000).

send_message_async({Port, IP}, Message) ->
  PID = naming_handler:get_identity(link_manager),
  gen_server:cast(PID, {send, {Port, IP}, Message}).

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
  case application:get_env(echo_rd, ip) of
    {ok, public} ->
      {naming_handler:get_identity(port), public_ip:get_public_ip()};
    _ -> local_address()
  end.

move_socket(Socket) ->
  PID = naming_handler:get_identity(link_manager),
  gen_tcp:controlling_process(Socket, PID).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
  self() ! startup,
  {ok, #state{}}.


handle_call({send, {Port, IP}, Message}, _From, State) ->
  {_, _, Params} = Message,
  Size = byte_size(list_to_binary(Params)),
  send(Port, IP, Message, State, Size);

handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("LM: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("LM: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("LM: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast({received, Message}, State) ->
  communication_manager:receive_message(Message),
  {noreply, State};

handle_cast({send, {Port, IP}, Message}, State) ->
  {_, _, Params} = Message,
  Size = byte_size(list_to_binary(Params)),
  send_async(Port, IP, Message, State, Size);

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
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("LM: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("LM: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("LM: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.


handle_info({'DOWN', Monitor, process, _PID, tcp_closed}, State) ->
  Present = [X || {_, X, M} <- State#state.connections, M =:= Monitor],
  Address = hd(Present),
  checker:notify_lost_node(Address),
  stabilizer:notify_lost_node(Address),
  router:notify_lost_node(Address),
  {noreply, State#state{connections = [{Pid, Addr, Mon} || {Pid, Addr, Mon} <- State#state.connections, Mon =/= Monitor]}};

handle_info({'DOWN', Monitor, process, _PID, unused}, State) ->
  {noreply, State#state{connections = [{Pid, Addr, Mon} || {Pid, Addr, Mon} <- State#state.connections, Mon =/= Monitor]}};

handle_info({'DOWN', Monitor, process, _PID, Reason}, State) ->
  Present = [X || {_, X, M} <- State#state.connections, M =:= Monitor],
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("LM: A handler failed: Address: ~p~nReason: ~p\n", [hd(Present), Reason]);
    {lager_only, _} ->
      lager:error("LM: A handler failed: Address: ~p~nReason: ~p\n", [hd(Present), Reason]);
    {lager_off, _} ->
      io:format("LM: A handler failed: Address: ~p~nReason: ~p\n", [hd(Present), Reason]);
    _ -> ok
  end,
  {noreply, State#state{connections = [{P, A, M} || {P, A, M} <- State#state.connections, M =/= Monitor]}};

handle_info({tcp, Socket, Bin}, State) ->
  {ok, Pid} = socket_handler:start(Socket),
  Pid ! {tcp, Socket, Bin},
  {noreply, State};

handle_info(startup, State) ->
  naming_handler:wait_service(port),
  naming_handler:notify_identity(self(), link_manager),
  {noreply, State#state{connections = []}};

handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("LM: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("LM: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("LM: Unexpected ! message: ~p\n", [Info]);
    _ -> ok
  end,
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

local_address() ->
  {ok, Addrs} = inet:getif(),
  IP = hd([Addr || {Addr, _,_} <- Addrs, size(Addr) == 4, Addr =/= {127,0,0,1}]),
  {naming_handler:get_identity(port), IP}.

send_async(Port, IP, Message, State, Size) when Size < 8000 ->
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
              {noreply, State#state{connections = [{PID, {Port, IP}, Monitor} | State#state.connections]}};
            {ok, PID, _} ->
              gen_tcp:controlling_process(RequestSocket, PID),
              Monitor = erlang:monitor(process, PID),
              socket_handler:send_message(PID, Message),
              {noreply, State#state{connections = [{PID, {Port, IP}, Monitor} | State#state.connections]}}
          end;
        {error, _Reason} ->
          {noreply, State}
      end;
    [H|_] ->
      socket_handler:send_message(H, Message),
      {noreply, State}
  end;

send_async(Port, IP, Message, State, _Size) ->
  case gen_tcp:connect(IP, Port, [binary, {packet, 0}], ?INTERVAL) of
    {ok, RequestSocket} ->
      Sup = naming_handler:get_identity(handler_supervisor),
      Ret = supervisor:start_child(Sup, [RequestSocket]),
      case Ret of
        {ok, PID} ->
          gen_tcp:controlling_process(RequestSocket, PID),
          socket_handler:send_message(PID, Message),
          {noreply, State};
        {ok, PID, _} ->
          gen_tcp:controlling_process(RequestSocket, PID),
          socket_handler:send_message(PID, Message),
          {noreply, State}
      end;
    {error, _Reason} ->
      {noreply, State}
  end.

send(Port, IP, Message, State, _Size) ->
  case gen_tcp:connect(IP, Port, [binary, {packet, 0}], ?INTERVAL) of
    {ok, RequestSocket} ->
      Sup = naming_handler:get_identity(handler_supervisor),
      Ret = supervisor:start_child(Sup, [RequestSocket]),
      case Ret of
        {ok, PID} ->
          gen_tcp:controlling_process(RequestSocket, PID),
          socket_handler:send_message(PID, Message),
          {reply, ok, State};
        {ok, PID, _} ->
          gen_tcp:controlling_process(RequestSocket, PID),
          socket_handler:send_message(PID, Message),
          {reply, ok, State}
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end.