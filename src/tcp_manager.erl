-module(tcp_manager).
-author("robyroc").

%% API
-export([init/1, get_own_address/0]).
-define(PORT, 6543).

%TODO handle tcp failures in whole file


init(CManager) ->
  process_flag(trap_exit, true),
  {ok, Listen} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
  io:format("Listening at port ~p~n", [?PORT]),
  Listener = spawn(fun() -> listener(self(), Listen) end),
  tcp_manager(CManager, Listener, []).

tcp_manager(CManager, Listener, Connections) ->
  receive
    {Listener, new_socket, Socket} ->
      Pid = spawn_link(fun() -> handler(CManager, Socket) end),
      gen_tcp:controlling_process(Socket, Pid),
      tcp_manager(CManager, Listener, [{Pid, incoming} | Connections]);
    {CManager, die} ->
      exit(Listener, die),
      [exit(H, die) || H <- Connections],
      ok;
    {PID, connect_to, {Port, IP}} ->
      Present = [X || {X, Addr} <- Connections, Addr == {Port, IP}],
      case Present of
        [] ->
          {ok, RequestSocket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}]),
          Pid = spawn_link(fun() -> handler(CManager, RequestSocket) end),
          gen_tcp:controlling_process(RequestSocket, Pid),
          NewConnections = [Pid | Connections];
        [H|_] ->
          Pid = H,
          NewConnections = Connections
      end,
      PID ! {CManager, link, Pid},
      tcp_manager(CManager, Listener, NewConnections)
  end.

listener(TcpManager, Socket) ->
  {ok, Listen} = gen_tcp:accept(Socket),
  TcpManager ! {self(), newSocket, Listen},
  listener(TcpManager, Socket).

handler(CManager, Socket) ->
  receive
    {tcp, Socket, Bin} ->
      handle_incoming(CManager, Bin),
      handler(CManager, Socket);
    {tcp_closed, Socket} ->
      gen_tcp:close(Socket);
    {_Pid, Method, noAlias, Params} ->
      Message = marshall(get_own_address(), Method, Params),
      gen_tcp:send(Socket, Message),
      handler(CManager, Socket);
    {_Pid, Method, Alias, Params} ->
      Message = marshall(Alias, Method, Params),
      gen_tcp:send(Socket, Message),
      handler(CManager, Socket);
    _ -> handler(CManager, Socket)
  end.

parse_message(Bin) ->
  String = binary_to_list(Bin),
  [Address, Rest] = string:split(String, 31),
  [Method, Params] = string:split(Rest, 31),
  {parse_address(Address), list_to_atom(Method), parse_params(Params)}.

parse_address(Address) -> ok.      %TODO parse address correctly

handle_incoming(CManager, Bin) ->
  {Address, Method, Params} = parse_message(Bin),
  CManager ! {self(), Method, Address, Params}.

get_own_address() ->
  {ok, Addrs} = inet:getif(),
  IP = hd([Addr || {Addr, _,_} <- Addrs, size(Addr) == 4, Addr =/= {127,0,0,1}]),
  {?PORT, IP}.

marshall(Address, Method, Params) -> ok.        %TODO marshall outgoing messages

parse_params(Params) -> ok.                %TODO parse params received