-module(tcp_manager).
-author("robyroc").

%% API
-export([init/1]).
-define(PORT, 6543).

%TODO handle tcp failures in whole file


init(CManager) ->
  process_flag(trap_exit, true),
  {ok, Listen} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
  io:format("Listening at port ~p~n", [?PORT]),
  Listener = spawn(fun() -> listener(self(), Listen) end),
  tcpManager(CManager, Listener, []).

tcpManager(CManager, Listener, Connections) ->
  receive
    {Listener, newSocket, Socket} ->
      Pid = spawn_link(fun() -> handler(CManager, Socket) end),
      gen_tcp:controlling_process(Socket, Pid),
      tcpManager(CManager, Listener, [Pid | Connections]);
    {CManager, die} ->
      exit(Listener, die),
      [exit(H, die) || H <- Connections],
      ok;
    {PID, connectTo, {Port, IP}} ->
      {ok, RequestSocket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}]),
      Pid = spawn_link(fun() -> handler(CManager, RequestSocket) end),
      gen_tcp:controlling_process(RequestSocket, Pid),
      PID ! {CManager, link, Pid},
      tcpManager(CManager, Listener, [Pid | Connections])
  end.

listener(TcpManager, Socket) ->
  {ok, Listen} = gen_tcp:accept(Socket),
  TcpManager ! {self(), newSocket, Listen},
  listener(TcpManager, Socket).

handler(CManager, Socket) ->
  receive
    {tcp, Socket, Bin} ->
      handleIncoming(CManager, Bin),
      handler(CManager, Socket);
    {tcp_closed, Socket} ->
      gen_tcp:close(Socket);
    {_Pid, Method, noAlias, Params} ->        %no params
      Message = marshall(getOwnAddress(), Method, Params),
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
  {parseAddress(Address), list_to_atom(Method), parseParams(Params)}.

parseAddress(Address) -> ok.      %TODO parse address correctly

handleIncoming(CManager, Bin) ->
  {Address, Method, Params} = parse_message(Bin),
  CManager ! {self(), Method, Address, Params}.

getOwnAddress() -> ok.      %TODO gather own address

marshall(Address, Method, Params) -> ok.        %TODO marshall outgoing messages

parseParams(Params) -> ok.                %TODO parse params received