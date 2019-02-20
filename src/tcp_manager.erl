-module(tcp_manager).
-author("robyroc").

%% API
-export([init/1, listener/2]).
-define(PORT, 6543).

%TODO handle tcp failures in whole file


init(CManager) ->
  process_flag(trap_exit, true),
  {ok, Listen} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
  io:format("Listening at port ~p~n", [?PORT]),
  Listener = spawn(?MODULE, listener, [self(), Listen]),
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
      Method = parse_method(Bin),
      case Method of
        true -> ok   %TODO: handle various requests and send to CManager
      end,
      handler(CManager, Socket);
    {tcp_closed, Socket} ->
      gen_tcp:close(Socket);
    {Pid, Message} -> ok      %TODO send message with formatting rules
  end.

parse_method(Bin) -> ok.      %TODO parse incoming message
