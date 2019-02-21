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
      handleIncoming(CManager, Bin),
      handler(CManager, Socket);
    {tcp_closed, Socket} ->
      gen_tcp:close(Socket);
    {_, Command, Alias} ->        %no params
      case Alias of
        noAlias ->
          Address = getOwnAddress();
        X ->
          Address = X
      end,
      Message = marshall0(Address, Command),
      gen_tcp:send(Socket, Message);
    {ok} -> ok                    %TODO end outgoing message handling
  end.

parse_message(Bin) ->       %TODO parse incoming message
  string:tokens(binary_to_list(Bin), [31]).

getMethod([97, 100, 100, 68| _]) -> addData;
getMethod([97, 100, 100, 101| _]) -> addedData;
getMethod([99|_]) -> checkAlive;
getMethod([100, 101, 108, 101, 116, 101, 68| _]) -> deleteData;
getMethod([100, 101, 108, 101, 116, 101, 100| _]) -> deletedData;
getMethod([103|_]) -> getData;
getMethod([105|_]) -> imAlive;
getMethod([106|_]) -> join;
getMethod([108|_]) -> lookup;
getMethod([111|_]) -> obtainedData;
getMethod([112, 114, 101, 100, 70| _]) -> predFind;
getMethod([112, 114, 101, 100, 84| _]) -> predTell;
getMethod(_) -> error.

parseAddress(Address) -> ok.      %TODO parse address correctly

scanData(Bin) -> ok.              %TODO parse data correctly

handleIncoming(CManager, Bin) ->
  [Address | [Method | Params]] = parse_message(Bin),
  AParsed = parseAddress(Address),
  case getMethod(Method) of
    lookup ->
      [X | []] = Params,
      {Integer, _} = string:to_integer(X),
      CManager ! {self(), lookup, AParsed, Integer};
    checkAlive ->
      [] = Params,
      CManager ! {self(), checkAlive, AParsed};
    imAlive ->
      [] = Params,
      CManager ! {self(), imAlive, AParsed};
    predFind ->
      [] = Params,
      CManager ! {self(), predFind, AParsed};
    predTell ->
      [X | []] = Params,
      XParsed = parseAddress(X),
      CManager ! {self(), predTell, AParsed, XParsed};
    addData ->
      Data = scanData(Params),
      CManager ! {self(), addData, AParsed, Data};
    addedData ->
      [X | HashData] = Params,
      {Integer, _} = string:to_integer(X),
      CManager ! {self(), addedData, AParsed, Integer, scanData(HashData)};
    getData ->
      [X | []] = Params,
      {Integer, _} = string:to_integer(X),
      CManager ! {self(), getData, AParsed, Integer};
    obtainedData ->
      Data = scanData(Params),
      CManager ! {self(), obtainedData, AParsed, Data};
    deleteData ->
      [X | []] = Params,
      {Integer, _} = string:to_integer(X),
      CManager ! {self(), deleteData, AParsed, Integer};
    deletedData ->
      [X | []] = Params,
      {Integer, _} = string:to_integer(X),
      CManager ! {self(), deleteDData, AParsed, Integer};
    join ->
      [] = Params,
      CManager ! {self(), checkAlive, AParsed};
    error -> io:format("error: ~p~n", [Bin])
  end.

getOwnAddress() -> ok.      %TODO gather own address

marshall0(Address, Command) -> ok.        %TODO marshall outgoing messages
