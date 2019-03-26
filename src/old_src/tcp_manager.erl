-module(tcp_manager).
-author("robyroc").

%% API
-export([init/1, get_own_address/0, marshall/3, parse_message/1]).
-define(PORT, 6543).
-define(SEP, 33).
%TODO set sep to 31 before release

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
      tcp_manager(CManager, Listener, NewConnections);
    {PID, compact_address, Address} ->
      {Port, {IPA, IPB, IPC, IPD}} = Address,
      Compacted = lists:flatten([integer_to_list(IPA), ".", integer_to_list(IPB), ".", integer_to_list(IPC), ".",
        integer_to_list(IPD), ":", integer_to_list(Port)]),
      PID ! {CManager, compacted_address, Compacted}
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
    {_Pid, Method, no_alias, Params} ->
      Message = marshall(get_own_address(), atom_to_list(Method), Params),  %TODO in communication manager handle conversion back
      gen_tcp:send(Socket, Message),
      handler(CManager, Socket);
    {_Pid, Method, Alias, Params} ->
      Message = marshall(Alias, atom_to_list(Method), Params),
      gen_tcp:send(Socket, Message),
      handler(CManager, Socket);
    _ -> handler(CManager, Socket)
  end.


%%TODO deprecated
parse_message_no(Bin) ->
  String = binary_to_list(Bin),
  SplittedList = [X || X <- string:split(String, [?SEP, ?SEP], all), X =/= [] ],
  [Port, IpA, IpB, IpC, IpD, Method | Params] = lists:map(
    fun(X) -> lists:flatten(string:replace(X, [?SEP, 0], [?SEP], all)) end, SplittedList),
  {{list_to_integer(Port), {list_to_integer(IpA), list_to_integer(IpB), list_to_integer(IpC), list_to_integer(IpD)}},
    Method, Params}.

%% To test if parse message and marshall works, run the following command
%% TODO remove both methods from exported list
%% tcp_manager:parse_message(tcp_manager:marshall({1234, {192, 1, 2, 3}}, 17, [<<"1">>,<<"kaka">>,<<"ab3">>])).

handle_incoming(CManager, Bin) ->
  {Address, Method, Params} = parse_message(Bin),
  CManager ! {self(), Method, Address, Params}.

get_own_address() ->
  {ok, Addrs} = inet:getif(),
  IP = hd([Addr || {Addr, _,_} <- Addrs, size(Addr) == 4, Addr =/= {127,0,0,1}]),
  {?PORT, IP}.


marshall(Address, Method, Params) ->
  {Port, {IpA, IpB, IpC, IpD}} = Address,
  Length = length(Params),
  case Length of
    0 -> <<Port:16, IpA:8, IpB:8, IpC:8, IpD:8, Method:8, Length:8>>;
    _ ->
      Sizes = list_to_binary(lists:map(fun(X) ->       %%TODO communication manager send Params in list of binary
        byte_size(X) end, lists:reverse(tl(lists:reverse(Params))))),
      ParamsList = list_to_binary(Params),
      <<Port:16, IpA:8, IpB:8, IpC:8, IpD:8, Method:8, Length:8, Sizes:(Length-1)/binary, ParamsList/binary>>
  end.

parse_cleaner({Address, Method, [<<>>]}) ->
  {Address, Method, []};
parse_cleaner(A) -> A.


parse_message(Bin) ->
  <<Port:16/integer, IpA:8/integer, IpB:8/integer, IpC:8/integer, IpD:8/integer, Method:8/integer,
    Length:8/integer, Rest/binary>> = Bin,
  case Length of
    0 -> ActualLength = 0;
    _ -> ActualLength = (Length-1)
  end,
  <<SizeBin:ActualLength/binary, Params/binary>> = Rest,
  Size = binary_to_list(SizeBin),
  ParamsParsed = lists:foldl(fun(Elem, Acc) ->
    {Res, RestBin} = Acc,
    <<First:Elem/binary, Tail/binary>> = RestBin,
    {[First | Res], Tail} end,
    {[], Params}, Size),
  {ListParams, LastParam} = ParamsParsed,
  Parameters = lists:reverse([LastParam | ListParams]),
  parse_cleaner({{Port, {IpA, IpB, IpC, IpD}}, Method, Parameters}).

%%TODO deprecated
marshall_no(Address, MethodList, Params) ->
  {Port, {IpA, IpB, IpC, IpD}} = Address,
  PortParsed = lists:flatten(string:replace(integer_to_list(Port), [?SEP], [?SEP,0], all)),
  IpAParsed = lists:flatten(string:replace(integer_to_list(IpA), [?SEP], [?SEP,0], all)),
  IpBParsed = lists:flatten(string:replace(integer_to_list(IpB), [?SEP], [?SEP,0], all)),
  IpCParsed = lists:flatten(string:replace(integer_to_list(IpC), [?SEP], [?SEP,0], all)),
  IpDParsed = lists:flatten(string:replace(integer_to_list(IpD), [?SEP], [?SEP,0], all)),
  MethodParsed = lists:flatten(string:replace(MethodList, [?SEP], [?SEP,0], all)),
  ParamsParsed = [lists:flatten(string:replace(X, [?SEP], [?SEP,0], all)) || X <- Params],

  Out = lists:flatten([?SEP, ?SEP | [unicode:characters_to_list([X, [?SEP, ?SEP]]) ||
    X <- [PortParsed, IpAParsed, IpBParsed, IpCParsed,IpDParsed, MethodParsed | ParamsParsed]]]),

  list_to_binary(Out).

