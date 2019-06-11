-module(socket_handler).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/1, send_message/2, start/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 240000).    % 4 minutes
-record(state, {socket, remaining, acc}).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

start(Socket) ->
  gen_server:start(?MODULE, [Socket], []).

send_message(PID, Message) ->
  gen_server:cast(PID, {send, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Socket]) ->
  inet:setopts(Socket, [{active, once}]),
  {ok, #state{socket = Socket, remaining = 0, acc = []}, ?TIMEOUT};

init(_) ->
  {stop, badarg}.


handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Handler: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("Handler: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("Handler: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State, ?TIMEOUT}.


handle_cast({send, {no_alias, Method, Params}}, State) ->
  Message = marshall(link_manager:get_own_address(), Method, Params),
  Size = byte_size(Message),
  case application:get_env(echo_rd, delay) of
    undefined -> ok;
    {ok, {constant, Delay}} -> timer:sleep(Delay);
    {ok, {normal, Mean, Var}} ->
      Delay = ceil(rand:normal(Mean, Var)),
      timer:sleep(Delay)
  end,
  ok = gen_tcp:send(State#state.socket, <<Size:40/integer, Message/binary>>),
  {noreply, State, ?TIMEOUT};

handle_cast({send, {Alias, Method, Params}}, State) ->
  Message = marshall(Alias, Method, Params),
  Size = byte_size(Message),
  case application:get_env(echo_rd, delay) of
    undefined -> ok;
    {ok, {constant, Delay}} -> timer:sleep(Delay);
    {ok, {normal, Mean, Var}} ->
      Delay = ceil(rand:normal(Mean, Var)),
      timer:sleep(Delay)
  end,
  ok = gen_tcp:send(State#state.socket, <<Size:40/integer, Message/binary>>),
  {noreply, State, ?TIMEOUT};

handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Handler: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("Handler: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("Handler: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State, ?TIMEOUT}.



handle_info({tcp, Socket, <<"Not used">>}, State) when Socket =:= State#state.socket ->
  inet:setopts(Socket, [{active, once}]),
  exit(self(), unused),
  {stop, unused_connection, State};

handle_info({tcp, Socket , Bin}, State) when Socket =:= State#state.socket ->
  Result = message_framer(Bin, State),
  inet:setopts(Socket, [{active, once}]),
  Result;

handle_info({tcp_closed, Socket}, State) when Socket =:= State#state.socket ->
  gen_tcp:close(Socket),
  exit(self(), tcp_closed),
  {stop, closed_connection, State};

handle_info(timeout, State) ->
  gen_tcp:send(State#state.socket, <<"Not used">>),
  timer:sleep(10000),
  exit(self(), unused),
  {stop, unused_connection, State};

handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Handler: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("Handler: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("Handler: Unexpected ! message: ~p\n", [Info]);
    _ -> ok
  end,
  {noreply, State, ?TIMEOUT}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
marshall(Address, Method, Params) ->
  {Port, {IpA, IpB, IpC, IpD}} = Address,
  Length = length(Params),
  case Length of
    0 -> <<Port:16, IpA:8, IpB:8, IpC:8, IpD:8, Method:8, Length:8>>;
    _ ->
      Sizes = list_to_binary(lists:map(fun(X) ->       %%Note: communication manager sends Params in list of binary
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

message_framer(Bin, State) when ((byte_size(Bin) < 6) and (State#state.remaining =:= 0)) ->
  {noreply, State#state{remaining = header, acc = [Bin]}, ?TIMEOUT};

message_framer(Bin, State) when State#state.remaining =:= 0 ->
  <<Size:40/integer, Message/binary>> = Bin,
  Received = byte_size(Message),
  case Size of
    Received ->
      {Address, Method, Params} = parse_message(Message),
      io:format("~p\n", [ets:match_object(naming_db, {'$0', '$1'})]),
      link_manager:notify_incoming_message({Method, Address, Params}),
      {noreply, State, ?TIMEOUT};
    _ when Received > Size ->
      TotalDimensionOfFirst = Size + 5,
      <<First:TotalDimensionOfFirst/binary, Second/binary>> = Bin,
      {noreply, NewState, _} = message_framer(First, State),
      message_framer(Second, NewState);
    _ ->
      {noreply, State#state{remaining = Size - Received, acc = [Message]}, ?TIMEOUT}
  end;

message_framer(Bin, State) when State#state.remaining =:= header ->
  message_framer(list_to_binary([State#state.acc, Bin]), State#state{remaining = 0, acc = []});

message_framer(Bin, State) ->
  Received = byte_size(Bin),
  Remaining = State#state.remaining,
  case Received of
    Remaining ->
      Total = list_to_binary(lists:reverse([Bin | State#state.acc])),
      {Address, Method, Params} = parse_message(Total),
      io:format("~p\n", [ets:match_object(naming_db, {'$0', '$1'})]),
      link_manager:notify_incoming_message({Method, Address, Params}),
      {noreply, State#state{remaining = 0, acc = []}, ?TIMEOUT};
    _ when Received > Remaining ->
      <<First:Remaining/binary, Second/binary>> = Bin,
      {noreply, NewState, _} = message_framer(First, State),
      message_framer(Second, NewState);
    _ ->
      {noreply, State#state{remaining = Remaining - Received, acc = [Bin | State#state.acc]}, ?TIMEOUT}
  end.