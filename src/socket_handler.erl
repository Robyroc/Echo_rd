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
start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

start(Socket) ->
  gen_server:start(?MODULE, [Socket], []).

send_message(PID, Message) ->
  gen_server:call(PID, {send, Message})   .        %TODO timeout?

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
init([Socket]) ->
  {ok, #state{socket = Socket}};

init(_) ->
  {stop, badarg}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({send, {no_alias, Method, Params}}, _From, State) ->
  Message = marshall(link_manager:get_own_address(), Method, Params),
  ok = gen_tcp:send(State#state.socket, Message),
  {reply, ok, State};

handle_call({send, {Alias, Method, Params}}, _From, State) ->
  Message = marshall(Alias, Method, Params),
  %timer:sleep(1000),
  ok = gen_tcp:send(State#state.socket, Message),
  {reply, ok, State};

handle_call(Request, _From, State) ->
  io:format("Handler: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  io:format("Handler: Unexpected cast message: ~p~n", [Request]),
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

handle_info({tcp, Socket , Bin}, State) when Socket =:= State#state.socket ->
  {Address, Method, Params} = parse_message(Bin),
  link_manager:notify_incoming_message({Method, Address, Params}),
  {noreply, State};

handle_info({tcp_closed, Socket}, State) when Socket =:= State#state.socket ->
  gen_tcp:close(Socket),
  exit(self(), tcp_closed),
  {stop, closed_connection, State};

handle_info(Info, State) ->
  io:format("Handler: Unexpected ! message: ~p~n", [Info]),
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
terminate(_Reason, State) ->
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