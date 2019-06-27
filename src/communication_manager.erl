-module(communication_manager).
-author("Antonio").

-behaviour(gen_server).

%% API
-export([start_link/0, send_message_async/4, receive_message/1, receive_nbits/1, send_message_sync/4]).

% In order to test encoding/decoding functions, uncomment the following export statement and run one of the following commands.
%-export([encode_resource/1,
%  decode_resource/1,
%  encode_ID/2,
%  decode_ID/1,
%  encode_successor_list/2,
%  decode_successor_list/2,
%  encode_nbits_successor_and_resources/1,
%  decode_nbits_successor_and_resources/1]).
%
% communication_manager:decode_ID(communication_manager:encode_ID(-17, 9), 9).
% communication_manager:decode_resource(communication_manager:encode_resource([{"pippo", <<"dwin">>}, {"pluto", <<"abcdefghijklmnopqrstuvwxyz">>}])).
% communication_manager:decode_nbits_successor_and_resources(communication_manager:encode_nbits_successor_and_resources([12, [{1, {2, {1,2,3,4}}}, {7, {743, {6,9,5,2}}}], [{"pippo", <<"dwin">>}, {"pluto", <<"abcdefghijklmnopqrstuvwxyz">>}]])).
% communication_manager:decode_successor_list(communication_manager:encode_successor_list([{1, {2, {1,2,3,4}}}, {7, {743, {6,9,5,2}}}], 7), 7).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nbits}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_message_async(Method, Params, Address, Alias) ->
  PID = naming_handler:get_identity(communication_manager),
  gen_server:cast(PID, {send_msg, Method, Params, Address, Alias}).

send_message_sync(Method, Params, Address, Alias) ->
  PID = naming_handler:get_identity(communication_manager),
  gen_server:call(PID, {send_msg, Method, Params, Address, Alias}).

receive_message({Method, Address, Params}) ->
  PID = naming_handler:get_identity(communication_manager),
  gen_server:cast(PID, {rcv_msg, Method, Address, Params}).

receive_nbits(NBits) ->
  PID = naming_handler:get_identity(communication_manager),
  gen_server:call(PID, {get_nbits, NBits}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  case naming_handler:get_maybe_identity(params_handler) of
    no_name_registered -> NBits = no_nbits;
    _ -> NBits = params_handler:get_param(nbits)
  end,
  naming_handler:notify_identity(self(), communication_manager),
  {ok, #state{nbits = NBits}}.


handle_call({send_msg, Method, Params, Address, Alias}, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, able} ->
      lagerConsole:info("### OUT ###: Method:~p | Params:~p | Address:~p \n", [Method, Params, Address]),
      inout:info("### OUT ###: Method:~p | Params:~p | Address:~p \n", [Method, Params, Address]);
    {lager_only, able} ->
      inout:info("### OUT ###: Method:~p | Params:~p | Address:~p \n", [Method, Params, Address]);
    {lager_off, able} ->
      io:format("### OUT ###: Method:~p | Params:~p | Address:~p \n", [Method, Params, Address]);
    _ -> ok
  end,
  Translated = translate(Method),
  Encoded = encode_params(Method, Params, State#state.nbits),
  case Encoded of
    badarg -> {reply, fail, State};
    _ ->
      {reply,link_manager:send_message_sync(Address, {Alias, Translated, Encoded}),State}
  end;

handle_call({get_nbits, NBits}, _From, State) ->
  {reply, ok, State#state{nbits = NBits}};

handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("CM: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("CM: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("CM: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast({send_msg, Method, Params, Address, Alias}, State) ->
  case Method of
    join_info -> print_out_only_method_and_address(Method, Address);
    leave_info -> print_out_only_method_and_address(Method, Address);
    command -> print_out_only_method_and_address(Method, Address);
    _ ->
      case logging_policies:check_lager_policy(?MODULE) of
        {lager_on, able} ->
          lagerConsole:info("### OUT ###: Method:~p | Params:~p | Address:~p\n", [Method, Params, Address]),
          inout:info("### OUT ###: Method:~p | Params:~p | Address:~p\n", [Method, Params, Address]);
        {lager_only, able} ->
          inout:info("### OUT ###: Method:~p | Params:~p | Address:~p\n", [Method, Params, Address]);
        {lager_off, able} ->
          io:format("### OUT ###: Method:~p | Params:~p | Address:~p\n", [Method, Params, Address]);
        _ -> ok
      end
  end,
  Translated = translate(Method),
  Encoded = encode_params(Method, Params, State#state.nbits),
  case Encoded of
    badarg ->
      {noreply, State};
    _ ->
      link_manager:send_message_async(Address, {Alias, Translated, Encoded}),
      {noreply,State}
  end;

handle_cast({rcv_msg, Method, Address, Params}, State) ->
  BackTranslated = back_translate(Method),
  DecodedParams = decode_params(back_translate(Method), Params, State#state.nbits),
  case Method of
    join_info -> print_in_only_method_and_address(Method, Address);
    leave_info -> print_in_only_method_and_address(Method, Address);
    command -> print_in_only_method_and_address(Method, Address);
    _ ->
      case logging_policies:check_lager_policy(?MODULE) of
        {lager_on, able} ->
          lagerConsole:info("### IN ###: Method:~p | Params:~p | Address:~p\n", [BackTranslated, DecodedParams, Address]),
          inout:info("### IN ###: Method:~p | Params:~p | Address:~p\n", [BackTranslated, DecodedParams, Address]);
        {lager_only, able} ->
          inout:info("### IN ###: Method:~p | Params:~p | Address:~p\n", [BackTranslated, DecodedParams, Address]);
        {lager_off, able} ->
          io:format("### IN ###: Method:~p | Params:~p | Address:~p\n", [BackTranslated, DecodedParams, Address]);
        _ -> ok
      end
  end,
  {Modules, Call} = forward(BackTranslated, DecodedParams, Address),
  [naming_handler:wait_service(Service) || Service <- Modules],
  Call(),
  {noreply,State};

handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("CM: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("CM: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("CM: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.


handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("CM: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("CM: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("CM: Unexpected ! message: ~p\n", [Info]);
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


translate(lookup_for_join) -> 1;
translate(lookup_response) -> 2;
translate(ready_for_info) -> 3;
translate(join_info) -> 4;
translate(ack_info) -> 5;
translate(abort) -> 6;
translate(ack_join) -> 7;
translate(leave_info) -> 8;
translate(ack_leave) -> 9;
translate(ask_pred) -> 10;
translate(pred_reply) -> 11;
translate(lookup) -> 12;
translate(command) -> 13;
translate(get_stats) -> 14;
translate(stats) -> 15;
translate(net_command) -> 16;
translate(net_command_response) -> 17;
translate(_) -> badarg.

back_translate(1) -> lookup_for_join;
back_translate(2) -> lookup_response;
back_translate(3) -> ready_for_info;
back_translate(4) -> join_info;
back_translate(5) -> ack_info;
back_translate(6) -> abort;
back_translate(7) -> ack_join;
back_translate(8) -> leave_info;
back_translate(9) -> ack_leave;
back_translate(10) -> ask_pred;
back_translate(11) -> pred_reply;
back_translate(12) -> lookup;
back_translate(13) -> command;
back_translate(14) -> get_stats;
back_translate(15) -> stats;
back_translate(16) -> net_command;
back_translate(17) -> net_command_response;
back_translate(_) -> badarg.

encode_params(lookup_for_join, [], _NBits) -> [];
encode_params(lookup_response, _, no_nbits) -> badarg;
encode_params(lookup_response, [ID, Addr, Length], NBits) -> [link_manager:address_to_binary(Addr), encode_ID(ID, NBits), <<Length:16/integer>>];
encode_params(ready_for_info, [], _NBits) -> [];
encode_params(join_info, [NBits, LS, Res], _NBits) -> [encode_nbits_successor_and_resources([NBits, LS, Res])];
encode_params(ack_info, [], _NBits) -> [];
encode_params(abort, [S], _NBits) -> [list_to_binary(S)];
encode_params(ack_join, [], _NBits) -> [];
encode_params(leave_info, _, no_nbits) -> badarg;
encode_params(leave_info, [], _NBits) -> [];
encode_params(leave_info, Res, _NBits) -> [encode_resource(Res)];
encode_params(ack_leave, [], _NBits) -> [];
encode_params(ask_pred, [], _NBits) -> [];
encode_params(pred_reply, _, no_nbits) -> badarg;
encode_params(pred_reply, [Pred, SL], NBits) -> [link_manager:address_to_binary(Pred), encode_successor_list(SL, NBits)];
encode_params(lookup, _, no_nbits) -> badarg;
encode_params(lookup, [ID, Hops], NBits) -> [encode_ID(ID, NBits), <<Hops:16/integer>>];
encode_params(lookup, [ID], NBits) -> [encode_ID(ID, NBits), <<(2 * NBits):16/integer>>];
encode_params(command, [Address,C], _NBits) -> [link_manager:address_to_binary(Address), C];
encode_params(get_stats, _, no_nbits) -> badarg;
encode_params(get_stats, [Number], NBits) -> [encode_ID(Number, NBits)];
encode_params(stats, [{A, B, C, D, E, F}], _NBits) -> [list_to_binary([<<A:32, B:32, C:32, E:32, F:32>>, float_to_binary(D)])];
encode_params(net_command, _, no_nbits) -> badarg;
encode_params(net_command, [Number, Command], NBits) -> [encode_ID(Number, NBits), list_to_binary(Command)];
encode_params(net_command_response, [], _NBits) -> [];
encode_params(_, _, _) -> badarg.

decode_params(lookup_for_join, _, no_nbits) -> badarg;
decode_params(lookup_for_join, [], NBits) -> [2 * NBits];
decode_params(lookup_response, [Addr, ID, Length], _NBits) -> <<Val:16/integer>> = Length, [decode_ID(ID), link_manager:binary_to_address(Addr), Val];
decode_params(ready_for_info, [], _NBits) -> [];
decode_params(join_info, [M], _NBits) -> decode_nbits_successor_and_resources(M);
decode_params(ack_info, [], _NBits) -> [];
decode_params(abort, [S], _NBits) -> [binary_to_list(S)];
decode_params(ack_join, [], _NBits) -> [];
decode_params(leave_info, _, no_nbits) -> badarg;
decode_params(leave_info, [], _NBits) -> [];
decode_params(leave_info, [Res], _NBits) -> [decode_resource(Res)];
decode_params(ack_leave, [], _NBits) -> [];
decode_params(ask_pred, [], _NBits) -> [];
decode_params(pred_reply, _, no_nbits) -> badarg;
decode_params(pred_reply, [Pred, SL], NBits) -> [link_manager:binary_to_address(Pred), decode_successor_list(SL, NBits)];
decode_params(lookup, [ID, Hops], _NBits) -> <<Val:16/integer>> = Hops, [decode_ID(ID), Val];
decode_params(command, [A,C], _NBits) -> [link_manager:binary_to_address(A), C];
decode_params(get_stats, [Number], _NBits) -> [decode_ID(Number)];
decode_params(stats, [Bin], _NBits) -> <<A:32, B:32, C:32, E:32, F:32, D/binary>> = Bin, [{A, B, C, binary_to_float(D), E, F}];
decode_params(net_command, [Number, Command], _NBits) -> [decode_ID(Number), binary_to_list(Command)];
decode_params(net_command_response, [], _NBits) -> [];
decode_params(_, _, _) -> badarg.

forward(lookup_for_join, [Hops], From) -> {[router], fun() -> router:lookup_for_join(From, Hops) end};
forward(lookup_response, [ID, Addr, Length], _From) -> {[request_gateway, join_handler], fun() -> request_gateway:lookup_response(ID, Addr, Length), join_handler:look_response(Addr) end};
forward(ready_for_info, [], From) -> {[join_handler], fun() -> join_handler:ready_for_info(From) end};
forward(join_info, [NBits, LS, R], From) -> {[join_handler], fun() -> join_handler:info(From, R, LS, NBits) end};
forward(ack_info, [], From) -> {[join_handler], fun() -> join_handler:ack_info(From) end};
forward(abort, [S], _From) -> {[join_handler], fun() -> join_handler:abort(S) end};
forward(ack_join, [], From) -> {[join_handler], fun() -> join_handler:ack_join(From) end};
forward(leave_info, [], From) -> {[join_handler], fun() -> join_handler:leave_info([], From) end};
forward(leave_info, [Res], From) -> {[join_handler], fun() -> join_handler:leave_info(Res, From) end};
forward(ack_leave, [], From) -> {[join_handler], fun() -> join_handler:ack_leave(From) end};
forward(ask_pred, [], From) -> {[checker], fun() -> checker:get_pred(From) end};
forward(pred_reply, [Pred, SL], _From) -> {[stabilizer], fun() -> stabilizer:notify_successor(Pred, SL) end};
forward(lookup, [ID, Hops], From) -> {[router], fun() -> router:remote_lookup(ID, From, Hops) end};
forward(command, [A,C], _From) -> {[application_manager], fun() -> application_manager:receive_command(A,C) end};
forward(get_stats, [Number], From) -> {[statistics], fun() -> statistics:get_statistics(From, Number) end};
forward(stats, [S], From) -> {[statistics], fun() -> statistics:incoming_statistics(From, S) end};
forward(net_command, [Number, Command], From) -> {[network_control], fun() -> network_control:command_incoming(From, Command, Number) end};
forward(net_command_response, [], _From) -> {[network_control], fun() -> network_control:command_response() end};
forward(_, _, _) -> {[], fun() -> ok end}.


encode_ID(ID, NBits) ->
  NBytes = ceil(NBits / 8),
  ActualID = normalizer:normalize_id(ID, NBits),
  <<ActualID:(NBytes * 8)/integer>>.

decode_ID(Bin) -> decode_ID(Bin, 0).

decode_ID(<<>>, Acc) -> Acc;
decode_ID(Bin, Acc) ->
  NewAcc = Acc bsl 8,
  <<Int:8/integer, Rest/binary>> = Bin,
  decode_ID(Rest, NewAcc + Int).

encode_resource([]) -> <<>>;

encode_resource(Resources) ->              % Resources are in the form of {Name, <<Bin>>}
  N = length(Resources),
  DimOfN = ceil(ceil(math:log2(N + 1))/8),
  Names = [X || {X, _} <- Resources],
  PaddedNames = lists:map(
    fun (X) ->
      Length = length(X),
      Pad = lists:duplicate(64 - Length, 0),
      lists:flatten([Pad, X])
    end,
    Names),
  EncodedNames = list_to_binary(lists:map(fun (X) -> list_to_binary(X) end, PaddedNames)),
  Binaries = [X || {_, X} <- Resources],
  Lengths = [byte_size(X) || X <- Binaries],       %NBits due to index
  MaxDim = lists:foldl(fun(Elem, Acc) -> max(Elem, Acc) end, 0, Lengths),
  BitsForDim = ceil(math:log2(MaxDim + 1)),
  EncodedLengths = list_to_binary([encode_ID(X, ceil(math:log2(MaxDim + 1))) || X <- Lengths]),
  BinaryResource = list_to_binary(Binaries),
  BinDimOfN = <<N:(DimOfN*8)>>,
  <<DimOfN:8, BinDimOfN/binary, EncodedNames/binary, (ceil(BitsForDim / 8)):8, EncodedLengths/binary ,BinaryResource/binary>>.

decode_resource(<<>>) -> [];

decode_resource(Bin) ->
  <<DimOfN:8/integer, Bin2/binary>> = Bin,
  BitDim = (DimOfN*8),
  <<N:BitDim/integer, Rest/binary>> = Bin2,
  {Names, Remaining} = extract_names(N, Rest),
  <<ByteForDim:8/integer, LengthsAndBins/binary>> = Remaining,
  {Lengths, Resources} = extract_integer(N, ByteForDim, LengthsAndBins),
  {RevResult, <<>>} = lists:foldl(
    fun(Elem, Acc) ->
      {Result, RestBin} = Acc,
      <<First:Elem/binary, Tail/binary>> = RestBin,
      {[First | Result], Tail} end,
    {[], Resources}, Lengths),
  Result = lists:reverse(RevResult),
  lists:zip(Names, Result).

encode_nbits_successor_and_resources([NBits, List, Res]) ->
  list_to_binary([<<NBits:8>>, encode_successor_list(List, NBits), encode_resource(Res)]).

decode_nbits_successor_and_resources(Bin) ->
  <<NBits:8/integer, Rest/binary>> = Bin,
  <<NList:8/integer, _/binary>> = Rest,
  BitsPerIndex = ceil(NBits / 8) * 8,
  BitsPerAddress = link_manager:binary_address_size() * 8,
  BitsPerList = 8 + BitsPerIndex * NList + BitsPerAddress * NList,
  BytePerList = BitsPerList div 8,
  <<List:BytePerList/binary, Res/binary>> = Rest,
  [NBits, decode_successor_list(List, NBits), decode_resource(Res)].

encode_successor_list(List, NBits) ->
  N = length(List),
  Indexes = [X || {X, _} <- List],
  Addresses = [X || {_, X} <- List],
  EncodedIndexes = list_to_binary([encode_ID(X, NBits) || X <- Indexes]),
  EncodedAddresses = list_to_binary([link_manager:address_to_binary(A) || A <- Addresses]),
  <<N:8, EncodedIndexes/binary, EncodedAddresses/binary>>.

decode_successor_list(Bin, NBits) ->
  <<N:8/integer, IndexesAndAddresses/binary>> = Bin,
  {Indexes, Rest} = extract_integer(N, ceil(NBits / 8), IndexesAndAddresses),
  {Addresses, <<>>} = extract_address(N, Rest),
  lists:zip(Indexes, Addresses).


extract_integer(N, Bytes, Rest) ->
  extract_integer(N, Bytes, Rest, []).

extract_integer(0, _Bytes, Rest, Acc) ->
  {lists:reverse(Acc), Rest};

extract_integer(N, Bytes, Rest, Acc) ->
  Bits = Bytes * 8,
  <<Length:Bits/integer, NewRest/binary>> = Rest,
  extract_integer(N-1, Bytes, NewRest, [Length | Acc]).

extract_address(N, Rest) ->
  extract_address(N, Rest, []).

extract_address(0, Rest, Acc) ->
  {lists:reverse(Acc), Rest};

extract_address(N, Rest, Acc) ->
  Size = link_manager:binary_address_size(),
  <<Address:Size/binary, NewRest/binary>> = Rest,
  extract_address(N-1, NewRest, [link_manager:binary_to_address(Address) | Acc]).

extract_names(N, Rest) ->
  extract_names(N, Rest, []).

extract_names(0, Rest, Acc) ->
  {lists:reverse(Acc), Rest};

extract_names(N, Rest, Acc) ->
  <<Name:64/binary, NewRest/binary>> = Rest,
  extract_names(N-1, NewRest, [unpad(binary_to_list(Name)) | Acc]).

unpad([0 | X]) -> unpad(X);
unpad(X) -> X.

print_out_only_method_and_address(Method, Address) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, able} ->
      lagerConsole:info("### OUT ###: Method:~p | Address:~p\n", [Method, Address]),
      inout:info("### OUT ###: Method:~p | Address:~p\n", [Method, Address]);
    {lager_only, able} ->
      inout:info("### OUT ###: Method:~p | Address:~p\n", [Method, Address]);
    {lager_off, able} ->
      io:format("### OUT ###: Method:~p | Address:~p\n", [Method, Address]);
    _ -> ok
  end.

print_in_only_method_and_address(Method, Address) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, able} ->
      lagerConsole:info("### IN ###: Method:~p | Address:~p\n", [Method, Address]),
      inout:info("### IN ###: Method:~p | Address:~p\n", [Method, Address]);
    {lager_only, able} ->
      inout:info("### IN ###: Method:~p | Address:~p\n", [Method, Address]);
    {lager_off, able} ->
      io:format("### IN ###: Method:~p | Address:~p\n", [Method, Address]);
    _ -> ok
  end.