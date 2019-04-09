-module(communication_manager).
-author("Antonio").

-behaviour(gen_server).

%% API
-export([start_link/0, send_message/4, receive_message/1, receive_nbits/1]).

%TODO remove me when done debugging
-export([encode_resource/2, decode_resource/2, encode_ID/2, decode_ID/1, encode_successor_list/2, decode_successor_list/2, encode_nbits_successor_and_resources/1, decode_nbits_successor_and_resources/1]).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%TODO remove comment, it is just for testing
%communication_manager:send_message(lookup,["K20"],{6543,{192,168,1,98}},no_alias).
send_message(Method, Params, Address, Alias) ->
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
  %naming_handler:wait_service(params_handler),
  %NBits = params_handler:get_param(nbits),
  naming_handler:notify_identity(self(), communication_manager),
  {ok, #state{nbits = no_nbits}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({send_msg, Method, Params, Address, Alias}, _From, State) ->
  io:format("### OUT ###: Method:~p | Params:~p | Address:~p~n", [Method, Params, Address]),
  Translated = translate(Method),
  Encoded = encode_params(Method, Params, State#state.nbits),
  case Encoded of
    badarg -> {reply, fail, State};
    _ ->
      link_manager:send_message(Address, {Alias, Translated, Encoded}),
      {reply,ok,State}
  end;

handle_call({get_nbits, NBits}, _From, State) ->
  {reply, ok, State#state{nbits = NBits}};

handle_call(Request, _From, State) ->
  io:format("CM: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast({rcv_msg, Method, Address, Params}, State) ->
  BackTranslated = back_translate(Method),
  DecodedParams = decode_params(back_translate(Method), Params, State#state.nbits),
  io:format("### IN ###: Method:~p | Params:~p | Address:~p~n", [BackTranslated, DecodedParams, Address]),
  forward(BackTranslated, DecodedParams, Address),
  {noreply,State};

handle_cast(Request, State) ->
  io:format("CM: Unexpected cast message: ~p~n", [Request]),
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

handle_info(Info, State) ->
  io:format("CM: Unexpected ! message: ~p~n", [Info]),
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


translate(lookup_for_join) -> 1;
translate(lookup_response) -> 2;
translate(ready_for_info) -> 3;
translate(join_info) -> 4;
translate(ack_info) -> 5;
translate(abort) -> 6;
translate(ack_join) -> 7;
translate(leave_info) -> 8;
translate(leave_ack) -> 9;
translate(ask_pred) -> 10;
translate(pred_reply) -> 11;
translate(lookup) -> 12;
translate(command) -> 13;
translate(_) -> badarg.

back_translate(1) -> lookup_for_join;
back_translate(2) -> lookup_response;
back_translate(3) -> ready_for_info;
back_translate(4) -> join_info;
back_translate(5) -> ack_info;
back_translate(6) -> abort;
back_translate(7) -> ack_join;
back_translate(8) -> leave_info;
back_translate(9) -> leave_ack;
back_translate(10) -> ask_pred;
back_translate(11) -> pred_reply;
back_translate(12) -> lookup;
back_translate(13) -> command;
back_translate(_) -> badarg.

encode_params(lookup_for_join, [], _NBits) -> [];
encode_params(lookup_response, _, no_nbits) -> badarg;
encode_params(lookup_response, [ID, Addr], NBits) -> [link_manager:address_to_binary(Addr), encode_ID(ID, NBits)];
encode_params(ready_for_info, [], _NBits) -> [];
encode_params(join_info, [NBits, LS, Res], _NBits) -> [encode_nbits_successor_and_resources([NBits, LS, Res])];
encode_params(ack_info, [], _NBits) -> [];
encode_params(abort, [S], _NBits) -> [list_to_binary(S)];
encode_params(ack_join, [], _NBits) -> [];
encode_params(leave_info, _, no_nbits) -> badarg;
encode_params(leave_info, [], _NBits) -> [];
encode_params(leave_info, [Res], NBits) -> [encode_resource(Res, NBits)];
encode_params(leave_ack, [], _NBits) -> [];
encode_params(ask_pred, [], _NBits) -> [];
encode_params(pred_reply, _, no_nbits) -> badarg;
encode_params(pred_reply, [Pred, SL], NBits) -> [link_manager:address_to_binary(Pred), encode_successor_list(SL, NBits)];
encode_params(lookup, _, no_nbits) -> badarg;
encode_params(lookup, [ID], NBits) -> [encode_ID(ID, NBits)];
encode_params(command, [C], _NBits) -> [C];
encode_params(_, _, _) -> badarg.

decode_params(lookup_for_join, [], _NBits) -> [];
decode_params(lookup_response, [Addr, ID], _NBits) -> [decode_ID(ID), link_manager:binary_to_address(Addr)];
decode_params(ready_for_info, [], _NBits) -> [];
decode_params(join_info, [M], _NBits) -> decode_nbits_successor_and_resources(M);
decode_params(ack_info, [], _NBits) -> [];
decode_params(abort, [S], _NBits) -> [binary_to_list(S)];
decode_params(ack_join, [], _NBits) -> [];
decode_params(leave_info, _, no_nbits) -> badarg;
decode_params(leave_info, [], _NBits) -> [];
decode_params(leave_info, [Res], NBits) -> [decode_resource(Res, NBits)];
decode_params(leave_ack, [], _NBits) -> [];
decode_params(ask_pred, [], _NBits) -> [];
decode_params(pred_reply, _, no_nbits) -> badarg;
decode_params(pred_reply, [Pred, SL], NBits) -> [link_manager:binary_to_address(Pred), decode_successor_list(SL, NBits)];
decode_params(lookup, [ID], _NBits) -> [decode_ID(ID)];
decode_params(command, [C], _NBits) -> [C];
decode_params(_, _, _) -> badarg.


forward(lookup_for_join, [], From) -> router:lookup_for_join(From);
forward(lookup_response, [ID, Addr], _From) -> request_gateway:lookup_response(ID, Addr), join_handler:look_response(Addr);
forward(ready_for_info, [], From) -> join_handler:ready_for_info(From);
forward(join_info, [NBits, LS, R], From) -> join_handler:info(From, R, LS, NBits);
forward(ack_info, [], From) -> join_handler:ack_info(From);
forward(abort, [S], _From) -> join_handler:abort(S);
forward(ack_join, [], From) -> join_handler:ack_join(From);
forward(leave_info, [], From) -> join_handler:leave_info([], From);
forward(leave_info, [Res], From) -> join_handler:leave_info(Res, From);
forward(leave_ack, [], From) -> join_handler:ack_leave(From);
forward(ask_pred, [], From) -> checker:get_pred(From);
forward(pred_reply, [Pred, SL], _From) -> stabilizer:notify_successor(Pred, SL);
forward(lookup, [ID], From) -> router:remote_lookup(ID, From);
forward(command, [C], From) -> ok;           %TODO send to AM
forward(_, _, _) -> badarg.

%TODO remove comment, it is just for testing
% communication_manager:decode_ID(communication_manager:encode_ID(-17, 9), 9).

encode_ID(ID, NBits) ->
  NBytes = ceil(NBits / 8),
  ActualID = router:normalize_id(ID, NBits),
  <<ActualID:(NBytes * 8)/integer>>.

decode_ID(Bin) -> decode_ID(Bin, 0).

decode_ID(<<>>, Acc) -> Acc;
decode_ID(Bin, Acc) ->
  NewAcc = Acc bsl 8,
  <<Int:8/integer, Rest/binary>> = Bin,
  decode_ID(Rest, NewAcc + Int).

%TODO remove comment, it is just for testing
% communication_manager:decode_resource(communication_manager:encode_resource([{3587, <<"dwin">>}, {321, <<"abcdefghijklmnopqrstuvwxyz">>}], 12), 12).

encode_resource([], _) -> <<>>;

encode_resource(Resources, NBits) ->              % Resources are in the form of {ID, <<Bin>>}
  N = length(Resources),
  DimOfN = ceil(ceil(math:log2(N))/8),
  Indexes = [X || {X, _} <- Resources],
  EncodedIndexes = list_to_binary([encode_ID(X, NBits) || X <- Indexes]),
  Binaries = [X || {_, X} <- Resources],
  Lengths = [byte_size(X) || X <- Binaries],       %NBits due to index
  MaxDim = lists:foldl(fun(Elem, Acc) -> max(Elem, Acc) end, 0, Lengths),
  BitsForDim = ceil(math:log2(MaxDim)),
  EncodedLengths = list_to_binary([encode_ID(X, ceil(math:log2(MaxDim))) || X <- Lengths]),
  BinaryResource = list_to_binary(Binaries),
  BinDimOfN = <<N:(DimOfN*8)>>,
  <<DimOfN:8, BinDimOfN/binary, EncodedIndexes/binary, (ceil(BitsForDim / 8)):8, EncodedLengths/binary ,BinaryResource/binary>>.

decode_resource(<<>>, _) -> [];

decode_resource(Bin, NBits) ->
  <<DimOfN:8/integer, Bin2/binary>> = Bin,
  BitDim = (DimOfN*8),
  <<N:BitDim/integer, Rest/binary>> = Bin2,
  NBytes = ceil(NBits / 8),
  {Indexes, Remaining} = extract_integer(N, NBytes, Rest),
  <<BitsForDim:8/integer, LengthsAndBins/binary>> = Remaining,
  {Lengths, Resources} = extract_integer(N, BitsForDim, LengthsAndBins),
  {RevResult, <<>>} = lists:foldl(
    fun(Elem, Acc) ->
      {Result, RestBin} = Acc,
      <<First:Elem/binary, Tail/binary>> = RestBin,
      {[First | Result], Tail} end,
    {[], Resources}, Lengths),
  Result = lists:reverse(RevResult),
  lists:zip(Indexes, Result).

%TODO remove comment, it is just for testing
% communication_manager:decode_nbits_successor_and_resources(communication_manager:encode_nbits_successor_and_resources([12, [{1, {2, {1,2,3,4}}}, {7, {743, {6,9,5,2}}}], [{3587, <<"dwin">>}, {321, <<"abcdefghijklmnopqrstuvwxyz">>}]])).

encode_nbits_successor_and_resources([NBits, List, Res]) ->
  list_to_binary([<<NBits:8>>, encode_successor_list(List, NBits), encode_resource(Res, NBits)]).

decode_nbits_successor_and_resources(Bin) ->
  <<NBits:8/integer, Rest/binary>> = Bin,
  <<NList:8/integer, _/binary>> = Rest,
  BitsPerIndex = ceil(NBits / 8) * 8,
  BitsPerAddress = link_manager:binary_address_size() * 8,
  BitsPerList = 8 + BitsPerIndex * NList + BitsPerAddress * NList,
  BytePerList = BitsPerList div 8,
  <<List:BytePerList/binary, Res/binary>> = Rest,
  [NBits, decode_successor_list(List, NBits), decode_resource(Res, NBits)].

%TODO remove comment, it is just for testing
% communication_manager:decode_successor_list(communication_manager:encode_successor_list([{1, {2, {1,2,3,4}}}, {7, {743, {6,9,5,2}}}], 7), 7).

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