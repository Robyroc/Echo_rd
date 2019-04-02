-module(communication_manager).
-author("Antonio").

-behaviour(gen_server).

%% API
-export([start_link/0, send_message/4, receive_message/1, check_params/2]).

-export([encode_resource/2, decode_resource/2, encode_ID/2, decode_ID/2]).      %TODO remove me when done debugging

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
  gen_server:call(PID, {rcv_msg, Method, Address, Params}).


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


init([NBits]) ->
  naming_handler:wait_service(params_handler),
  params_handler:get_param(nbits),
  naming_handler:notify_identity(self(), communication_manager),
  {ok, #state{nbits = NBits}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({send_msg, Method, Params, Address, Alias}, _From, State) ->
  case check_params(Method,Params) of
    ok ->  {reply,{translate(Method), Address, encode_params(Method, Params), Alias},State};
    _ -> {stop, fail, State}
  end;

handle_call({rcv_msg, Method, Address, Params}, _From, State) ->
  {reply,{back_translate(Method), Address, decode_params(back_translate(Method), Params)},State};

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
translate(request_for_info) -> 3;
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
back_translate(3) -> request_for_info;
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

check_params(lookup_for_join, Params) when length(Params) =:= 1 -> ok;
check_params(lookup_response, Params) when length(Params) =:= 2 -> ok;
check_params(request_for_info, []) -> ok;
check_params(join_info, Params) when length(Params) =:= 0 -> ok;    %TODO check this
check_params(ack_info, []) -> ok;
check_params(abort, Params) when length(Params) =:= 1 -> ok;
check_params(ack_join, []) -> ok;
check_params(leave_info, Params) when length(Params) =:= 1 -> ok;   %TODO check this
check_params(leave_ack, []) -> ok;
check_params(ask_pred, []) -> ok;
check_params(pred_reply, Params) when length(Params) =:= 1 -> ok;   %TODO check this
check_params(lookup, Params) when length(Params) =:= 1 -> ok;
check_params(command, Params) when length(Params) =:= 1 -> ok;
check_params(_, _) -> badarg.


encode_params(Method, Params) ->
  ok.

decode_params(Method, Params) ->
  ok.


%test with communication_manager:decode_ID(communication_manager:encode_ID(17, 9), 9).

encode_ID(ID, NBits) ->
  NBytes = ceil(NBits / 8),
  <<ID:(NBytes * 8)/integer>>.

decode_ID(Bin, NBits) ->
  ActualNBits = ceil(NBits / 8) * 8,
  <<ID:ActualNBits/integer>> = Bin,
  ID.

% test with communication_manager:decode_resource(communication_manager:encode_resource([{3587, <<"dwin">>}, {321, <<"abcdefghijklmnopqrstuvwxyz">>}], 12), 12).
%Resources are in the form of {ID, <<Bin>>}
encode_resource(Resources, NBits) ->
  N = length(Resources),
  Indexes = [X || {X, _} <- Resources],
  EncodedIndexes = list_to_binary([encode_ID(X, NBits) || X <- Indexes]),
  Binaries = [X || {_, X} <- Resources],
  Lengths = [byte_size(X) || X <- Binaries],       %NBits due to index
  MaxDim = lists:foldl(fun(Elem, Acc) -> max(Elem, Acc) end, 0, Lengths),
  BitsForDim = ceil(math:log2(MaxDim)),
  EncodedLengths = list_to_binary([encode_ID(X, ceil(math:log2(MaxDim))) || X <- Lengths]),
  BinaryResource = list_to_binary(Binaries),
  <<N:8, EncodedIndexes/binary, (ceil(BitsForDim / 8)):8, EncodedLengths/binary ,BinaryResource/binary>>.

decode_resource(Bin, NBits) ->
  <<N:8/integer, Rest/binary>> = Bin,
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

extract_integer(N, Bytes, Rest) ->
  extract_integer(N, Bytes, Rest, []).

extract_integer(0, _Bytes, Rest, Acc) ->
  {lists:reverse(Acc), Rest};

extract_integer(N, Bytes, Rest, Acc) ->
  Bits = Bytes * 8,
  <<Length:Bits/integer, NewRest/binary>> = Rest,
  extract_integer(N-1, Bytes, NewRest, [Length | Acc]).