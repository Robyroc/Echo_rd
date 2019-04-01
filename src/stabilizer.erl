-module(stabilizer).
-author("Giacomo").

-behaviour(gen_server).

%% API
-export([start_link/4, get_successor/0, get_successor_list/0, notify_successor/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 20000).

-record(state, {succ_list, id, nbits, successor}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(SuccessorList, ID, NBits, Successor) ->                    %%TODO decide how to pass these parameters
  gen_server:start_link({local, ?SERVER}, ?MODULE, [SuccessorList, ID, NBits, Successor], []).

get_successor() ->
  PID = naming_service:get_identity(stabilizer),
  gen_server:call(PID, {get_succ}).

get_successor_list() ->
  PID = naming_service:get_identity(stabilizer),
  gen_server:call(PID, {get_succ_list}).

notify_successor(SuccessorList) ->
  PID = naming_service:get_identity(stabilizer),
  gen_server:cast(PID, {stabilize_response, SuccessorList}).

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
init([SuccessorList, ID, NBits, Successor]) ->
  naming_service:notify_identity(self(), stabilizer),
  CutList = cut_last_element(SuccessorList, NBits),
  Smaller = [{I + round(math:pow(2, NBits)), A} || {I, A} <- CutList, I =< ID],
  Corrected = [{I, A} || {I, A} <- CutList, I > ID] ++ Smaller,
  {_, Addr} = hd(Corrected),
  %%TODO make a call to open a connection for the successor and initialize Successor
  erlang:send_after(?INTERVAL, self(), stabilize),
  {ok, #state{succ_list = lists:sort(Corrected), id = ID, nbits = NBits, successor = Successor}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(get_succ, _From, State) ->
  {Index, Address} = hd(State#state.succ_list),
  {reply, {Index, Address}, State};

handle_call(get_succ_list, _From, State) ->
  {reply, State#state.succ_list, State};

handle_call(Request, _From, State) ->
  io:format("STABILIZER: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({stabilize_response, SuccessorList}, State) ->
  Address = call_to_successor_for_predecessor,
  Index = hash_f:get_hashed_addr(Address),
  HeadIndex = hd([I || {I, _} <- State#state.succ_list]),
  #state{succ_list = SuccessorList, id = ID, nbits = NBits} = State,
  NewSuccessorList = handle_pred_tell(Index, ID, HeadIndex, SuccessorList, Address, NBits),
  {noreply, State#state{succ_list = NewSuccessorList}};

handle_cast(Request, State) ->
  io:format("STABILIZER: Unexpected cast message: ~p~n", [Request]),
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
handle_info(stabilize, State) ->
  %%TODO ask to the successor who is his predecessor through Communication Manager
  erlang:send_after(?INTERVAL, self(), stabilize),
  {noreply, State};

handle_info(Info, State) ->
  io:format("STABILIZER: Unexpected ! message: ~p~n", [Info]),
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

cut_last_element(SuccessorList, NBits) ->
  Length = length(SuccessorList),
  case Length of
    Length when Length > NBits ->
      [_ | T] = lists:reverse(SuccessorList),
      lists:reverse(T);
    Length when Length =< NBits ->
      SuccessorList
  end.

handle_pred_tell(Index, ID, HeadIndex, SuccessorList, Address, NBits) when Index > ID and not(Index >= HeadIndex) ->
  update_successor_list(SuccessorList, {Index, Address}, NBits);

handle_pred_tell(Index, ID, _HeadIndex, SuccessorList, _Address, _NBits) when Index =:= ID ->
  SuccessorList;                        %TODO get succ_list from successor

handle_pred_tell(Index, ID, HeadIndex, SuccessorList, Address, NBits) when Index < ID ->
  handle_pred_tell(Index + round(math:pow(2, NBits)), ID, HeadIndex, SuccessorList, Address, NBits).

update_successor_list(SuccessorList, NewElem, NBits) ->
  Cut = cut_last_element(SuccessorList, NBits),
  [NewElem | Cut].