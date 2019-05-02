-module(lookup_request).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/4, respond/2, notify_lost_node/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {requested, from, list, type}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Requested, From, List, ListType) ->
  gen_server:start_link(?MODULE, [Requested, From, List, ListType], []).

respond(PID, Address) ->
  gen_server:cast(PID, {response, Address}).

notify_lost_node(PID, Address) ->
  gen_server:call(PID, {lost, Address}).

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
init([Requested, From, List, ListType]) ->
  erlang:send_after(10, self(), next),
  {ok, #state{requested = Requested, from = From, list = List, type = ListType}};

init(_) ->
  {stop, badarg}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({lost, Address}, _From, State) ->
  List = State#state.list,
  CleanedList = [X || X <- List, X =/= Address],
  {reply, ok, State#state{list = CleanedList}};

handle_call(Request, _From, State) ->
  lager:error("Request: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({response, Address}, State) ->
  gen_server:reply(State#state.from, {found, Address}),
  {stop, normal, State};

handle_cast(Request, State) ->
  lager:error("Request: Unexpected cast message: ~p~n", [Request]),
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
handle_info(next, State) ->
  NewState = next_message(State),
  case NewState of
    terminate ->
      {stop, not_reachable, State};
    _ ->
      erlang:send_after(1500, self(), next),
      {noreply, NewState}
  end;

handle_info(Info, State) ->
  lager:error("Request: Unexpected ! message: ~p~n", [Info]),
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


next_message(State) ->
  next_message(State#state.requested, State#state.list, State#state.type, State#state.from).


next_message(_Requested, [], succ, _From) ->
  terminate;

next_message(Requested, [], finger, From) ->
  List = stabilizer:get_successor_list(),
  AList = [X || {_, X} <- List],
  next_message(Requested, AList, succ, From);

next_message(Requested, List, Type, From) ->
  Address = hd(List),
  communication_manager:send_message_async(lookup, [Requested], Address, link_manager:get_own_address()),
  #state{requested = Requested, list = tl(List), type = Type, from = From}.