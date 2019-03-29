-module(request_gateway).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, add_request/3, lookup_response/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {requests}).

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

add_request(Requested, From, List) ->
  PID = naming_service:get_identity(request_gateway),
  gen_server:call(PID, {add, Requested, From, List}).

lookup_response(Requested, Address) ->
  PID = naming_service:get_identity(request_gateway),
  gen_server:cast(PID, {response, Requested, Address}).

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
  naming_service:wait_service(router),
  naming_service:notify_identity(self(), request_gateway),
  {ok, #state{requests = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({add, Requested, From, List}, _From, State) ->
  Sup = naming_service:get_identity(request_supervisor),
  Ret = supervisor:start_child(Sup, [Requested, From, List, finger]),
  case Ret of
    {ok, PID} ->
      Monitor = erlang:monitor(process, PID),
      {reply, ok, #state{requests = [{PID, Requested, Monitor} | State#state.requests]}};
    {ok, PID, _} ->
      Monitor = erlang:monitor(process, PID),
      {reply, ok, #state{requests = [{PID, Requested, Monitor} | State#state.requests]}}
  end;

handle_call(Request, _From, State) ->
  io:format("R. Gateway: Unexpected call message: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({response, Requested, Address}, State) ->
  [lookup_request:respond(PID, Address) || {PID, R, _} <- State#state.requests, R =:= Requested],
  {noreply, State};

handle_cast(Request, State) ->
  io:format("R. Gateway: Unexpected cast message: ~p~n", [Request]),
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
handle_info({'DOWN', Monitor, process, _PID, Reason}, State) ->
  Present = [X || {_, X, M} <- State#state.requests, M =:= Monitor],
  io:format("R. Gateway: A request failed: Requested: ~p~nReason: ~p~n", [hd(Present), Reason]),
  {noreply, #state{requests = [{P, R, M} || {P, R, M} <- State#state.requests, M =/= Monitor]}};

handle_info(Info, State) ->
  io:format("R. Gateway: Unexpected ! message: ~p~n", [Info]),
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
