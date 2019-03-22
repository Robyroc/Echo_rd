-module(fixer).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {id, nbits, index}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(ID, NBits) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ID, NBits], []).

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
init([ID, NBits]) ->
  naming_service:notify_identity(self(), fixer),
  erlang:send_after(20000, self(), fix),                    %TODO tune parameters accordingly
  erlang:send_after(120000, self(), notify),
  {ok, #state{id = ID, nbits = NBits, index = 0}};

init(_) ->
  {stop, incorrect_params}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
  io:format("FIX: Unexpected call message: ~p~n", Request),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  io:format("FIX: Unexpected cast message: ~p~n", Request),
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
handle_info(fix, State) ->
  %TODO make it ask for a lookup
  %TODO After receipt of response tell router to update finger table
  erlang:send_after(20000, self(), fix),
  {noreply, iterate_state(State)};

handle_info(notify, State) ->
  naming_service:notify_identity(self(), fixer),
  {noreply, State};

handle_info(Info, State) ->
  io:format("FIX: Unexpected ! message: ~p~n", Info),
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

iterate_state(State) ->
  #state{id = ID, nbits = NBits, index = Index} = State,
  case Index of
    NBits -> #state{id = ID, nbits = NBits, index = 0};
    _ -> #state{id = ID, nbits = NBits, index = Index + 1}
  end.