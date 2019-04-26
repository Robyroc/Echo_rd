-module(application_manager).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0,
  join/1,
  create/1,
  leave/0,
  issue_command/2,
  receive_command/2,
  add_many_resources/1,
  get_local_resources/1,
  drop_many_resources/1,
  join_p/2,
  create_p/2,
  connect/1,
  hash_name/1,
  send_response/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name}).

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

join_p(Port, Address) ->
  join_handler:join(Port, Address).

create_p(Port, NBits) ->
  join_handler:create(Port, NBits).

join(Address) ->
  join_handler:join(6543, Address).

create(Nbits) ->
  join_handler:create(6543, Nbits).

leave() ->
  join_handler:leave().

issue_command(Name, Command) ->
  Router = naming_handler:get_maybe_identity(router),
  case Router of
    no_name_registered -> out_of_network;
    _ ->
      Index = hash_f:get_hashed_name(Name),
      {found, Address} = router:local_lookup(Index),
      communication_manager:send_message_async(command, [link_manager:get_own_address(), Command], Address, no_alias)
  end.

receive_command(From, Command) ->
  PID = naming_handler:get_identity(application_manager),
  Module = gen_server:call(PID, get_name),
  erlang:apply(Module, receive_command, [From, Command]).

add_many_resources(Resources) ->
  PID = naming_handler:get_identity(application_manager),
  Module = gen_server:call(PID, get_name),
  erlang:apply(Module, add_many_resources, [Resources]).

get_local_resources(From) ->
  PID = naming_handler:get_identity(application_manager),
  Module = gen_server:call(PID, get_name),
  erlang:apply(Module, get_local_resources, [From]).

drop_many_resources(From) ->
  PID = naming_handler:get_identity(application_manager),
  Module = gen_server:call(PID, get_name),
  erlang:apply(Module, drop_many_resources, [From]).

connect(Name) ->
  PID = naming_handler:get_identity(application_manager),
  gen_server:call(PID, {connect, Name}).

hash_name(Name) ->
  HashF = naming_handler:get_maybe_identity(hash_f),
  case HashF of
    no_name_registered -> out_of_network;
    _ ->
      hash_f:get_hashed_name(Name)
  end.

send_response(Message, Address) ->
  communication_manager:send_message_async(command, [Address, Message], Address, no_alias).

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
  %TODO the following 3 lines are for testing lager
  lager:info("~s is ~s!", [lager, cool]),
  lager:warning("but pay ~s!", [attention]),
  lager:error("there is always some ~s", [error]),
  naming_handler:notify_identity(self(), application_manager),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(get_name, _From, State) ->
  {reply, State#state.name, State};

handle_call({connect, Name}, _From, State) ->
  {reply, ok, State#state{name = Name}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
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
handle_info(_Info, State) ->
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
