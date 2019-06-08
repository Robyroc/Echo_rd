-module(application_manager).
-author("robyroc").

-behaviour(gen_server).
-behavior(gen_app).

%% API
-export([start_link/0,
  receive_command/2,
  add_many_resources/1,
  get_local_resources/1,
  drop_many_resources/1]).

%% gen_app callbacks
-export([join/1,
  create/1,
  leave/0,
  issue_command/2,
  join_p/2,
  create_p/2,
  connect/1,
  hash_name/1,
  send_response/2,
  get_successor_list/0,
  show_finger_table/0,
  get_predecessor/0,
  get_own_id/0,
  statistics_gather/0]).

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
      naming_handler:wait_service(router),
      normalizer:normalize_as_predecessor(hash_f:get_hashed_name(Name))
  end.

get_successor_list() ->
  stabilizer:get_successor_list().

show_finger_table() ->
  router:show_table().

get_predecessor() ->
  checker:get_pred(local_address).

get_own_id() ->
  router:show_id().

send_response(Message, Address) ->
  communication_manager:send_message_async(command, [Address, Message], Address, no_alias).

statistics_gather() ->
  statistics:gather().

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  naming_handler:notify_identity(self(), application_manager),
  {ok, #state{}}.


handle_call(get_name, _From, State) ->
  {reply, State#state.name, State};

handle_call({connect, Name}, _From, State) ->
  {reply, ok, State#state{name = Name}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

