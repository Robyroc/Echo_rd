-module(network_control).
-author("robyroc").

-behaviour(gen_server).

%% API
-export([start_link/0, command/1, command_response/0, command_incoming/3, run_command/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {response_number, number_of_nodes, caller}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

command(Command) ->
  PID = naming_handler:get_identity(network_control),
  gen_server:call(PID, {command, Command}).

command_response() ->
  PID = naming_handler:get_identity(network_control),
  gen_server:cast(PID, command_response).

command_incoming(From, Command, Number) ->
  PID = naming_handler:get_identity(network_control),
  gen_server:cast(PID, {command_incoming, From, Command, Number}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  self() ! startup,
  {ok, #state{}}.

handle_call({command, Command}, From, State) ->
  {_, Succ} = stabilizer:get_successor(),
  communication_manager:send_message_async(net_command, [1, Command], Succ, no_alias),
  run_command(Command),
  {noreply, State#state{response_number = 1, caller = From}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({command_incoming, From, Command, Number}, State) ->
  OwnAddress = link_manager:get_own_address(),
  case From of
    OwnAddress ->
      NewState = possible_response(State#state{number_of_nodes = Number}),
      {noreply, NewState};
    _ ->
      {_, Succ} = stabilizer:get_successor(),
      communication_manager:send_message_async(net_command, [Number + 1, Command], Succ, From),
      run_command(Command),
      communication_manager:send_message_async(net_command_response, [], From, no_alias),
      {noreply, State}
  end;

handle_cast(command_response, State) ->
  case State#state.number_of_nodes of
    undefined -> {noreply, State#state{response_number = State#state.response_number + 1}};
    _ ->
      NewState = possible_response(State#state{response_number = State#state.response_number + 1}),
      {noreply, NewState}
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(startup, State) ->
  naming_handler:wait_service(stabilizer),
  naming_handler:notify_identity(self(), network_control),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run_command(Commands) ->
  CommandsList = re:split(Commands, "[.]", [{return,list}]),
  LineSep = io_lib:nl(),
  AdjustedCommandsList = [string:join(CommandsList, "." ++ LineSep) ++ "."],
  {ok, Directory} = file:get_cwd(),
  Port = naming_handler:get_identity(port),
  Path = Directory ++ "/" ++ integer_to_list(Port) ++ "_temp.tmp",
  file:write_file(Path, AdjustedCommandsList),
  file:script(Path),
  file:delete(Path),
  ok.

possible_response(State) when State#state.response_number =:= State#state.number_of_nodes ->
  gen_server:reply(State#state.caller, State#state.number_of_nodes),
  #state{};

possible_response(State) ->
  State.