%%%-------------------------------------------------------------------
%%% @author Antonio
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mag 2019 16:57
%%%-------------------------------------------------------------------
-module(lager_sinks_handler).
-author("Antonio").

-behaviour(gen_server).

%% API
-export([start_link/0,
  create_log_sink/3,
  create_console_sink/2,
  create_chord_sinks/0,
  start_if_not_started/0,
  terminate_if_not_terminated/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_info/2,
  terminate/2,
  code_change/3,
  handle_cast/2]).

-define(SERVER, ?MODULE).

-record(state, {path, started}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_log_sink(Name, Level, FileName) ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:cast(PID, {create_log_sink, Name, Level, FileName}).

create_console_sink(Name, Level) ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:cast(PID, {create_console_sink, Name, Level}).

create_chord_sinks() ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:cast(PID, create_chord_sinks).

start_if_not_started() ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:call(PID, start_if_not_started).

terminate_if_not_terminated() ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:call(PID, terminate_if_not_terminated).

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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  self() ! startup,
  {ok, #state{path = udefine, started = not_started}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
%%    State :: #state{}) ->
%%  {reply, Reply :: term(), NewState :: #state{}} |
%%  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
%%  {noreply, NewState :: #state{}} |
%%  {noreply, NewState :: #state{}, timeout() | hibernate} |
%%  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
%%  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(start_if_not_started, _From, State) when State#state.started =:= not_started ->
  lager:start(),
  {reply, ok, State#state{started = started}};

handle_call(start_if_not_started, _From, State) ->
  {reply, ok, State};

handle_call(terminate_if_not_terminated, _From, State) when State#state.started =:= started ->
  lager:stop(),
  {reply, ok, State#state{started = not_started}};

handle_call(terminate_if_not_terminated, _From, State) ->
  {reply, ok, State};

handle_call(_, _, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(handle_cast(Request :: term(), State :: #state{}) ->
%%  {noreply, NewState :: #state{}} |
%%  {noreply, NewState :: #state{}, timeout() | hibernate} |
%%  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({create_log_sink, Name, Level, FileName}, State) ->
  case check_level(Level) of
      ok ->
        HandlerName = list_to_atom(atom_to_list(Name) ++ "_lager_event"),
        LogPath = State#state.path ++ "/" ++ FileName ++ ".log",
        lager_app:configure_sink(
          HandlerName,
          [{handlers,
            [
              {lager_file_backend, [{file, [LogPath]}, {level, Level}, {formatter, lager_default_formatter},
                {formatter_config, [time," [",severity,"] ",pid, " ", message, "\n"]}]}
            ]
          }]);
      _ -> undefined_level
    end,
  {noreply,State};

handle_cast({create_console_sink, Name, Level}, State) ->
  case check_level(Level) of
    ok ->
      HandlerName = list_to_atom(atom_to_list(Name) ++ "_lager_event"),
      lager_app:configure_sink(
        HandlerName,
        [{handlers,
          [
            {lager_console_backend, [{level, Level}]}
          ]
        }]
      );
    _ -> undefined_level
  end,
  {noreply,State};


handle_cast(create_chord_sinks, State) ->
  create_log_sink(joinerLager,info, "joiner"),
  create_log_sink(inout,info, "inout"),
  create_log_sink(fixerLager,info, "fixer"),
  create_log_sink(checkerLager,info, "checker"),
  create_log_sink(routerLager,info, "router"),
  create_console_sink(lagerConsole, info),
  {noreply,State};

handle_cast(_, State) ->
  {norpely, State}.





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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(startup, _State) ->
  Port = naming_handler:get_identity(port),
  {ok, Directory} = file:get_cwd(),
  Path = Directory ++ "/log/" ++ integer_to_list(Port) ++ "_logging",
  application:set_env(lager, log_root, Path),
  application:set_env(echo_rd, lager_log, lager_on),
  application:set_env(echo_rd, log, all),

  %TODO check if it is useful
  lager:start(),

  naming_handler:notify_identity(self(), lager_sinks_handler),
  {noreply, #state{path = Path, started = started}};

handle_info(_, State) ->
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_level(Level) ->
  case Level of
    info -> ok;
    warning -> ok;
    error -> ok;
    _ -> no_ok
  end.

