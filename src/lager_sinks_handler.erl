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

init([]) ->
  self() ! startup,
  {ok, #state{path = udefine, started = not_started}}.


handle_call(start_if_not_started, _From, State) when State#state.started =:= not_started ->
  lager:start(),
  create_chord_sinks(),
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


handle_info(startup, _State) ->
  naming_handler:wait_service(port),
  Port = naming_handler:get_identity(port),
  {ok, Directory} = file:get_cwd(),
  Path = Directory ++ "/log/" ++ integer_to_list(Port) ++ "_logging",
  application:set_env(lager, log_root, Path),

  naming_handler:notify_identity(self(), lager_sinks_handler),
  {noreply, #state{path = Path, started = not_started}};

handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.


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

