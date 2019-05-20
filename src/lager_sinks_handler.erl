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
-export([start_link/0, create_log_sink/3, create_console_sink/2]).
-export([test_file/0, test_console_error/0, test_logging/0, test_console_info/0]).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_info/2,
  terminate/2,
  code_change/3,
  handle_cast/2]).

-define(SERVER, ?MODULE).

-record(state, {path}).

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

test_file() ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:cast(PID, test_file).

test_logging() ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:cast(PID, test_loggisacng).

test_console_info() ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:cast(PID, test_console_info).

test_console_error() ->
  PID = naming_handler:get_identity(lager_sinks_handler),
  gen_server:cast(PID, test_console_info).

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
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
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

%TODO these following 4 cast have to been deleted, used for testing
handle_cast(test_file, State) ->
  provaLog:info("Questa è una prova info"),
  {noreply,State};
handle_cast(test_logging, State) ->
  create_log_sink(testLog, error, "prova2"),
  testLog:error("Questa è una prova error"),
  {noreply,State};
handle_cast(test_console_error, State) ->
  create_console_sink(testLog, error),
  provaConsoleError:error("Prova error Console"),
  {noreply,State};
handle_cast(test_console_info, State) ->
  create_console_sink(testLog, info),
  provaConsoleInfo:info("Prova info Console"),
  {noreply,State};


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
  {noreply,State}.




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
  naming_handler:notify_identity(self(), lager_sinks_handler),
  {noreply, #state{path = Path}}.


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
    A -> A
  end.

