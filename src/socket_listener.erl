-module(socket_listener).
-author("robyroc").
-behaviour(gen_server).

%% API
-export([start_link/0,
  check_availability/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check_availability(Port) ->
  case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}])
  of
    {ok, Sock} ->
      ok = gen_tcp:close(Sock);
    {error, Reason} ->
      Reason
end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  self() ! startup,
  {ok, #state{}}.


handle_call(Request, _From, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Listen: Unexpected call message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("Listen: Unexpected call message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("Listen: Unexpected call message: ~p\n", [Request]);
    _ -> ok
  end,
  {reply, ok, State}.


handle_cast(Request, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Listen: Unexpected cast message: ~p\n", [Request]);
    {lager_only, _} ->
      lager:error("Listen: Unexpected cast message: ~p\n", [Request]);
    {lager_off, _} ->
      io:format("Listen: Unexpected cast message: ~p\n", [Request]);
    _ -> ok
  end,
  {noreply, State}.


handle_info(loop, State) ->
  {ok, Socket} = gen_tcp:accept(State#state.socket),
  link_manager:move_socket(Socket),
  link_manager:incoming_connection(Socket),
  erlang:send_after(10, self(), loop),
  {noreply, State};

handle_info(startup, _State) ->
  naming_handler:wait_service(link_manager),
  Port = naming_handler:get_identity(port),
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]),
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:info("Listening at port ~p\n", [Port]);
    {lager_only, _} ->
      lager:info("Listening at port ~p\n", [Port]);
    {lager_off, _} ->
      io:format("Listening at port ~p\n", [Port]);
    _ -> io:format("Listening at port ~p\n", [Port])
  end,
  naming_handler:notify_identity(self(), listener),
  erlang:send_after(10, self(), loop),
  {noreply, #state{socket = Listen}};

handle_info(Info, State) ->
  case logging_policies:check_lager_policy(?MODULE) of
    {lager_on, _} ->
      lager:error("Listen: Unexpected ! message: ~p\n", [Info]);
    {lager_only, _} ->
      lager:error("Listen: Unexpected ! message: ~p\n", [Info]);
    {lager_off, _} ->
      io:format("Listen: Unexpected ! message: ~p\n", [Info]);
    _ -> ok
  end,
  {noreply, State}.


terminate(_Reason, State) ->
  gen_tcp:close(State#state.socket),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

