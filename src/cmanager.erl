-module(cmanager).
-author("robyroc").

%% API
-export([init/1]).

init(API) ->
  receive
    {API, create_info, Nbits} -> handle_create(Nbits, API);
    {API, join, NodeAddress} -> handle_join(NodeAddress, API);
    {API, exit} -> ok
  end.

handle_create(Nbits, API) ->
  LM = spawn(fun() -> tcp_manager:init(self()) end),
  RM = spawn(fun() -> router_manager:init(self(), Nbits, no_pred, [tcp_manager:get_own_address()]) end),
  API ! {self(), creation_result, {success, "Successfully created network"}},
  operating(API, LM, RM, Nbits).

handle_join(NodeAddress, API) ->
  LM = spawn(fun() -> tcp_manager:init(self()) end),
  RM = spawn(fun() -> router_manager:init(self(), no_nbits, no_pred, wait_for_succ) end),
  LM ! {self(), connect_to, NodeAddress},
  OwnPID = self(),
  receive
    {OwnPID, link, PID} ->
      PID ! {self(), join, no_alias, [no_index, tcp_manager:get_own_address()]}
  end,
  wait_for_join_data(LM, RM, API).

wait_for_join_data(LM, RM, API) ->
  receive
    {_PID, Method, Address, Params} ->
      Parsed = parse_method(Method),
      case Parsed of
        join_data ->
          [Nbits, Resources, SuccList] = Params,
          RM ! {self(), send_info, Nbits, Resources, [Address | SuccList]},
          API ! {self(), creation_result, {success, "Successfully joined network"}},
          operating(API, LM, RM, Nbits);
        _ -> wait_for_join_data(LM, RM, API)
      end
  after
    20000 ->
      API ! {self(), creation_result, {fail, "Timeout on join network"}},
      LM ! {self(), kill},
      RM ! {self(), kill},
      init(API)
  end.

operating(ApplicationManager, LinkManager, RouterManager, Nbits) -> ok.         %TODO implement main operating loop


parse_method(Method) -> ok.                    %TODO implement this method