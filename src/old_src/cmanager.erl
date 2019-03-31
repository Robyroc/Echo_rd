-module(cmanager).
-author("robyroc").

%% API
-export([init/1, prepareMessage/3]).

init(API) ->
  receive
    {API, create_info, Nbits} -> handle_create(Nbits, API);
    {API, join, NodeAddress} -> handle_join(NodeAddress, API);
    {API, exit} -> ok
  end.

handle_create(Nbits, API) ->
  Hash = spawn(fun() -> hash_func(self(), Nbits) end),
  register(hash_f, Hash),
  Address = tcp_manager:get_own_address(),
  h_func ! {self(), a_code, Address},
  receive
    {h_func, code, Index} ->
      LM = spawn(fun() -> tcp_manager:init(self()) end),
      RM = spawn(fun() -> router_manager:init(self(), Nbits, no_pred, [{Index, Address}]) end),
      API ! {self(), creation_result, {success, "Successfully created network"}},
      operating(API, LM, RM, Nbits)
  end.

handle_join(NodeAddress, API) ->
  LM = spawn(fun() -> tcp_manager:init(self()) end),
  RM = spawn(fun() -> router_manager:init(self(), no_nbits, no_pred, wait_for_succ) end),
  LM ! {self(), connect_to, NodeAddress},
  OwnPID = self(),
  receive
    {OwnPID, link, PID} ->
      PID ! {self(), join, no_alias, [no_index, tcp_manager:get_own_address()]}
  after 20000 ->
    API ! {self(), creation_result, {fail, "Timeout on join network"}},
    LM ! {self(), kill},
    RM ! {self(), kill},
    init(API)
  end,
  wait_for_join_data(LM, RM, API).

wait_for_join_data(LM, RM, API) ->
  receive
    {_PID, Method, Address, Params} ->
      Parsed = parse_method(Method),
      case Parsed of
        join_data ->
          [Nbits, Resources, SuccList] = Params,
          Hash = spawn(fun() -> hash_func(self(), Nbits) end),
          register(hash_f, Hash),
          h_func ! {self(), a_code, Address},
          receive
            {h_func, code, Index} ->
              RM ! {self(), send_info, Nbits, Resources, [{Index, Address} | SuccList]},
              API ! {self(), creation_result, {success, "Successfully joined network"}},
              operating(API, LM, RM, Nbits)
          end;
        _ -> wait_for_join_data(LM, RM, API)
      end
  after
    20000 ->
      API ! {self(), creation_result, {fail, "Timeout on join network"}},
      LM ! {self(), kill},
      RM ! {self(), kill},
      init(API)
  end.

hash_func(CManager, Nbits) ->
  receive
    {PID, a_code, Address} ->
      CManager ! {self(), compact_address, Address},
      receive
        {CManager, compacted_address, Compacted} ->
          Digest = crypto:bytes_to_integer(crypto:hash(sha, Compacted)),
          Index = Digest rem round(math:pow(2, Nbits)),
          PID ! {self(), code, Index}
      end;
    {PID, r_code, Resource} ->
      Digest = crypto:bytes_to_integer(crypto:hash(sha, Resource)),
      Index = Digest rem round(math:pow(2, Nbits)),
      PID ! {self(), code, Index}
  end,
  hash_func(CManager, Nbits).

operating(ApplicationManager, LinkManager, RouterManager, Nbits) -> ok.         %TODO implement main operating loop

prepareMessage(Address, Method, Params) -> ok.          %TODO implement this method that will allow to prepare msg for link manager

parse_method(Method) -> ok.                    %TODO implement this method