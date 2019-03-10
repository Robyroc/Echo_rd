-module(router_manager).
-author("robyroc").

%% API
-export([init/4]).



init(CManager, Nbits, Predecessor, SuccList) ->     %TODO all router must be implemented
  Hash = spawn(fun() -> hash_func(CManager, Nbits) end),
  register(hash_f, Hash),
  CManager ! {self(),get_address},
  receive
    {CManager, address, Address} ->
      hash_f ! {self(), a_code, Address}
  end,
  receive
    {hash_f, code, ID} ->
      Fixer = spawn(fun() -> fixer(ID, 0) end),
      Stabilizer = spawn(fun() -> stabilizer(SuccList) end),
      Checker = spawn(fun() -> checker(Predecessor) end),
      FingerTable = [{ID + math:pow(2, Exp), no_ID, no_handler, no_address} || Exp <- lists:seq(0, Nbits - 1)],
      router(CManager, ID, FingerTable, Fixer, Stabilizer, Checker, [])
  end.

router(CManager, ID, [{IDSucc, no_ID,_,_} | Tail], Fixer, Stabilizer, Checker, Requests) ->
  Stabilizer ! {self(), ask_successor},
  receive
    {Stabilizer, successor, IDSuccessor, Address} ->
      CManager ! {self(), connect_to, Address},
      receive
        {CManager, link, Handler} ->
          router(CManager, ID, [{IDSucc, IDSuccessor, Handler, Address} | Tail], Fixer, Stabilizer, Checker, Requests)
      end
  end;

router(CManager, ID, FingerTable, Fixer, Stabilizer, Checker, Requests) -> ok.

hash_func(CManager, Nbits) ->               %TODO move to CManager
  receive
    {PID, a_code, Address} ->
      CManager ! {self(), compact_address, Address},
      receive
        {CManager, compacted_address, Compacted} ->
          Digest = crypto:hash(sha, Compacted),
          PID ! {self(), code, Digest}
      end;
    {PID, r_code, Resource} ->
      Digest = crypto:hash(sha, Resource),
      PID ! {self(), code, Digest}
  end,
  hash_func(CManager, Nbits).


fixer(ID, Index) -> ok.

stabilizer(SuccessorList) -> ok.

checker(Predecessor) -> ok.