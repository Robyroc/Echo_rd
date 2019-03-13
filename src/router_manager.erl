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
      Fixer = spawn(fun() -> fixer(self(), ID, 0, Nbits) end),
      Stabilizer = spawn(fun() -> stabilizer(SuccList) end),
      Checker = spawn(fun() -> checker(self(), Predecessor, no_ID, ID, Nbits) end),
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

fixer(Router, ID, Index, Nbits) when Index >= Nbits ->
  fixer(Router, ID, 0, Nbits);

fixer(Router, ID, Index, Nbits) ->
  Theoretical = math:pow(2, Index) + ID,
  Router ! {self(), lookup, local_address, Theoretical},
  receive
    {Router, lookup_response, Address} ->
      Router ! {self(), ftable_update, Theoretical, Address},
      receive
        {Router, kill} -> ok
      after 20000 ->
        flush(),
        fixer(Router, ID, Index + 1, Nbits)
      end;
    {Router, kill} -> ok
  end.

flush() ->
  receive
    _ -> flush()
  after 0 -> ok
  end.

stabilizer(SuccessorList) -> ok.

checker(Router, Predecessor, no_ID, ID, Nbits) ->
  hash_f ! {self(), a_code, Predecessor},
  receive
    {hash_f, code, Index} when Index =< ID ->
      checker(Router, Predecessor, Index, ID, Nbits);
    {hash_f, code, Index} when Index > ID ->
      checker(Router, Predecessor, Index - math:pow(2, Nbits), ID, Nbits)
  end;

checker(Router, Predecessor, PredecessorID, ID, Nbits) ->
  receive
    {Router, pred_find, local_address} ->
      Router ! {self(), pred_tell, Predecessor},
      checker(Router, Predecessor, PredecessorID, ID, Nbits);
    {Router, pred_find, Address} ->
      case Predecessor of
          nil ->
            Router ! {self(), pred_tell, Address},
            checker(Router, Address, no_ID, ID, Nbits);
          Predecessor ->
            hash_f ! {self(), a_code, Address},
            receive
              {hash_f, code, Index} when Index =< ID ->
                AddressID = Index,
                predecessor_chooser(AddressID,  PredecessorID, Address, Predecessor, Router, ID, Nbits);
              {hash_f, code, Index} when Index > ID ->
                AddressID = Index - math:pow(2, Nbits),
                predecessor_chooser(AddressID,  PredecessorID, Address, Predecessor, Router, ID, Nbits)
            end
      end
  after 30000 ->
    checker(Router, nil, nil, ID, Nbits)
  end.

predecessor_chooser(AddressID,  PredecessorID, Address, Predecessor, Router, ID, Nbits) ->
  case AddressID of
    _ when AddressID > PredecessorID ->
      Router ! {self(), pred_tell, Address},
      checker(Router, Address, AddressID, ID, Nbits);
    _ when AddressID =< PredecessorID ->
      Router ! {self(), pred_tell, Predecessor},
      checker(Router, Predecessor, PredecessorID, ID, Nbits)
  end.

