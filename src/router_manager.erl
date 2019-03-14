-module(router_manager).
-author("robyroc").

%% API
-export([init/4]).



init(CManager, Nbits, Predecessor, SuccList) ->
  CManager ! {self(),get_address},
  receive
    {CManager, address, Address} ->
      hash_f ! {self(), a_code, Address}
  end,
  receive
    {hash_f, code, ID} ->
      Fixer = spawn(fun() -> fixer(self(), ID, 0, Nbits) end),
      Stabilizer = spawn(fun() -> stabilizer(not_ready, SuccList, self(), ID, Nbits) end),
      Checker = spawn(fun() -> checker(self(), Predecessor, no_ID, ID, Nbits) end),
      FingerTable = [{ID + round(math:pow(2, Exp)), no_ID, no_handler, no_address} || Exp <- lists:seq(0, Nbits - 1)],
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

router(CManager, ID, FingerTable, Fixer, Stabilizer, Checker, Requests) -> ok.    %TODO implement me plz

fixer(Router, ID, Index, Nbits) when Index >= Nbits ->
  fixer(Router, ID, 0, Nbits);

fixer(Router, ID, Index, Nbits) ->
  Theoretical = round(math:pow(2, Index)) + ID,
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

hash_address(Address) ->
  hash_f ! {self(), a_code, Address},
  receive
    {hash_f, code, Index} ->
      Index
  end.

stabilizer(not_ready, SuccessorList, Router, ID, NBits) ->
  CutList = cut_last_element(SuccessorList, NBits),
  stabilizer(not_ready, CutList, Router, ID, NBits),
  Smaller = [{I + round(math:pow(2, NBits)), A} || {I, A} <- SuccessorList, I =< ID],
  Corrected = [{I, A} || {I, A} <- SuccessorList, I > ID] ++ Smaller,
  stabilizer(ready, lists:sort(Corrected), Router, ID, NBits);

stabilizer(ready, SuccessorList, Router, ID, NBits) ->
  {_, A} = hd(SuccessorList),
  Router ! {self(), connect_to, A},
  receive
    {_, link, PID} ->
      spawn_link(fun() -> wakeup(self(), 20000) end),
      stabilizer(running, SuccessorList, Router, ID, NBits, PID)
  end.

stabilizer(running, SuccessorList, Router, ID, NBits, PID) ->
  receive
    wakeup ->
      PID ! {self(), pred_find, no_alias, []},
      receive
        {Router, pred_tell, Address} ->
          Index = hash_address(Address),
          HeadIndex = hd([I || {I, _} <- SuccessorList]),
          NewSuccessorList = handle_pred_tell(Index, ID, HeadIndex, SuccessorList, Address, NBits),
          stabilizer(running, NewSuccessorList, Router, ID, NBits, PID)
      end,
      back_to_sleep();
    {Router, ask_successor} ->
      {I, A} = hd(SuccessorList),
      Router ! {self(), successor, I, A};
    {Router, get_succ_list} ->
      Router ! {self(), successor_list, SuccessorList}
  end.

handle_pred_tell(Index, ID, HeadIndex, SuccessorList, Address, NBits) when Index > ID and Index < HeadIndex ->
  update_successor_list(SuccessorList, {Index, Address}, NBits);

handle_pred_tell(Index, ID, _HeadIndex, SuccessorList, _Address, _NBits) when Index =:= ID ->
  SuccessorList;                        %TODO get succ_list from successor

handle_pred_tell(Index, ID, HeadIndex, SuccessorList, Address, NBits) when Index < ID ->
  handle_pred_tell(Index + round(math:pow(2, NBits)), ID, HeadIndex, SuccessorList, Address, NBits).



cut_last_element(SuccessorList, NBits) ->
  Length = length(SuccessorList),
  case Length of
    Length when Length > NBits ->
      [_ | T] = lists:reverse(SuccessorList),
      lists:reverse(T);
    Length when Length =< NBits ->
      SuccessorList
  end.

update_successor_list(SuccessorList, NewElem, NBits) ->
  Cut = cut_last_element(SuccessorList, NBits),
  [NewElem | Cut].

wakeup(PID, Time) ->
  receive
  after Time ->
    PID ! wakeup,
    wakeup(PID, Time)
  end.

back_to_sleep() ->
  receive
    wakeup -> back_to_sleep()
  after 0 -> ok
  end.

checker(Router, Predecessor, no_ID, ID, Nbits) ->
  hash_f ! {self(), a_code, Predecessor},
  receive
    {hash_f, code, Index} when Index =< ID ->
      checker(Router, Predecessor, Index, ID, Nbits);
    {hash_f, code, Index} when Index > ID ->
      checker(Router, Predecessor, Index - round(math:pow(2, Nbits)), ID, Nbits)
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
                AddressID = Index - round(math:pow(2, Nbits)),
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
