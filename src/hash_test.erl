%%%-------------------------------------------------------------------
%%% @author mrbo9
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Mar 2019 11:06
%%%-------------------------------------------------------------------
-module(hash_test).
-author("robyroc").

%% API
-export([launch_same_port/1, port_from_id/2, launch_different_port/1, id_from_port/2]).

launch_same_port(NBits) ->
  X = [lists:flatten([integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".", integer_to_list(D),
    ":", integer_to_list(6543)]) || A <- [192, 5], B <- lists:seq(0, 255), C <- lists:seq(0, 255),
    D <- lists:seq(0,255)],
  Z = lists:map(fun(Y) -> crypto:hash(sha, Y) end, X),
  I = lists:map(fun(K) -> crypto:bytes_to_integer(K) end, Z),
  M = lists:map(fun(F) -> F rem round(math:pow(2, NBits)) end, I),
  [io:format("~p      ~p~n",[In, length([A || A <- M, A =:= In])]) || In <- lists:seq(0, round(math:pow(2, NBits))-1)].

launch_different_port(NBits) ->
  {ok, Addrs} = inet:getif(),
  IP = hd([Addr || {Addr, _,_} <- Addrs, size(Addr) == 4, Addr =/= {127,0,0,1}]),
  {IPA, IPB, IPC, IPD} = IP,
  X = [lists:flatten([integer_to_list(IPA), ".", integer_to_list(IPB), ".", integer_to_list(IPC), ".", integer_to_list(IPD),
    ":", integer_to_list(Port)]) || Port <- lists:seq(0, 65535)],
  Z = lists:map(fun(Y) -> crypto:hash(sha, Y) end, X),
  I = lists:map(fun(K) -> crypto:bytes_to_integer(K) end, Z),
  M = lists:map(fun(F) -> F rem round(math:pow(2, NBits)) end, I),
  [io:format("~p      ~p~n",[In, length([A || A <- M, A =:= In])]) || In <- lists:seq(0, round(math:pow(2, NBits))-1)].

id_from_port(Port, NBits) ->
  {ok, Addrs} = inet:getif(),
  IP = hd([Addr || {Addr, _,_} <- Addrs, size(Addr) == 4, Addr =/= {127,0,0,1}]),
  {IPA, IPB, IPC, IPD} = IP,
  Address = lists:flatten([integer_to_list(IPA), ".", integer_to_list(IPB), ".", integer_to_list(IPC), ".", integer_to_list(IPD),
    ":", integer_to_list(Port)]),
  Hashed = crypto:hash(sha, Address),
  crypto:bytes_to_integer(Hashed) rem round(math:pow(2, NBits)).

port_from_id(Id, NBits) ->
  port_from_id(Id, NBits, 0).

port_from_id(Id, NBits, Curr) ->
  case id_from_port(Curr, NBits) of
    Id -> Curr;
    _ -> port_from_id(Id, NBits, Curr + 1)
  end.