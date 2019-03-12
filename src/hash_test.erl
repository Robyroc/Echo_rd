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
-export([launch/0]).

launch() ->
  X = [lists:flatten([integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".", integer_to_list(D),
    ":", integer_to_list(6543)]) || A <- [192], B <- lists:seq(0, 255), C <- lists:seq(0, 255),
    D <- lists:seq(0,255)],
  Z = lists:map(fun(Y) -> crypto:hash(sha, Y) end, X),
  I = lists:map(fun(K) -> crypto:bytes_to_integer(K) end, Z),
  M = lists:map(fun(F) -> F rem 128 end, I),
  Res = [{I, length([A || A <- M, A =:= I])} || I <- lists:seq(0, 127)],
  Res = [io:format("~p      ~p~n",[I, length([A || A <- M, A =:= I])]) || I <- lists:seq(0, 127)].
