-module(public_ip).
-author("robyroc").

%% API
-export([get_public_ip/0]).

get_public_ip() ->
  inets:start(),
  {ok, {_, _, IP}} = httpc:request(get, {"http://myexternalip.com/raw", []}, [], []),
  list_to_tuple(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(IP, "."))).
