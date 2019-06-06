-module(gen_bm).
-author("Giacomo").

-type ip_port() :: integer().
-type ip() :: {integer(), integer(), integer(), integer()}.
-type address() :: {ip_port(), ip()}.
-type resource() :: {string(), binary()}.
-type resource_list() :: [] | [resource() | resource_list()].

-callback receive_command(From :: address(), Command :: binary()) -> ok.
-callback add_many_resources(Resources :: resource_list()) -> ok.
-callback get_local_resources(From :: integer()) -> Resources :: resource_list().
-callback drop_many_resources(From :: integer()) -> ok.
