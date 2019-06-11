-module(gen_app).
-author("robyroc").

-type ip_port() :: integer().
-type ip() :: {integer(), integer(), integer(), integer()}.
-type address() :: {ip_port(), ip()}.
-type successor_entry() :: {integer(), address()}.
-type successor_list() :: [] | [successor_entry() | successor_list()].

%TODO comment these callbacks showing the meaning of the parameters and check return values.

-callback join(Address :: address()) -> ok | fail.
-callback join_p(Port :: ip_port(), Address :: address()) -> ok | fail.
-callback create(Nbits :: integer()) -> ok | fail.
-callback create_p(Port :: ip_port(), NBits :: integer()) -> ok | fail.
-callback leave() -> ok.
-callback issue_command(Name :: string(), Command :: binary()) -> ok | out_of_network.
-callback hash_name(Name :: string()) -> integer().
-callback send_response(Message :: binary(), Address :: address()) -> ok.
-callback get_successor_list() -> successor_list().
-callback show_finger_table() -> ok.
-callback get_predecessor() -> address().
-callback get_own_id() -> integer().
-callback statistics_gather() -> ok.
-callback connect(Name :: atom()) -> ok.
-callback get_average_lookup_time() -> integer().

