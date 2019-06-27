-module(gen_app).
-author("robyroc").

-type ip_port() :: integer().
-type ip() :: {integer(), integer(), integer(), integer()}.
-type address() :: {ip_port(), ip()}.
-type successor_entry() :: {integer(), address()}.
-type successor_list() :: [] | [successor_entry() | successor_list()].

%%--------------------------------------------------------------------
%% @doc
%% Join a Chord network through an address, with 6543 as the default port
%% where
%%      Address: is the address of a node already present in the Chord network
%% return
%%       ok -> when it successfully joins the network
%%       fail -> when it encounters some problem in the communication during the join in the network
%%       used_id -> when the node's id is already present in the network
%%       {error, econnrefused} -> when the connection is refused
%%       {error, timeout} -> when the address is wrong or it doesn't belong to a Chord node
%%       term() -> when it encounters some problem in entering in the network
%%
%% @end
%%--------------------------------------------------------------------
-callback join(Address :: address()) -> ok |
                                        fail |
                                        used_id |
                                        {error, econnrefused} |
                                        {error, timeout} |
                                         term().

%%--------------------------------------------------------------------
%% @doc
%% Join a Chord network through an address, choosing a specific port
%% where
%%      Port: is a specific port used to create a socket in order to connect to the node
%%      Address: is the address of a node already present in the Chord network
%% return
%%       ok -> when it successfully joins the network
%%       fail -> when it encounters some problem in the communication during the join in the network
%%       used_id -> when the node's id is already present in the network
%%       {error, econnrefused} -> when the connection is refused
%%       {error, timeout} -> when the address is wrong or it doesn't belong to a Chord node
%%       term() -> when it encounters some problem in entering in the network
%%
%% @end
%%--------------------------------------------------------------------
-callback join_p(Port :: ip_port(), Address :: address()) ->  ok |
                                                              fail |
                                                              used_id |
                                                              {error, econnrefused} |
                                                              {error, timeout} |
                                                              term().

%%--------------------------------------------------------------------
%% @doc
%% Create a new Chord network with a specific number of bits
%% where
%%      Nbits: is the number of bits of the new Chord network
%% return
%%       ok -> when it successfully creates the network
%%       fail -> when it encounters some problem in creating in the network
%%
%% @end
%%--------------------------------------------------------------------
-callback create(Nbits :: integer()) -> ok | fail.

%%--------------------------------------------------------------------
%% @doc
%% Create a new Chord network with a specific number of bits, choosing a specific port
%% where
%%      Port: is a specific port used to create a socket in order to connect to future nodes
%%      Nbits: is the number of bits of the new Chord network
%% return
%%       ok -> when it successfully creates the network
%%       fail -> when it encounters some problem in creating in the network
%%
%% @end
%%--------------------------------------------------------------------
-callback create_p(Port :: ip_port(), NBits :: integer()) -> ok | fail.

%%--------------------------------------------------------------------
%% @doc
%% Leave a Chord network
%% return
%%       ok -> when it successfully left the network
%%
%% @end
%%--------------------------------------------------------------------
-callback leave() -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Send a specific message to a Block Manager, in particular to the one that has
%% the key related to the Name passed as a parameter
%% where
%%      Name: is the name of the file, on which any operation can be done,
%%            depending on the implementation of the Block Manager
%%      Command: is the specific message to send to the Block Manager related to the specific resource
%% return
%%       ok -> when it successfully sends the message to the right node
%%       out_of_network -> when it hasn't joined a Chord network
%%
%% @end
%%--------------------------------------------------------------------
-callback issue_command(Name :: string(), Command :: binary()) -> ok | out_of_network.

%%--------------------------------------------------------------------
%% @doc
%% Transform a String using SHA-1
%% where
%%      Name: is the specific string, whose hash is to be encoded
%% return
%%       integer() -> the hashed name of the Name passed as a parameter
%%
%% @end
%%--------------------------------------------------------------------
-callback hash_name(Name :: string()) -> integer().

%%--------------------------------------------------------------------
%% @doc
%% Send a response message to a Block Manager, that previously sent a message
%% using issue_command
%% where
%%      Message: is the response
%%      Address: is the address of the node whose Block Manager previously sent an issue_command
%% return
%%       ok -> when the message has been successfully sent to the other node
%%
%% @end
%%--------------------------------------------------------------------
-callback send_response(Message :: binary(), Address :: address()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Get the successor list of the node
%% return
%%       successor_list() -> the list of nodes present in the successor list
%%
%% @end
%%--------------------------------------------------------------------
-callback get_successor_list() -> successor_list().

%%--------------------------------------------------------------------
%% @doc
%% Print the finger tale of the node
%% return
%%        ok -> when the finger table has been printed.
%%
%% @end
%%--------------------------------------------------------------------
-callback show_finger_table() -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Get the predecessor of the node
%% return
%%       address() -> the address of the predecessor of the node
%%
%% @end
%%--------------------------------------------------------------------
-callback get_predecessor() -> address().

%%--------------------------------------------------------------------
%% @doc
%% Get the Chord id of the node
%% return
%%       integer() -> the Chord id of the node
%%
%% @end
%%--------------------------------------------------------------------
-callback get_own_id() -> integer().

%%--------------------------------------------------------------------
%% @doc
%% Collect the statistics (Join time, Highest lookup time, Average lookup time,
%% Average lookup length, Number of lookup timeouts, FTable last refresh timings) of network
%% return
%%       ok -> all the statistics of the node have been collected and the message has been forwarded to the successor
%%
%% @end
%%--------------------------------------------------------------------
-callback statistics_gather() -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Create the connection between Application Manager and the Block Manager, required to handle resources
%% where
%%      Name: is the name of the module of the Block Manager
%% return
%%       ok -> when the Application Manager successfully saved the name in its State
%%
%% @end
%%--------------------------------------------------------------------
-callback connect(Name :: atom()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Get the Average lookup time of the node
%% return
%%       integer() -> the Average lookup time of the node of the node
%%
%% @end
%%--------------------------------------------------------------------
-callback get_average_lookup_time() -> integer().

