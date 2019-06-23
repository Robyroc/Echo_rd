-module(gen_bm).
-author("Giacomo").

-type ip_port() :: integer().
-type ip() :: {integer(), integer(), integer(), integer()}.
-type address() :: {ip_port(), ip()}.
-type resource() :: {string(), binary()}.
-type resource_list() :: [] | [resource() | resource_list()].

%%--------------------------------------------------------------------
%% @doc
%% Receive a message from another Block Manager
%% where
%%      From: is the address of the Block Manager's node
%%      Command: is the message codified in binary
%% return
%%       ok -> when the message has been successfully received by the Block Manager
%%
%% @end
%%--------------------------------------------------------------------
-callback receive_command(From :: address(), Command :: binary()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Add a list of resources to the Block Manager of the node, usually called when the node
%% just entered in the network, so the list represents the resources that belonged to the successor; or when
%% the predecessor is leaving, so it sends its resources to its successor.
%% where
%%      Resources: is the list of the resources that has to be passed to the Block Manager
%% return
%%       ok -> when the message has been successfully received by the Block Manager
%%
%% @end
%%--------------------------------------------------------------------
-callback add_many_resources(Resources :: resource_list()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Get the list of all the resources locally stored by the the Block Manager of the node.
%% Usually it's called when the node is leaving, so the resources have to be passed to the successor, or when
%% a new predecessor just entered in the network.
%% where
%%      From: (From = all_res) if the Block Manager has to get all the local resources
%%            (From = Predecessor's ID) if the Block Manager has to get just the resources
%%            whose id is <= than Predecessor's ID
%% return
%%       Resources -> the list of the resources
%%
%% @end
%%--------------------------------------------------------------------
-callback get_local_resources(From :: integer() | all_res) -> Resources :: resource_list().

%%--------------------------------------------------------------------
%% @doc
%% Drop the list of all the resources locally stored by the the Block Manager of the node.
%% Usually it's called when the node is leaving, so the resources have been passed to the successor, or when
%% a new predecessor just entered in the network.
%% where
%%      From: (From = all_res) if the Block Manager has to drop all the local resources
%%            (From = Predecessor's ID) if the Block Manager has to drop just the resources
%%            whose id is <= than Predecessor's ID
%% return
%%       ok ->  when the list of the resources has been dropped
%%
%% @end
%%--------------------------------------------------------------------
-callback drop_many_resources(From :: integer() | all_res) -> ok.
