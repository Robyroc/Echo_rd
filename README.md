Echo_rd
=====

An Erlang library that implements the chord P2P system.
For more details see there https://pdos.csail.mit.edu/papers/ton:chord/paper-ton.pdf.

Build
-----

    $ rebar3 compile
    
Interface
-----

The library allows to abstract the location of a service and distribute it. All the commands of the service are not changed by Echo_rd, it just routes them to the node handling it correctly.
The library will need a block service providing an interface compatible with gen_bm.erl file.

The full list of methods complete with a brief explanation can be seen in the gen_app.erl file.


Modules
-----

- **application_manager**: it will handle the communication with the upper level (application and block manager);
- **checker**: it will handle the communication with the predecessor on the network. It will also handle the address (and id) of the predecessor;
- **communication_manager**: it will encode and decode incoming/outcoming messages and route them properly;
- **fixer**: it will make lookups in order to update the finger table entries;
- **hash_f**: it will provide methods for the hashing of addresses and resources;
- **join_handler**: it will handle the status of the node regarding join/create/leave;
- **lager_sinks_handler**: it will provide methods for the correct execution of lager logging;
- **link_manager**: it will create/manage tcp sockets;
- **logging_policies**: it will handle the filters on the logging based on the chosen policy;
- **lookup_request**: it will handle a single lookup request waiting for response;
- **naming_handler**: it will handle all naming operations;
- **naming_manager**: it will handle the naming table when naming_handler doesn't exist or is restarting;
- **normalizer**: it will handle all the operations on nodes id;
- **params_handler**: it will handle values needed for the execution after entering a network;
- **request_gateway**: it will handle and create all the lookup_request present;
- **router**: it will handle the finger table and the lookup mechanism;
- **socket_handler**: it will handle a single tcp connection;
- **socket_listener**: it will listen for tcp connections;
- **stabilizer**: it will handle the successor list and handles the communication with the successor;
- **statistics**: it will provide useful metrics to analyze the network.

Configuration
-----

Echo_rd can be configured using 

    $ application:set_env(echo_rd, #VAR#, #VAL#).
    
The following table shows the possible configurations (in square brackets default values):

| #VAR#     | #VAL#       | Meaning                                                                                 |
|-----------|-------------|-----------------------------------------------------------------------------------------|
| ip        | public      | Set this to use public ip address instead of the private one                            |
| ip        | [private]   | Use private ip address                                                                  |
| log       | all         | Set this to use the maximum level of logging                                            |
| log       | logic       | Set this to log only the events strictly related to chord and join mechanisms           |
| log       | comm_only   | Set this to log only the communication events                                           |
| log       | chord_only  | Set this to log only the events strictly related to chord                               |
| log       | joiner_only | Set this to log only the join mechanisms                                                |
| log       | naming_only | Set this to log only the naming operations                                              |
| log       | [undefined] | Log only errors                                                                         |
| lager_log | lager_on    | Logs are written into file and printed on screen (may make it hard to use the terminal) |
| lager_log | lager_only  | Logs are written only on file                                                           |
| lager_log | lager_off   | Logs are written only on terminal (may make it hard to use the terminal)                |
| lager_log | [undefined] | No logging                                                                              |
| delay     | NUMBER      | Add NUMBER ms of latency to communication                                               |
| delay     | [undefined] | No artificial delay                                                                     |

Messages
-----

| Message         | Params                          | Created by             | Forwarded to                  | Notes                                                                                            |
|-----------------|---------------------------------|------------------------|-------------------------------|--------------------------------------------------------------------------------------------------|
| lookup_for_join |                                 | join_handler           | router                        |                                                                                                  |
| lookup_response | ID, Address                     | router                 | request_gateway, join_handler | ID contains the requested id, Address will points to the closest   successor of the requested id |
| ready_for_info  |                                 | join_handler           | join_handler                  |                                                                                                  |
| join_info       | Nbits, SuccessorList, Resources | join_handler           | join_handler                  | Contains all the info needed for the join                                                        |
| ack_info        |                                 | join_handler           | join_handler                  | Confirms the reception of join_info                                                              |
| abort           | Reason                          | join_handler           | join_handler                  | Aborts current join operation                                                                    |
| ack_join        |                                 | join_handler           | join_handler                  | Ends the join operation                                                                          |
| leave_info      | Resources                       | join_handler           | join_handler                  | Contains the resources of the leaving node                                                       |
| ack_leave       |                                 | join_handler           | join_handler                  | Confirms the reception of leave_info                                                             |
| ask_pred        |                                 | stabilizer             | checker                       |                                                                                                  |
| pred_reply      | Predecessor, SuccessorList      | checker                | stabilizer                    | Responds to the ask_pred message                                                                 |
| lookup          | ID, Hops                        | router, lookup_request | router                        | Hops indicates the max number of nodes that can be visited                                       |
| command         | Address, Command                | application_manager    | application_manager           | Address contains the ip address of the node sending the command                                  |
| get_stats       | Number                          | statistics             | statistics                    | Number contains the number of already visited nodes                                              |
| stats           | Statistics                      | statistics             | statistics                    |                                                                                                  |

- 1 is Api (l), Router, Fixer(l)
- 2 is Api (any) 


Truth-Table of the CM's state machine
-----

| ID | Curr_State    | Action         | Re-Action                      | New_State     | 
|----|---------------|----------------|--------------------------------|---------------| 
| a  | init_joiner   | join           | lookup_for_join                | look          | 
| b  | init_joiner   | create         | start                          | init_provider | 
| c  | look          | look_resp      | ready_for_info                 | pre_join      |
| d  | look          | join           |                                | look          | 
| ti | look          | timeout        | hard_stop                      | init_joiner   | 
| e  | pre_join      | info           | ack_info                       | j_ready       | 
| f  | pre_join      | abort          | lookup_for_join                | look          | 
| ti | pre_join      | timeout        | hard_stop                      | init_joiner   | 
| g  | j_ready       | ack_join       | start                          | init_provider | 
| h  | j_ready       | abort          | lookup_for_join                | look          | 
| ti | j_ready       | timeout        | hard_stop                      | init_joiner   | 
| i1 | init_provider | ready_for_info | join_info                      | not_alone     |
| i2 | init_provider | ready_for_info | abort                          | init_provider |  
| j  | init_provider | leave_info     | ack_leave                      | init_provider |
| k  | init_provider | look_resp      |                                | init_provider | 
| l  | init_provider | leave          | leave_info                     | leaving       | 
| m1 | not_alone     | ready_for_info | abort                          | not_alone     | 
| m2 | not_alone     | ready_for_info | abort(curr), join_info(joiner) | not_alone     | 
| n  | not_alone     | ack_info       | ack_join, drop_many_resources  | init_provider | 
| o  | not_alone     | leave          | abort(curr), leave_info        | leaving       |
| p  | not_alone     | leave_info     | abort(curr), ack_leave         | init_provider | 
| q  | not_alone     | look_resp      |                                | not_alone     |
| tj | not_alone     | timeout        | hard_stop                      | init_provider | 
| r  | leaving       | ack_leave      | stop                           | init_joiner   |
| s  | leaving       | leave          |                                | init_joiner   |
| u  | leaving       | look_resp      |                                | leaving       |
| v  | leaving       | ready_for_info | abort                          | leaving       |
| tl | leaving       | timeout        | hard_stop                      | init_joiner   |

Where:
- The **ID** field column the id of the event
- The **Curr_State** column represents the current state, i.e. the state from which the transition comes from
- The **Action** column represents the incoming request 
- The **Re-Action** column represents the state-machine’s response w.r.t. the corresponding action
- The **New_State** column represents the new state after the event

*Observation*:
* For the id of the timeout scenarios is used the following notation:
    * **ti** for the default interval
    * **tj** for the join interval
    * **tl** for the leaving interval
* For the ready_for_info message we have the IDs i1,i2 and m1,m2 according to the priority of the joiner who has to be served.