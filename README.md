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

The application can send messages to the library with the following atoms: 
- create, will create a new Chord network. It needs two parameters, i.e. the number of bits of the indexes of the network and the port number. The default port number is 6543.
- join, will enter a network. Needs also the port number and an address of a node part of the interested network. The default port number is 6543.
- leave, will leave (softly) the network.
- service_command, will perform a command on a resource. Needs the index of the resource and the complete command that can be handled by the service. 


Modules
-----

- ApplicationManager/API/chord: it will handle the communication with the upper level (application and service manager).
- RouterManager: it will handle all the routing operations and all the other tasks needed to keep routing consistently.
- CommunicationManager: it will handle all the messages received from the network.
- LinkManager: it will handle the communication with the lower level (typically tcp). It's strictly dependent on the 4th level protocol used. In this version we'll use TCP.

Main status
-----

State diagram on notes 

Messages
-----

| message       | From       | To         | Description                                     | Params                  |
|---------------|------------|------------|-------------------------------------------------|-------------------------|
| lookup        | 1          | Router     | Request to find successor of index passed       | Sender, index           |
| checkAlive    | Checker(s) | Stabilizer | Checks if the predecessor is still alive        | Sender                  |
| imAlive       | Stabilizer | Checker(s) | Notifies checker that the process is alive      | Sender                  |
| updateTable   | Fixer      | Router     | Tells the router to update the finger table     | Sender, index, newValue |
| newSucc       | Router     | Stabilizer | Tells the stab that there is a new succ         | Sender, newSucc         |
| predFind      | Stabilizer | Checker(s) | Asks for the predecessor of the successor       | Sender                  |
| predTell      | Checker(s) | Stabilizer | Answers the above message                       | Sender, predecessor     |
| localAdd      | 2          | B. Manager | Adds the data to the storage                    | Sender, data            |
| localAdded    | B. Manager | many       | Returns the index of the added item             | Sender, index           |
| localGet      | 2          | B. Manager | Asks for an item in the given position          | Sender, index           |
| localObtained | B. Manager | many       | Answers the above message                       | Sender, data            |
| localDelete   | 2          | B. Manager | Ask for the deletion of the item of that index  | Sender, index           |
| localDeleted  | B. Manager | many       | Answers the above message                       | Sender, index           |
| getAll        | Api        | B. Manager | Asks for all the blocks stored                  | Sender                  |
| obtainedAll   | B. Manager | API        | Answers the above message                       | Sender, data            |
| join          | C.M.(any)  | C.M.       | Ask for the successor in order to join          | Sender                  |
| leave         | Api        | C.M.       | Tells the CM to stop all incoming conversations | Sender                  |

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

TO BE REVISITED HEAVILY