Echo_rd
=====

An Erlang library that implements the chord P2P system.
For more details see there https://pdos.csail.mit.edu/papers/ton:chord/paper-ton.pdf.

Build
-----

    $ rebar3 compile
    
Interface
-----

- add(data), will include the data provided into the network. It will return the index of the added item
- get(n), will get the data at index n
- delete(n), will remove the data at index n from the network.
- create(), will create a new Chord network
- join(node), will enter the network whose node is part
- leave(), will leave (softly) the network


Actors
-----

- API: it will handle the communication with the application.
- Block Manager: it will handle the data stored within the node.
- Router: it will handle the lookups operations and the finger table management.
- Communication Manager: it will handle all the message received from the network.
- Stabilizer: verifies immediate successor, and tells the successor about itself. Periodically.
- Fixer: refreshes finger table entries. Periodically.
- Checker: checks if the predecessor failed. Periodically.

Initialize procedure
-----
chord:init() will create the Api and the Communication Manager will be created from it. After a join/create, Communication Manager will create Router and block manager, notifying API about it. Router will then proceed creating the other 3 services.

Closure procedure
-----

Leave command will proceed to pass all the blocks stored to the successor. Then will kill all the services except Api and Communication Manager, returning in a post chordInit state. Exit will then close the package completely.

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
| localAdded    | B. Manager | many       | Returns the index of the added item             | Sender, index     |
| localGet      | 2          | B. Manager | Asks for an item in the given position          | Sender, index           |
| localObtained | B. Manager | many       | Answers the above message                       | Sender, data     |
| localDelete   | 2          | B. Manager | Ask for the deletion of the item of that index  | Sender, index           |
| localDeleted  | B. Manager | many       | Answers the above message                       | Sender, index         |
| getAll        | Api        | B. Manager | Asks for all the blocks stored                  | Sender                  |
| obtainedAll   | B. Manager | API        | Answers the above message                       | Sender, data            |
| join          | C.M.(any)  | C.M.       | Ask for the successor in order to join          | Sender                  |
| leave         | Api        | C.M.       | Tells the CM to stop all incoming conversations | Sender                  |

- 1 is Api (l), Router, Fixer(l)
- 2 is Api (any) 