# Chordial
Chordial is our fabulous Erlang project for the Distributed system course !

## Group
* Group number: 3
* Group members:
  * Rebstein Gregoire
  * Geminiani Maxime

## Objective
Chordial is an implementation of the Chord algorithm modeling distribution hash tables.\
\
The project is composed of a javascript front-end that is used to:
* Display the current state of the DHT (as in, the state of each DHT node)
* Offer the usual DHT commands of looking for a value or adding a new value in the hash table

The project is not composed of a single centralized back-end. The back-end is simply the system of all the DHT nodes working together. The first DHT root node, the node n°1, is considered always present on the web page because it is more convenient to have a stable endpoint to send/lookup values.\
The DHT messages (such as joining, asking for an update of the fingers, etc...) are managed by the nodes themselves, making a true decentralized system for our project.\
\
As each node is responsible of connecting to the system, the view has no control over which nodes can join (also, we don't plan on adding security to the join mecanism). A "add node" / "remove node" button does not exist as adding a node will be done via starting the erlang process from another computer, which is also an objective of the project. As such, each node can connect via contacting the root node and will update the table via the chord algorithm of communication with its peers.

## Technology
The front-end is done using VueJS, a simple javascript framework that works out-of-the-box in localhost.\
The back-end is done in Erlang, with two processes per DHT nodes (see mockups).

## Mockups
The mockups try to convey the project structure to a reader and guide the implementaion of the view according to a predetermined design.\
\
![Web view](/img/view.png)\
_Web view_\
The web view is simple:
* The central element is a canvas representing the DHT (as in - its nodes).
* The bottom element is used to add or lookup a node's value.

This view should reflect the DHT state when a node is added, when a node leaves, and should show the path of a message when looking up or adding a value. It should also shows the state of each DHT nodes, as in its fingers and current values.\
\
![Node interaction](/img/node_interactions.png)
_Node interactions_\
The first 'root' node is used as a simplification for the web view - this node is always present and is used as an endpoint to add/lookup tables.\
This first node is also usefull for the connection of new nodes - we can easily think about serving the table's nodes IPs to a node that want to join. That way, the joining node is served the list of nodes and is still responsible of contacting its peers to manage its addition to the system.\
\
![DHT Nodes](/img/dht_nodes.png)\
_DHT Nodes_\
Each DHT node is actually composed of two erlang processes:
* The DHT process, which has the code for indexing, joining or leaving the table
* The simplified storage which is a very simple process with a list of integers

This second process is important to make the dinstinction between the 'transmission' layer of the DHT, which is simply an indexing tool, and the upper application layer which is responsible of actually using the value. This process could be anything built on top of the DHT - database, filesystem, etc. We want to make a point that we understood this fine distinction and want to separate both functions.

## Questions
* We are unsure how the communication View <-> Erlang processes node will take place, we plan on taking the example project as a guide. Our orders are still simple (Add / lookup value, report node's state), but we don't know how to make javascript communicate with an erlang process.
* We are unsure about how to make each node report its state to the view:
  * Message upon change ?
  * Regular scheduled message node -> view ?
  * Regular polling view --request_state--> Node --Respond_state--> View ?
* Are there hash algorithm already present in the erlang packages ? (Probably, have to search that up)