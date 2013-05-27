open Paxos_types
open Paxos_message

type event = Message of (nodeId * message)
