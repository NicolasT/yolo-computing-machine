open Paxos_types
open Paxos_message

type command = Broadcast of message
             | Send of (nodeId * message)
             | ResetElectionTimeout
             | Log of string

(* TODO Complete *)
let string_of_command = function
  | Broadcast m -> Printf.sprintf "Broadcast (...)"
  | Send (s, m) -> Printf.sprintf "Send (%S, ...)" s
  | ResetElectionTimeout -> "ResetElectionTimeout"
  | Log s -> Printf.sprintf "Log %S" s
