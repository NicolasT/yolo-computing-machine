open Monad_identity
open Monad_trans_rws

open Paxos_event
open Paxos_state
open Paxos_types
open Paxos_config
open Paxos_command
open Paxos_message

(* Our state machine is very pure and runs in the Identity monad *)
module CM = RWST
    (struct type t = config end)
    (Monoid.List(struct type t = command end))
    (struct type t = candidateState end)
    (Identity)

module CH = Paxos_candidate.Handler(CM)

let handle config state event = match state with
  | Candidate s -> Identity.runIdentity (CM.runRWST (CH.handle event) config s)
  | _ -> failwith "Not implemented"

;;

let config =
    let nodeId = "node0" in
    let nodes = NodeSet.add "node1" (NodeSet.singleton nodeId) in
    { _configNodeId = nodeId
    ; _configNodes = nodes
    }
in

let state0 = Candidate { _candidateN = n0
                       ; _candidateVotes = NodeSet.singleton config._configNodeId
                       } in
let promise = { promiseN = n0
              ; promiseI = i0
              } in
let event = Message ("node1", Promise promise) in

Printf.printf "Original state: %s\n" (string_of_state state0);
print_newline ();

print_endline "Handling event...";
print_newline();

let (state, _, commands) = handle config state0 event in

Printf.printf "New state: %s\n" (string_of_state state);
print_newline ();
print_endline "Commands:";
List.iter (fun c -> Printf.printf "    %s\n" (string_of_command c)) commands
