open Lens

open Paxos_event
open Paxos_state
open Paxos_types
open Paxos_command
open Paxos_message

let n : (candidateState, n) lens =
    let get s = s._candidateN
    and set s v = { s with _candidateN = v } in
    (get, set)

let votes : (candidateState, NodeSet.t) lens =
    let get s = s._candidateVotes
    and set s v = { s with _candidateVotes = v } in
    (get, set)

module Handler = functor(M : MONAD_HANDLER with type s = candidateState) -> (struct
    module SU = StateUtils(M)
    include SU

    let currentState = M.get >>= fun s -> return (Candidate s)
    let hasMajority = use votes >>= isMajority

    let becomeMaster =
        resetElectionTimeout >>= fun () ->
        broadcast Accept >>= fun () ->
        use n >>= fun n' ->
        return (Master { _masterN = n' })

    let handleMessage s = function
      | Prepare p ->
          begin
              log "Ignoring Prepare message in Candidate state" >>= fun () ->
              currentState
          end
      | Promise p ->
          begin
              log "Received Promise" >>= fun () ->
              votes %= NodeSet.add s >>= fun () ->
              hasMajority >>= function
                | true ->
                    log "Reached majority" >>= fun () ->
                    becomeMaster
                | false ->
                    log "No majority yet" >>= fun () ->
                    currentState
          end
      | Accept ->
          begin
              log "Received Accept, dropping (yup, bogus implementation!)" >>= fun () ->
              currentState
          end

    let handle = function
      | Message (s, m) -> handleMessage s m

end : HANDLER with type 'a m = 'a M.m)
