open Monad
open Monad_reader
open Paxos_types
open Lens

type config = { _configNodeId : nodeId
              ; _configNodes : NodeSet.t
              }

let configNodeId : (config, nodeId) lens =
    let get s = s._configNodeId
    and set s v = { s with _configNodeId = v } in
    (get, set)

let configNodes : (config, NodeSet.t) lens =
    let get s = s._configNodes
    and set s v = { s with _configNodes = v } in
    (get, set)

module ConfigUtils = functor(M : sig
    include MONAD
    include MONAD_READER with type r = config and type 'a rm = 'a m
end) -> (struct
    module MU = MonadUtils(M)
    open MU

    let quorumSize =
        M.ask >>= fun c ->
        let s = c._configNodes in
        let q = (NodeSet.cardinal s) / 2 + 1 in
        M.return q

    let isMajority s =
        quorumSize >>= fun q ->
        return (NodeSet.cardinal s >= q)

end : sig
    val quorumSize : int M.m
    val isMajority : NodeSet.t -> bool M.m
end)
