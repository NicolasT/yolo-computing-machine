open Paxos_types

type prepare = { prepareN : n
               }
type promise = { promiseN : n
               ; promiseI : i
               }

(* Note: this is a bogus implementation! *)
type message = Prepare of prepare
             | Promise of promise
             | Accept
