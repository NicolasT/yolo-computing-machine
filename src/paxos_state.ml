open Monad
open Monad_reader
open Monad_writer
open Monad_state
open Paxos_config
open Paxos_types
open Paxos_config
open Paxos_event
open Paxos_command
open Paxos_message
open Lens

type slaveState = { _slaveN : n
                  ; _slaveI : i
                  }

type candidateState = { _candidateN : n
                      ; _candidateVotes : NodeSet.t
                      }

type masterState = { _masterN : n
                   }

type state = Slave of slaveState
           | Candidate of candidateState
           | Master of masterState

(* TODO Complete *)
let string_of_state = function
  | Slave _ -> "Slave { ... }"
  | Candidate _ -> "Candidate { ... }"
  | Master _ -> "Master { ... }"

module type MONAD_HANDLER = sig
    include MONAD
    include MONAD_READER with type 'a rm = 'a m and type r = config
    include MONAD_WRITER with type 'a wm = 'a m and type w = command list
    include MONAD_STATE with type 'a sm = 'a m
end

module type HANDLER = sig
    type 'a m

    val handle : event -> state m
end

module StateUtils = functor(M : MONAD_HANDLER) -> (struct
    module MU = MonadUtils(M)
    include MU
    module CU = ConfigUtils(M)
    include CU
    module RU = ReaderUtils(M)
    include RU
    module SU = StateUtils(M)
    include SU

    let log s = M.tell [Log s]
    let broadcast m = M.tell [Broadcast m]
    let send n m = M.tell [Send (n, m)]
    let resetElectionTimeout = M.tell [ResetElectionTimeout]
end : sig
    include MONAD_UTILS with type 'a m = 'a M.m

    include CONFIG_UTILS with type 'a cm = 'a M.m
    include READER_UTILS with type rur = M.r and type 'a rum = 'a M.m
    include STATE_UTILS with type sus = M.s and type 'a sum = 'a M.m

    val log : string -> unit M.m
    val broadcast : message -> unit M.m
    val send : nodeId -> message -> unit M.m
    val resetElectionTimeout : unit M.m
end)
