yolo-computing-machine
======================

Toying with an FSM implementation in OCaml.

Check out [paxos_candidate.ml](https://github.com/NicolasT/yolo-computing-machine/blob/master/src/paxos_candidate.ml)
to get an idea what state transition implementations could look like, taking the state definition into account:

```ocaml
type candidateState = { _candidateN : n
                      ; _candidateVotes : NodeSet.t
                      }
```
