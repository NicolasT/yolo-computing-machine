open Monoid
open Monad
open Monad_reader
open Monad_writer
open Monad_state
open Monad_identity
open Monad_trans_rws

module RWS =
    functor(R : sig type t end) ->
    functor(W : MONOID) ->
    functor(S : sig type t end) ->
    (struct

    module T = RWST(R)(W)(S)(Identity)
    include T

    let runRWS m r s = Identity.runIdentity (T.runRWST m r s)
end : sig
    include MONAD
    include MONAD_READER with type r = R.t and type 'a rm = 'a m
    include MONAD_WRITER with type w = W.t and type 'a wm = 'a m
    include MONAD_STATE with type s = S.t and type 'a sm = 'a m

    val runRWS : 'a m -> R.t -> S.t -> ('a * S.t * W.t)
end)
