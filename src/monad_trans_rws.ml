open Monoid
open Monad
open Monad_reader
open Monad_writer
open Monad_state
open Monad_trans

module RWST =
    functor(R : sig type t end) ->
    functor(W : MONOID) ->
    functor(S : sig type t end) ->
    functor(M : MONAD) ->
    (struct

    type 'a m = RWST of (R.t -> S.t -> ('a * S.t * W.t) M.m)
    let unRWST (RWST f) = f

    let return a = RWST (fun _ s -> M.return (a, s, W.empty))
    let bind m k = RWST (fun r s->
        let f = unRWST m in
        M.bind (f r s) (fun (a, s', w) ->
        let f' = unRWST (k a) in
        M.bind (f' r s') (fun (b, s'', w') ->
        M.return (b, s'', W.append w w'))))

    type r = R.t
    type 'a rm = 'a m
    let ask = RWST (fun r s -> M.return (r, s, W.empty))

    type w = W.t
    type 'a wm = 'a m
    let tell w = RWST (fun _ s -> M.return ((), s, w))

    type s = S.t
    type 'a sm = 'a m
    let get = RWST (fun _ s -> M.return (s, s, W.empty))
    let put s = RWST (fun _ _ -> M.return ((), s, W.empty))

    type 'a tm = 'a m
    type 'a bm = 'a M.m
    let lift m = RWST (fun _ s ->
        M.bind m (fun a ->
        M.return (a, s, W.empty)))

    let runRWST a r s =
        let f = unRWST a in
        f r s
end : sig
    include MONAD
    include MONAD_READER with type r = R.t and type 'a rm = 'a m
    include MONAD_WRITER with type w = W.t and type 'a wm = 'a m
    include MONAD_STATE with type s = S.t and type 'a sm = 'a m
    include MONAD_TRANS with type 'a bm = 'a M.m and type 'a tm = 'a m

    val runRWST : 'a m -> R.t -> S.t -> ('a * S.t * W.t) M.m
end)

