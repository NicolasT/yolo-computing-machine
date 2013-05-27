open Monad
open Monad_reader
open Monad_state

type ('a, 'b) lens = ('a -> 'b) * ('a -> 'b -> 'a)

module ReaderUtils = functor(M: sig
    include MONAD
    include MONAD_READER with type 'a rm = 'a m
end) -> (struct
    let view (g, _) = M.bind M.ask (fun c -> M.return (g c))
end : sig
    val view : (M.r, 'b) lens -> 'b M.m
end)

module StateUtils = functor(M: sig
    include MONAD
    include MONAD_STATE with type 'a sm = 'a m
end) -> (struct
    let use (g, _) = M.bind M.get (fun s -> M.return (g s))
    let (@=) (_, s) v = M.bind M.get (fun t -> M.put (s t v))
    let (%=) (g, s) f = M.bind M.get (fun t ->
        let v = g t in
        let v' = f v in
        M.put (s t v'))
end : sig
    val use : (M.s, 'b) lens -> 'b M.m
    val (@=) : (M.s, 'b) lens -> 'b -> unit M.m
    val (%=) : (M.s, 'b) lens -> ('b -> 'b) -> unit M.m
end)
