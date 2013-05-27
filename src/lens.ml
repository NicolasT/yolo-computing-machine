open Monad
open Monad_reader
open Monad_state

type ('a, 'b) lens = ('a -> 'b) * ('a -> 'b -> 'a)

module type READER_UTILS = sig
    type rur
    type 'a rum
    val view : (rur, 'b) lens -> 'b rum
end

module ReaderUtils = functor(M: sig
    include MONAD
    include MONAD_READER with type 'a rm = 'a m
end) -> (struct
    type rur = M.r
    type 'a rum = 'a M.m

    let view (g, _) = M.bind M.ask (fun c -> M.return (g c))
end : READER_UTILS with type rur = M.r and type 'a rum = 'a M.m)

module type STATE_UTILS = sig
    type sus
    type 'a sum

    val use : (sus, 'b) lens -> 'b sum
    val (@=) : (sus, 'b) lens -> 'b -> unit sum
    val (%=) : (sus, 'b) lens -> ('b -> 'b) -> unit sum
end

module StateUtils = functor(M: sig
    include MONAD
    include MONAD_STATE with type 'a sm = 'a m
end) -> (struct
    type 'a sum = 'a M.m
    type sus = M.s

    let use (g, _) = M.bind M.get (fun s -> M.return (g s))
    let (@=) (_, s) v = M.bind M.get (fun t -> M.put (s t v))
    let (%=) (g, s) f = M.bind M.get (fun t ->
        let v = g t in
        let v' = f v in
        M.put (s t v'))
end : STATE_UTILS with type sus = M.s and type 'a sum = 'a M.m)
