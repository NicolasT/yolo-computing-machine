module type MONAD = sig
    type 'a m
    val return : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m
end

module MonadUtils = functor(M: MONAD) -> (struct
    include M

    let (>>=) m k = bind m k
end : sig
    include MONAD with type 'a m = 'a M.m
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
end)
