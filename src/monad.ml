module type MONAD = sig
    type 'a m
    val return : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m
end

module type MONAD_UTILS = sig
    include MONAD
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
end

module MonadUtils = functor(M: MONAD) -> (struct
    include M
    let (>>=) m k = M.bind m k
end : MONAD_UTILS with type 'a m = 'a M.m)
