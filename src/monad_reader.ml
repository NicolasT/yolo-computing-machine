module type MONAD_READER = sig
    type r
    type 'a rm

    val ask : r rm
end
