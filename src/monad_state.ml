module type MONAD_STATE = sig
    type s
    type 'a sm

    val get : s sm
    val put : s -> unit sm
end
