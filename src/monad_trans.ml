module type MONAD_TRANS = sig
    type 'a bm
    type 'a tm

    val lift : 'a bm -> 'a tm
end
