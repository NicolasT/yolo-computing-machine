module type MONAD_WRITER = sig
    type w
    type 'a wm

    val tell : w -> unit wm
end
