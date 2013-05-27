open Monad

module Identity = (struct
    type 'a m = Identity of 'a

    let return a = Identity a
    let bind a f = match a with
      | Identity v -> f v

    let runIdentity m = match m with
      | Identity v -> v
end : sig
    include MONAD

    val runIdentity : 'a m -> 'a
end)
