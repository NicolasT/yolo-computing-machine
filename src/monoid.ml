module type MONOID = sig
    type t
    val empty : t
    val append : t -> t -> t
end

module List = functor(T : sig type t end) -> (struct
    type t = T.t list

    let empty = []
    let append a b = a @ b
end : MONOID with type t = T.t list)
