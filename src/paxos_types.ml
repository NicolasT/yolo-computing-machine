type nodeId = string

module NodeSet = Set.Make(struct
    type t = nodeId
    let compare = compare
end)

type n = N of int
let n0 = N 0
let succN (N n) = N (n + 1)

type i = I of int
let i0 = I 0
let succI (I i) = I (i + 1)
