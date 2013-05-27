open Monad
open Monad_identity
open Monad_trans_rws
open Lens

type r = { _rA : int
         ; _rB : string
         }
let string_of_r r = Printf.sprintf "{ _rA = %d; _rB = %S }" r._rA r._rB

let rA : (r, int) lens =
    let get s = s._rA
    and set s v = { s with _rA = v } in
    (get, set)

let rB : (r, string) lens =
    let get s = s._rB
    and set s v = { s with _rB = v } in
    (get, set)


type s = { _sA : float
         ; _sB : bool
         }
let string_of_s s = Printf.sprintf "{ _sA = %f; _sB = %b }" s._sA s._sB

let sA : (s, float) lens =
    let get s = s._sA
    and set s v = { s with _sA = v } in
    (get, set)

let sB : (s, bool) lens =
    let get s = s._sB
    and set s v = { s with _sB = v } in
    (get, set)


let demo r s =
    let module M = RWST
        (struct type t = r end)
        (Monoid.List(struct type t = int end))
        (struct type t = s end)
        (Identity) in
    let open M in
    let module MU = MonadUtils(M) in
    let open MU in
    let module RU = ReaderUtils(M) in
    let open RU in
    let module SU = StateUtils(M) in
    let open SU in

    let r = { _rA = 1
            ; _rB = "foo"
            }
    and s = { _sA = 3.14
            ; _sB = false
            }
    in

    let act =
        tell [1] >>= fun () ->
        use sA >>= fun s ->
        sA @= 2.1 >>= fun () ->
        view rA >>= fun r ->
        tell [2] >>= fun () ->
        sA %= (fun f -> f +. float r) >>= fun () ->
        tell [3] >>= fun () ->
        sB @= true >>= fun () ->
        sA %= tan >>= fun () ->
        return (floor s)
    in

    Identity.runIdentity (M.runRWST act r s)

let main () =
    let r = { _rA = 1
            ; _rB = "foo"
            }
    and s = { _sA = 3.14
            ; _sB = false
            } in

    Printf.printf "Initial state: %s\n" (string_of_s s);

    let (a, s', w) = demo r s in

    Printf.printf "Result: %f\n" a;
    Printf.printf "State: %s\n" (string_of_s s');
    Printf.printf "Output: [%s]\n" (String.concat ", " (List.map string_of_int w))

;;

main ()
