module type S = sig
  type ele
  type t

  val create : int -> t
  val find : t -> ele -> ele
  val union : t -> ele -> ele -> unit
  val iter : t -> (ele -> unit) -> unit
  val sets : t -> ele list list
end

module type HashedTypeWithPriority = sig
  include Hashtbl.HashedType

  (* should the first argument be the representative of the second *)
  val should_be_rep : t -> t -> bool
end

module Make (H : HashedTypeWithPriority) : S with type ele = H.t = struct
  module Tbl = Hashtbl.Make (H)

  type ele = H.t
  type t = ele Tbl.t

  let create n = Tbl.create n

  let rec find tbl x =
    match Tbl.find_opt tbl x with
    | None -> x
    | Some parent ->
        let new_parent = find tbl parent in
        Tbl.replace tbl x new_parent;
        new_parent

  let union tbl x y =
    let x = find tbl x in
    let y = find tbl y in
    if x = y then ()
    else if H.should_be_rep x y then Tbl.replace tbl y x
    else Tbl.replace tbl x y

  let iter tbl f = Tbl.iter (fun k _ -> f k) tbl

  (*impure version*)

  let sets tbl =
    let module Eletbl = Hashtbl.Make (H) in
    let tys_by_reps = Eletbl.create 10 in
    Tbl.iter
      (fun x _ ->
        let rep = find tbl x in
        if not @@ Eletbl.mem tys_by_reps rep then Eletbl.add tys_by_reps rep [];
        Eletbl.replace tys_by_reps rep (x :: Eletbl.find tys_by_reps rep))
      tbl;
    Eletbl.fold (fun _ v acc -> v :: acc) tys_by_reps []
end
