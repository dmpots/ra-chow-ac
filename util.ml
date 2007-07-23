(* could use buf for more efficency here *)
let join lst sep to_s = 
    List.fold_left ( fun acc mem -> 
      acc ^ sep ^ (to_s mem)
    ) (to_s (List.hd lst)) (List.tl lst)

(* timestamp as a string *)
let timestamp () = 
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d:%02d:%02d  %02d/%02d/%d" 
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (tm.Unix.tm_mon+1) tm.Unix.tm_mday (tm.Unix.tm_year+1900)

(*================================================================
 * RANDOM LIST
 *===============================================================
 * Module for functions that act randomly in some way on lists
 *---------------------------------------------------------------*)
module RandomList = 
struct
  let swap a i j =
    let t = a.(i) in 
      a.(i) <- a.(j); a.(j) <- t

  (* chooses n values randomly from the passed list *)
  let pick n from_list =
    let a = Array.of_list from_list in
    let size = Array.length a in
    if n > size then raise (Invalid_argument "pick: n > size") else
    let rec loop i ub picks =
      if i = n then picks else
      let _ = swap a (if ub = 0 then 0 else Random.int ub) ub in
      loop (i+1) (ub-1) (a.(ub)::picks)
    in
    loop 0 (size-1) []

  let random_choice lst = 
    List.nth lst (Random.int (List.length lst)) 
end


