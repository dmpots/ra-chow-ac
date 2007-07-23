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


