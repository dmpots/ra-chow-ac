(* could use buf for more efficency here *)
let join lst sep to_s = 
    List.fold_left ( fun acc mem -> 
      acc ^ sep ^ (to_s mem)
    ) (to_s (List.hd lst)) (List.tl lst)

