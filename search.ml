
let _ = Random.self_init ()


type 'a fitness_bundle = {
  fitness : 'a -> float;
  best_fitness : 'a list -> 'a * float;
}

(* general neighborhood structure *)
module type NEIGHBORHOOD =
sig
  type t 
  type resident
  type description
  val create : description -> t
  val neighbors : t -> resident -> int -> resident list
  val random_neighbor : t -> resident -> resident
  val random_member : t -> resident
  val fitness : t -> resident fitness_bundle 
end

module PassNeighborHood =
struct 
  type resident = string
  type t = {
    all_passes : resident list;
    fb    : resident fitness_bundle
  }
  type description = resident list * resident fitness_bundle

  (* change one letter in the pass string *)
  let change_one = fun hood pass -> 
    let newpass = String.copy pass in
    let place1 = Random.int (String.length newpass) in
    let place2 = Random.int (List.length hood.all_passes) in
    let _ = 
      String.set newpass place1 
        (String.get (List.nth hood.all_passes place2) 0)
    in
    newpass

  (* create the neighborhood *)
  let create (passes, fitb) = {
    all_passes = passes; 
    fb = fitb;
  }

  (* new neighbors *)
  let neighbors hood pass count = 
    let len = Array.to_list (Array.make count pass) in
    List.map (change_one hood) len

  (* random neighbor *)
  let random_neighbor hood = change_one hood 

  (* fitness definitons *)
  let fitness hood = hood.fb

  (* silly (non)random member *)
  let random_member hood = List.hd hood.all_passes
end

module WeirdFitness =
struct
  let count_chars goal accum ch =
    if ch = goal then incr(accum) 

  let fitness resident = 
    let fit = ref 0 in
    let _ = String.iter (count_chars 'd' fit) resident in
    float_of_int !fit

  let mass_fitness residents =
    let fits = List.map fitness residents in
    List.combine residents fits

  let best_fitness residents =
    let res_fits = mass_fitness residents in
    let sorted = List.sort (fun (_,f1) (_,f2) -> compare f2 f1) res_fits in
    List.hd sorted

  let make () : PassNeighborHood.resident fitness_bundle = {
    fitness=fitness; 
    best_fitness=best_fitness;
  }
end



module FunctionPassHood =
struct 
  type file = string
  type passes = string
  type resident = (file * passes) list
  type t = {
    all_passes : passes list;
    fb    : resident fitness_bundle
  }
  type description = passes list * resident fitness_bundle

  (* change one letter in the pass string *)
  let change_one = fun hood pass -> 
    let newpass = String.copy pass in
    let place1 = Random.int (String.length newpass) in
    let place2 = Random.int (List.length hood.all_passes) in
    let _ = 
      String.set newpass place1 
        (String.get (List.nth hood.all_passes place2) 0)
    in
    newpass

  (* change one in each of the files *)
  let change_one_each_file = fun hood file_passes ->
    List.map (fun (file, pass) -> (file, change_one hood pass)) file_passes

  (* create the neighborhood *)
  let create (passes, fitb) = {
    all_passes = passes; 
    fb = fitb;
  }

  (* new neighbors *)
  (* find some new neighbors of the current state (file_passes) by
     taking each file and transforming the pass associated with it.
     This is done count number of times to make count neighbors.

     example:
      [("seval.i", "abc"); ("spline.i", "bdd")]
      --> becomes -->
      [[("seavl.i", "adc"); ("spline.i", "bcd")]; [("seval.i", ...]]
  *)
  let neighbors hood file_passes count = 
    let len = Array.to_list (Array.make count file_passes) in
    List.map (change_one_each_file hood) len

  (* random neighbor *)
  let random_neighbor = fun hood res -> 
    List.map (fun (file,pass) -> (file, change_one hood pass)) res

  let random_member = fun hood -> [("noway", "hose")]
    
  (* fitness definitons *)
  let fitness hood = hood.fb
end

(*
module ChowHood =
struct 
  type file = string
  type args = string
  type resident = (file * args)
  type description =
end
*)

(* Access an external program for computing fitness. This is used to
 * run the iloc compiler and then use the operation count for the
 * fitness function.
 *
 * The i/o contract with external programs is as follows:
 * 1) INPUT consists of pairs (file: args) that are used as specific
 * input for that file. Once all files have been listed for a given
 * benchmark a line starting with % and containing nothing else is
 * written
 * 2) OUTPUT consists of triples containing file|opcount|args where
 * args are the args that were used for the file and opcount is the
 * operation count for those files and args. Once all output for a
 * given benchmark has been written, a line with a single % is
 * written. This will match up with the input given to the program.
 * 
 * If this contract is followed then the code below should work
 * correctly.
 ***)
module ExternalFitness =
struct
  let extrn_prog = "tw.rb"
  let sec_sep = "%"
  let line_sep = "|"

  (* send each residnet to be scored. each group is scored by
   *  writing pairs of files and their args to the process
   ***)
  let send_fitness_request extrn_fit resident =
    List.iter (fun (file, args) ->
      output_string extrn_fit (file^": "^args^"\n")
    ) resident;
    output_string extrn_fit "%\n"

  (* split line into three parts separated by pipes("|")
   ***)
  let sp_regex = Str.regexp line_sep
  let split3 line = 
    let ht = Str.bounded_split sp_regex line 3  in
      if List.length ht < 3 then failwith "input split failed";
      (List.nth ht 0, List.nth ht 1, List.nth ht 2)

  (* read result from process. results are formated as lines of
   *  file|opcount|args
   ***)
  let read_fitness_results extrn_res = 
    let fits = ref [] in
    begin
      try
        let rec loop accum =
          let line = input_line extrn_res in
          if line = sec_sep then (
            fits := accum :: !fits; 
            loop [] 
          )
          else
            let file,count,pass = split3 line in
            loop ((file,pass,float_of_string count) :: accum)
        in
        loop []
      with End_of_file -> ()
    end;
    !fits

  (* compute the fitness of each resident in one swoop
   * (file * args) list list -> (file * args * float) list
   ***)
  let bulk_fitness residents  =
    let extrn_res,extrn_fit = Unix.open_process extrn_prog in
    List.iter (send_fitness_request extrn_fit) residents;
    close_out extrn_fit;
    let results = read_fitness_results extrn_res in
    let _ = Unix.close_process (extrn_res,extrn_fit) in
    results

  (* take the single (file * args * opcount) from the results and
   * return it *)
  let single_fitness resident = 
    List.hd (bulk_fitness [resident])
end

module FunctionSpecificFitness = 
struct
  type resident = FunctionPassHood.resident 

  (*---------------------------fun---------------------------------*) 
 let fitness resident = 
    let each_fun_marked = ExternalFitness.single_fitness resident in 
    List.fold_left (fun sum (file,pass,fit) -> sum +. fit) 0.0 each_fun_marked

  (*---------------------------fun---------------------------------*) 
  (* find the fitness of each (function,pass) pair and choose the result
     to be a combination of the highest individual function pairs *)
  let best_fitness residents =
    (* get a long list containing (function,args,fitness) for every
       function and args that exists in the original residents entries *)
    let each_fun_marked = 
      List.flatten (ExternalFitness.bulk_fitness residents) in
    (* collect results into map of function --> (pass,bestfitness) *)
    let module M = Map.Make(String) in
    let best_map = 
    List.fold_left (fun map (file, pass, fit) -> 
      try
        let (_,bestfit) = M.find file map in
        if fit > bestfit then 
          M.add file (pass,fit) (M.remove file map) 
        else map
      with Not_found -> 
        M.add file (pass, fit) map
    ) (M.empty) each_fun_marked
    in
    (* convert the map into resident * fitness pair *)
    M.fold (fun file (pass, fit) (res, sum) ->
      ((file, pass)::res, fit +. sum)
    ) best_map ([], 0.0)

  (*---------------------------fun---------------------------------*) 
  (* probably change this to pass in the external fitness function to
     use and curry the fitness functions to take that as the first
     param *)
  let make () : FunctionPassHood.resident fitness_bundle = {
    fitness=fitness; 
    best_fitness=best_fitness;
  }
end

module HillClimber (N : NEIGHBORHOOD) =
struct
  let better_than challenger champ = challenger < champ
  (* right now low fitness is good so that opcount translates directly
     to fitness, but we could pass a function to compare fitness
     levels if we need high fitness to be good *)
  let search (hood : N.t) (seed : N.resident) =
    let patience = 10 in 
    let num_nebs = 100 in 
    let best_fit = (N.fitness hood).best_fitness in
    let rec do_search state curfit laterals =
      let nebs = N.neighbors hood state num_nebs in
      let newstate,bestfit = best_fit (state::nebs) in
      if better_than bestfit curfit then (* low fitness is good *)
        do_search newstate bestfit laterals 
      else
        if curfit = bestfit then
          if laterals > 0 then
            (*let _ = print_string ">> LATERAL\n" in*)
            do_search newstate bestfit (laterals - 1) 
          else
            (*let _ = print_string "SAME BEST - NO MORE LATERALS\n" in*)
            state
        else (* only found worse neighbors *)
            (*let _ = print_string "!SAME BEST\n" in*)
          state
    in
    let startfit = ((N.fitness hood).fitness) seed  in
    do_search seed startfit patience 
end

(* benchmark specific neighbordhoods *)
let wfb = WeirdFitness.make () 
let passes = ["a"; "b"; "c"; "d"; "g"; 
              "l"; "m"; "n"; "o"; "p"; 
              "q"; "r"; "s"; "t"; "v"; "z"]
let ph = PassNeighborHood.create (passes, wfb);;
module HC = HillClimber(PassNeighborHood)

(* function specific neighbordhoods *)
let ffb = FunctionSpecificFitness.make () 
let fh = FunctionPassHood.create (passes, ffb);;
let tests = [
  [("seval.i", "abcdefg"); ("spline.i", "abdeftg")];
  [("seval.i", "abddefg"); ("spline.i", "acdeftg")];
  [("seval.i", "abddefs"); ("spline.i", "acdmftg")];
  [("seval.i", "abedefs"); ("spline.i", "acdiofg")];
]
module HCF = HillClimber(FunctionPassHood)

module Benchmarks = 
struct
  let fmin = ["fmin.i"]
  let seval = ["spline.i"; "seval.i"]
end


