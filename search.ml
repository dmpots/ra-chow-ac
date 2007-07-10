
(*let _ = Random.self_init ()*)


type ('a, 'b) fitness_bundle = {
  fitness : 'a -> 'b;
  best_fitness : 'a list -> 'a * 'b;
  summarize : 'b -> float;
}

(* general neighborhood structure *)
module type NEIGHBORHOOD =
sig
  type t 
  type resident
  type description
  type fit_result
  val create : description -> t
  val neighbors : t -> resident -> int -> resident list
  val random_neighbor : t -> resident -> resident
  val fitness : t -> (resident,fit_result) fitness_bundle 
end

module PassNeighborHood =
struct 
  type resident = string
  type fit_result = float
  type t = {
    all_passes : resident list;
    fb    : (resident,fit_result) fitness_bundle
  }
  type description = resident list * (resident,float) fitness_bundle

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

  let make () : (PassNeighborHood.resident, float) fitness_bundle = {
    fitness=fitness; 
    best_fitness=best_fitness;
    summarize = (fun x -> x);
  }
end



module FunctionPassHood =
struct 
  type file = string
  type passes = string
  type resident = (file * passes) list
  type fit_result = ((file * passes * float) list * float)
  type t = {
    all_passes : passes list;
    fb    : (resident, fit_result) fitness_bundle
  }
  type description = passes list * (resident,fit_result) fitness_bundle

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

    
  (* fitness definitons *)
  let fitness hood = hood.fb
end

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
  type rep = (string * string) list
  type output = string * string * float list
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
   * (file * args) list list -> (file * args * float) list list
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
 (*---------------------------fun---------------------------------*) 
  let fitness massage resident = 
    let massaged = massage resident in
    let each_fun_marked = ExternalFitness.single_fitness massaged in 
    List.fold_left (fun (fits, sum) (file,pass,fit) -> 
      (file,pass,fit)::fits, sum +. fit
    ) ([], 0.0) each_fun_marked

  (*---------------------------fun---------------------------------*) 
  (* find the fitness of each (function,pass) pair and choose the result
     to be a combination of the highest individual function pairs *)
  let best_fitness massage unmassage residents =
    let better_than challenger champ = challenger < champ in
    (* make sure the residents are in a form suitable for use with the
       External fitness module *)
    let massaged = List.map massage residents in
    (* get a long list containing (function,args,fitness) for every
       function and args that exists in the original residents entries *)
    let each_fun_marked = 
      List.flatten (ExternalFitness.bulk_fitness massaged) in
    (* collect results into map of function --> (pass,bestfitness) *)
    let module M = Map.Make(String) in
    let best_map = 
    List.fold_left (fun map (file, pass, fit) -> 
      try
        let (_,bestfit) = M.find file map in
        if better_than fit bestfit then 
          M.add file (pass,fit) (M.remove file map) 
        else map
      with Not_found -> 
        M.add file (pass, fit) map
    ) (M.empty) each_fun_marked
    in
    (* convert the map into resident * fitness pair *)
    let best_res,fits,fit = 
      M.fold (fun file (pass, fit) (res, fits, sum) ->
        ((file, pass)::res, (file,pass,fit)::fits, fit +. sum)
      ) best_map ([], [], 0.0)
    in
    (unmassage best_res, (fits,fit))

  (*---------------------------fun---------------------------------*) 
  (* probably change this to pass in the external fitness function to
     use and curry the fitness functions to take that as the first
     param *)
  let make massage unmassage = {
    fitness=fitness massage; 
    best_fitness=best_fitness massage unmassage;
    summarize = (fun (_, sum) -> sum)
  }
end

module HillClimber (N : NEIGHBORHOOD) =
struct
  let better_than_in_hood hood challenger champ = 
    let summ = (N.fitness hood).summarize in
    (summ challenger ) < (summ champ)
(*  print_string "better_than? :";
    print_float challenger;
    print_string " ";
    print_float champ;
    print_string "\n";
*)
  (* right now low fitness is good so that opcount translates directly
     to fitness, but we could pass a function to compare fitness
     levels if we need high fitness to be good *)
  let search (hood : N.t) (seed : N.resident) =
    let patience = 10 in 
    let num_nebs = 100 in 
    let better_than = better_than_in_hood hood in
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
            state,bestfit
        else (* only found worse neighbors *)
            (*let _ = print_string "!SAME BEST\n" in*)
          state,bestfit
    in
    let startfit = ((N.fitness hood).fitness) seed  in
    do_search seed startfit patience 
end

module ChowArgs = 
struct
  type arg_choices = 
  | BoolC   of bool list
  | FloatC  of float list
  | IntC    of int list
  | TupleC  of (int * int) list

  type arg_val = 
  | Bool   of bool 
  | Float  of float 
  | Int    of int 
  | Tuple  of (int * int) 

  type arg_name = string
  type spec = arg_name *  arg_choices
  type chow_arg = arg_name * arg_val 

  let to_string (name, value) =
    let name_s = ("-"^name^" ") in
    match value with
      | Bool true -> name_s 
      | Bool false -> ""
      | Float f -> name_s^(string_of_float f)
      | Int i   -> name_s^(string_of_int i)
      | Tuple (f,s) -> name_s^(string_of_int f)^","^(string_of_int s)



  (* for parsing arg values from a string. note that the parser here
     will be kinda picky in that each arg must have a space following
     the arg, i.e  -b 5 works but -b5 does not *)
  let spaces = Str.regexp "[ ]+" 
  let comma  = Str.regexp "," 
  let arg_accum = ref []
  let add_arg arg = arg_accum := arg :: !arg_accum
  let make_bool_arg name ()  = add_arg (name, Bool true)
  let make_int_arg name ival = add_arg (name, Int ival)
  let make_tup_arg name sval = 
    match (Str.bounded_split comma sval 2) with
      | [h;t] -> add_arg (name, Tuple (int_of_string h, int_of_string t))
      | _ -> failwith "tuple arg did not have two parts"

  let all_args = [
    ("-b", Arg.Int (make_int_arg  "b"), "");
    ("-r", Arg.Int (make_int_arg  "r"), "");
    ("-p", Arg.Unit(make_bool_arg "p"), "");
    ("-m", Arg.Unit(make_bool_arg "m"), "");
    ("-e", Arg.Unit(make_bool_arg "e"), "");
    ("-f", Arg.Unit(make_bool_arg "f"), "");
    ("-y", Arg.Unit(make_bool_arg "y"), "");
    ("-z", Arg.Unit(make_bool_arg "z"), "");
    ("-t", Arg.Unit(make_bool_arg "t"), "");
    ("-c", Arg.Int (make_int_arg  "c"), "");
    ("-i", Arg.Int (make_int_arg  "i"), "");
    ("-w", Arg.Int (make_int_arg  "w"), "");
    ("-s", Arg.Int (make_int_arg  "s"), "");
    ("-g", Arg.Unit (make_bool_arg "g"), "");
    ("-o", Arg.Unit (make_bool_arg "o"), "");
    ("-a", Arg.Unit (make_bool_arg "a"), "");
    ("-l", Arg.String (make_tup_arg  "l"), "");
  ]

  let from_string str  : chow_arg list =
    let split = Array.of_list ("dummy"::(Str.split spaces str)) in
    arg_accum := [];
    Arg.parse_argv ~current:(ref 0) split all_args (fun _ -> ()) "";
    !arg_accum
   
(*  let change_random_arg (arg : chow_arg) =*)
    
end


module ChowHood =
struct
  open ChowArgs
  type file = string
  type resident = (file * (chow_arg list)) list
  type fit_result = ((file * string * float) list * float)
  type t = {
    fixed_args : chow_arg list;
    adaptable_args : ChowArgs.spec list;
  }
  type description = (chow_arg list * ChowArgs.spec list)

  let create (fixed, adaptable) = {
    fixed_args = fixed;
    adaptable_args  = adaptable;
  }
  
  (* could use buf for more efficency here *)
  let join lst sep to_s = 
    List.fold_left (fun acc mem -> acc ^ sep ^ (to_s mem)) "" lst
    
  (* convet the resident to a format usable with external fitness *)
  let to_extern (resident : resident) : ExternalFitness.rep =
    List.map (fun (file, args) -> 
      (file, join args " " ChowArgs.to_string)
    ) resident

  (* convet the resident from format used by external fitness *)
  let from_extern (extrn_rep :ExternalFitness.rep) : resident =
    List.map (fun (file, args) ->
      (file, ChowArgs.from_string args)
    ) extrn_rep


  (* some utility functions *)
  let same_name name ((n,_) : chow_arg) = name = n
  let diff_name name ((n,_) : chow_arg) = name <> n
  let random_choice lst = List.nth lst (Random.int (List.length lst)) 

  (* choose randomly among the valid choices for an arg *)
  let take_random_choice (choices : arg_choices) : arg_val =
    match choices with
    | BoolC  lst -> Bool  (random_choice lst)
    | IntC   lst -> Int   (random_choice lst)
    | FloatC lst -> Float (random_choice lst)
    | TupleC lst -> Tuple (random_choice lst)

  (* change one arg in the arglist *)
  let change_random_arg hood (arglist : chow_arg list) = 
    (* some utility funs *)
    let filter_int_list n = function
      | IntC cs -> List.filter (fun n' -> n <> n') cs
      | _ -> failwith "oops should be int"
    in
    let filter_tuple_list tup = function
      | TupleC cs -> List.filter (fun tup' -> tup <> tup') cs
      | _ -> failwith "oops should be tuple"
    in
    let filter_float_list f = function
      | FloatC cs -> List.filter (fun f' -> f <> f') cs
      | _ -> failwith "oops should be float"
    in
    (* choose an arg to change *)
    let (name,choices) = random_choice hood.adaptable_args in
    let chosen_arg = same_name name in
    (* see if there is a previous value for this arg *)
    let old_choice = 
      if List.exists chosen_arg arglist then 
        Some (snd (List.find chosen_arg arglist))
      else
        None
    in
    let new_choice = 
    match old_choice with
      | None -> take_random_choice choices
      | Some(arg) ->(
        match arg with
          | Bool b -> Bool (not b)
          | Int  n -> Int (random_choice (filter_int_list n choices))
          | Float f -> Float (random_choice (filter_float_list f choices))
          | Tuple t -> Tuple (random_choice (filter_tuple_list t choices))
        )
    in (name, new_choice)::(List.filter (diff_name name) arglist)

  (* a group of random neighbors (possibly with repeats) *)
   let neighbors hood (resident : resident) count = 
    let len = Array.to_list (Array.make count resident) in
    List.map (fun file_args ->
      List.map (fun (file, arglist) ->
        file, (change_random_arg hood arglist)
      ) file_args
    ) len

  (* one random neighbor *)
  let random_neighbor hood (resident: resident) : resident = 
    List.map (fun (file, arglist) ->
        file, (change_random_arg hood arglist)
    ) resident
    
  (* chow neighborhoods can all use the same fitness *)
  let fitbun = 
    FunctionSpecificFitness.make to_extern from_extern
  let fitness (hood : t) = fitbun

  let random_resident hood files : resident = 
    List.map (fun file ->
      let adaptables = 
        List.map (fun (name, choices) -> 
          (name, take_random_choice choices)
        ) hood.adaptable_args
      in
      (file, hood.fixed_args @ adaptables)
    ) files
    
end

(* benchmark specific neighbordhoods *)
let wfb = WeirdFitness.make () 
let passes = ["a"; "b"; "c"; "d"; "g"; 
              "l"; "m"; "n"; "o"; "p"; 
              "q"; "r"; "s"; "t"; "v"; "z"]
let ph = PassNeighborHood.create (passes, wfb);;
module HC = HillClimber(PassNeighborHood)

(* function specific neighbordhoods *)
let id = (fun x -> x)
let ffb = FunctionSpecificFitness.make id id
let fh = FunctionPassHood.create (passes, ffb);;
let tests2 = [
  [("seval.i", "abcdefg"); ("spline.i", "abdeftg")];
  [("seval.i", "abddefg"); ("spline.i", "acdeftg")];
  [("seval.i", "abddefs"); ("spline.i", "acdmftg")];
  [("seval.i", "abedefs"); ("spline.i", "acdiofg")];
]
module HCF = HillClimber(FunctionPassHood)


(* chow specific neighbordhoods *)
let fixed = [
  ("r", ChowArgs.Int 32)
]
let adaptable = [
  ("b", ChowArgs.IntC [2;3;4;5]);
  ("l", ChowArgs.TupleC [(0,0); (0,2); (0,4)]);
  ("m", ChowArgs.BoolC  [true; false]);
  ("e", ChowArgs.BoolC  [true; false]);
]
let chood = ChowHood.create (fixed, adaptable)
module HCC = HillClimber(ChowHood)


let ars = ChowArgs.from_string "-r 32 -b 5 -l 2,2"
let rezs : ChowHood.resident list = 
List.map (fun config ->
  List.map (fun (file, args) -> (file, ChowArgs.from_string args))config
) [
  [("seval.i", "-r 32 -b 5 -e"); ("spline.i", "-r 32 -b 6 -m")];
]

module Benchmarks = 
struct
  type benchmark = 
    | Fmin
    | Seval

  let valid_names =
    ["fmin"; "seval"]
  let from_name = function
    | "fmin" -> Fmin
    | "seval" -> Seval
    | _ as s -> failwith ("unknown benchmark "^s)

  let fmin = ["fmin.i"]
  let seval = ["spline.i"; "seval.i"]

  let get_files_from_name s =
    match (from_name s) with
      | Fmin -> fmin
      | Seval -> seval
 
end

(* module for saving solutions for some definition of saving *)
module ChowSolution =
(
struct
  type save_type = (ChowHood.resident * ChowHood.fit_result)
  let timestamp () = 
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%02d:%02d:%02d  %02d/%02d/%d" 
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      (tm.Unix.tm_mon+1) tm.Unix.tm_mday (tm.Unix.tm_year+1900)

  let file out_file (resident, (fits,sum)) = 
      (*let ts = timestamp () in
        Printf.printf "--- %s ---\n" ts;*)
      List.iter (fun (file,args,count) ->
        Printf.fprintf out_file "%s|%.0f|%s\n" file count args;
      ) fits;
      Printf.fprintf out_file "%%\n";
     flush out_file
  let std = file stdout 
end
:
sig
  type save_type = (ChowHood.resident * ChowHood.fit_result)
  val std  : save_type -> unit
  val file : out_channel -> save_type -> unit
  val timestamp : unit -> string
end
)

(* perform the search *limit* times on problems of size *size* *)
let iter_search ~limit search seed save next =
  let rec do_search n start =
    if n <= 0 then start else
      let solution = search start in
      let _ = save solution in
      let seed' = next solution in do_search (n-1) seed'
  in
  do_search limit seed

