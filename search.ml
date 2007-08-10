open Cache

(*================================================================
 * EXTERNAL FITNESS
 *===============================================================
 * Computes fitness for neighborhoods that specify specific args per
 * fuction.
 * Access an external program for computing fitness. This is used to
 * run the iloc compiler and then use the operation count for the
 * fitness function.
 *
 *  The i/o contract with external programs is as follows:
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
 *-------------------------------------------------------------*)
module ExternalFitness =
struct
  type input_element = (string * string)
  type output_element = (string * string * float)
  type input = input_element list
  type output = output_element list
  let extrn_prog = "run-tw"
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
    if resident = [] then [] 
    else List.hd (bulk_fitness [resident])
end

(*================================================================
 * SEARCH INTERFACE
 *================================================================
 * These modules define the interface used for running searching
 *---------------------------------------------------------------*)
module type SEARCH =
sig
  type t 
  type elem
  type search_state
  type fitness_input  = ExternalFitness.input
  type fitness_output = ExternalFitness.output

  val make_state : t -> elem -> search_state
  val best_value : t -> search_state -> fitness_output
  val get_next_eval : t -> search_state -> fitness_input * search_state
  val apply_results : t -> search_state -> fitness_output -> search_state
  val fitness : t -> fitness_input -> fitness_output
  val checkpoint : t -> search_state -> int -> unit
end

module type SEARCHSPACE = 
sig
  type elem
  val to_fitness_input   : elem -> ExternalFitness.input_element
  val from_fitness_input : ExternalFitness.input_element -> elem
  val random_point  : elem -> elem
  val all_neighbors : elem -> elem list
end


(*===============================================================
 * SINGLE FILE SEARCH
 *===============================================================
 * Module for searching over params on just a single file in a
 * benchmark.
 *---------------------------------------------------------------
 *)
(* type for encapsulating the state of a search *)
type 'state file_search_state = {
  current : 'state;
  fitness : float option;
  best : ('state * float);
  to_explore : 'state list;
  neighbors : ('state * float) list;
  restarts : int;
  patience : float;
  greedy : bool;
  cache : Cache.cache;
  file : string;
}

(* search module *)
module SingleFileSearch(SS: SEARCHSPACE) = 
struct
(* ---------- space specific --------*)
  type elem = SS.elem
  let to_fitness_input   = SS.to_fitness_input
  let from_fitness_input = SS.from_fitness_input
  let random_point = SS.random_point
  let all_neighbors = SS.all_neighbors
(* ---------- space specific --------*)
  type search_state = elem file_search_state

  (* computation for number of neighbors to explore based on patience *)
  let num_nebs_to_explore all patience = 
      int_of_float (ceil ((float (List.length all) ) *.  patience)) 

  (* choose random neighbors of the element. number based on patience *)
  let choose_random_neighbors elem patience =
    let all = all_neighbors elem in
    Util.RandomList.pick (num_nebs_to_explore all patience) all

  (* query the cache to see if the element is present *)
  let lookup_fitness cache elem =
    let file,args = to_fitness_input elem in cache.find file args

  (* make a new search state from the given values *)
  let make_state (seed : elem) cache ~patience:patience ~greedy:greed = 
    let state = {
        current = seed;
        fitness = None;
        best    = seed, max_float;
        to_explore = choose_random_neighbors seed patience;
        neighbors = [];
        restarts = 0;
        patience = patience;
        greedy = greed;
        cache = cache;
        file  = fst (to_fitness_input seed)
      }
    in 
    state

  (* return either the current best or this neighbor as best whichever
     is better *)
  let  new_best state neighbor fit =
    let _,bestfit = state.best in
      if fit < bestfit then (neighbor,fit) else state.best
    
    
  (* move the search state to have the neighbor be the current position *)
  let move_to_neighbor neighbor fit state  =
      { state with
        current = neighbor;
        fitness = Some fit;
        to_explore = choose_random_neighbors neighbor state.patience;
        neighbors = [];
        best = new_best state neighbor fit }

  (* record the fitness of the neighbor so that we may choose it later *)
  let record_neighbor_fitness neighbor fit state =
    { state with
      neighbors = (neighbor,fit)::state.neighbors;
      best = new_best state neighbor fit; }
    
  (* restart the search at a new random place in the space *)
  let restart_search state = 
    let new_start = random_point state.current in
      {state with
       current = new_start;
       fitness = None;
       to_explore = choose_random_neighbors new_start state.patience;
       neighbors = [];
       restarts = state.restarts + 1;}

  (* return the fitness value for the curren state, raise exception if
     the fitness has not yet been computed *)
  let get_current_fitness state =  
    match state.fitness with
      | Some fit -> fit
      | None -> failwith "oops, the current state has no fitness"

  (* pick the next neighbor from the to_explore queue *)
  let next_neighbor state = 
    if state.to_explore = [] then (None, state)
    else  
      let hd = List.hd state.to_explore in
      let new_state = {state with to_explore = List.tl state.to_explore;} in
      (Some(hd), new_state)

  (* pick best neighbor from all evaluated neighbors *)
  let pick_best_neighbor state = 
    List.fold_left (fun (best,bestfit) (neighbor,fit) ->
      if fit < bestfit then (neighbor,fit) else (best,bestfit)
    ) (List.hd state.neighbors) (List.tl state.neighbors)

  (* find a neighbor that has never been evaluated and return it or if
     no such neighbor exists then return None.
     to find an unevaluated neighbor:
      look at queue, 
      if queue is empty then return None
        
      else queue is not empty so look at top
      if we have never seen this before
        return this as the unevaluated neighbor
      else 
        if it is better and we are greedy then take it 
        otherwise otherwise record the value and look at next value
    *)
  let rec find_unevaluated_neighbor state  =
    let (neighbor,state) = next_neighbor state in
    match neighbor with
      | None -> None,state
      | Some neb -> begin
          let file,args = to_fitness_input neb in
          match state.cache.find file args  with
            | None -> Some (file,args),state (* cache miss *)
            | Some fit -> (* cache hit *)
                let curfit = get_current_fitness state in
                let new_state = 
                  if fit < curfit && state.greedy then
                    move_to_neighbor neb fit state 
                  else
                    record_neighbor_fitness neb fit state
                in
                  find_unevaluated_neighbor new_state 
        end


  (* return the next evalutation needed for the search *)
  let rec get_next_eval state =
    (* see if our current state needs its fitness computed *)
    match state.fitness with
      (* current state needs fitness recorded *)
      | None   -> begin
          (* lookup in the cache for the current state *)
          match lookup_fitness state.cache state.current with
            | None -> (to_fitness_input state.current), state
            | Some fit -> get_next_eval 
                {state with 
                  fitness = Some fit;
                  best = new_best state state.current fit; } 
        end
      (* current state already has fitness recorded *)
      | Some _ -> begin
        (* find an unevaluated neighbor to explore *)
        match find_unevaluated_neighbor state  with
          | Some n,state' -> n, state'
          | None,state' -> 
              (* here is where we restart or pick the best neighbor *)
              let new_state = 
                if state'.greedy then restart_search state'
                else 
                  (* look at all evaluated neighbors. if one is better
                     then move to it, else time to restart the search *)
                  let bestneb,nebfit = pick_best_neighbor state' in
                  if nebfit < (get_current_fitness state') then
                    move_to_neighbor bestneb nebfit state'
                  else
                    restart_search state'
              in
              get_next_eval new_state 
        end
          
  (* return the next evalutation needed for the search *)
  let apply_results state (fitness_output : ExternalFitness.output_element) =
    let (file,args,fitness) = fitness_output in
    let elem = from_fitness_input (file,args) in
    state.cache.add file args fitness; (* update cache *)
      match state.fitness with 
        | None -> 
            (* current state has unknown fitness. make sure that is
             * the fitness we are applying (ju,jj just for assert) *)
            let ju,jj = to_fitness_input state.current in
            assert (ju = file && jj = args);
            { state with 
              fitness = Some(fitness);
              best = new_best state elem fitness; } 
        | Some curfit -> 
            (* if this element is better and we are greedy then move
               there *)
            if fitness < curfit && state.greedy then
              move_to_neighbor elem fitness state
            else
              record_neighbor_fitness elem fitness state
end

(*
 *==============================================================
 * BENCHMARK SEARCH
 *===============================================================
 * Module that lifts the search from single files to entire
 * benchmarks. The search is paramertized by the search space over
 * which the search is performed. A single file search module is used
 * to search over each individual file in independent searches. This
 * module just exists to tie all the searches together into one
 * convienient interface.
 *---------------------------------------------------------------
 *)
module BenchmarkSearch (SS : SEARCHSPACE) =
struct
  module SFS = SingleFileSearch(SS)
  type t = {
    pat : float;
    cash : cache;
    greed : bool;
    logger : out_channel option;
  }
  type elem = SFS.elem list
  type search_state   = SFS.search_state list
  type fitness_input  = ExternalFitness.input
  type fitness_output = ExternalFitness.output

  (* create : float -> cache -> greedy:bool -> t 
   * creates new search with given params *)
  let create patience ?logger:(log=None) ~greedy:greedy cache = { 
    pat = patience;
    cash = cache;
    greed = greedy;
    logger = log;
  }

  (* make_state : t -> elem -> search_state
   * the state is just an ordered collection of individual file 
   * searches. it is important that the searches remain sorted as this
   * order is used to apply results since we do not have a guarantee
   * that the external fitness will return results in the same order
   * that we give them.
   *)
  let make_state config seeds = 
    let states = 
      List.map (fun seed -> 
        SFS.make_state 
          seed config.cash ~patience:config.pat ~greedy:config.greed
      ) seeds
    in
    List.sort (fun s1 s2 -> compare s1.file s2.file) states

  (* best_value : t -> search_state -> fitness_output *)
  let best_value _ searches = 
    List.map (fun s -> 
      let best,fit = s.best in 
      let file,args = SFS.to_fitness_input best  in
      (file,args,fit)
    ) searches 

  (* get_next_eval : t -> search_state -> fitness_input * search_state *)
  let get_next_eval _ searches = 
    List.split (List.map (fun s -> SFS.get_next_eval s) searches)

  (* apply_results : t -> search_state -> fitness_output -> search_state 
   * the results are sorted to be in the same order as the list of
   * individual file searches we are conducting. this means that the
   * searches list must remain sorted.
   *) 
  let apply_results _ searches fitness_output =
    let order_results = 
      List.sort (fun (file1,_,_) (file2,_,_) -> compare file1 file2)
    in
    List.map2 (fun s fop -> 
      SFS.apply_results s fop
    ) searches (order_results fitness_output)

  (* fitness : t -> fitness_input -> fitness_output *)
  let fitness _ fitness_input = 
    ExternalFitness.single_fitness fitness_input

  (* throw_dart : string list -> elem *)
  let throw_dart files  =  
    let elems = List.map (fun f -> SFS.from_fitness_input (f,"")) files in
    List.map (fun elem -> SFS.random_point elem) elems

  (* checkpoint: t -> search_state -> int -> elem *)
  let checkpoint config search_state evals = 
    match config.logger with
      | None -> ()
      | Some out ->
        let ts = Util.timestamp () in
        let sorter = (fun (_,f1,_,_) (_,f2,_,_) -> compare f2 f1) in
        let bests = List.map (fun state ->
          let best,fit = state.best in
          let file,args = SFS.to_fitness_input best in
          (file, fit, args, state.restarts)
        ) search_state
        in 
        Printf.fprintf out "*** CHECKPOINT (%d evals): %s ***\n" evals ts;
        List.iter (fun (file,fit,args,restarts) ->
          Printf.fprintf out "%s|%.0f|%s|%d\n" file fit args restarts
        ) (List.sort sorter bests);
        flush out
end



(*================================================================
 *  SEARCH DRIVER
 *===============================================================
 * This module is the driver for the actual search. It is paramertized
 * by a search module that is used to do the actual searching. This
 * module takes care of running the search until the limit is reached
 * and performing checkpoint operations at appropriate times.
 *---------------------------------------------------------------*)
module SearchDriver (S : SEARCH) = 
struct
  (* types for describing when to save checkpoints of the search *)
  type time_desc = Sec of int | Min of int | Hour of int
  type check_time = Eval of int | Time of time_desc
  type check_points = {
    checktime : check_time;
    mutable last_save : float;
  }

  (* returns a check_time from the given string. the format is an
   * integer followed by either e,s,m,h to denote the type. i.e
   * 10m = 10 minutes
   * 10e = 10 evals *)
  let parse_check str = 
    let len  = String.length str  in
    let ival = int_of_string (String.sub str 0 (len - 1)) in
    match String.get str (len - 1) with
      | 'e' -> Eval ival
      | 's' -> Time (Sec ival)
      | 'm' -> Time (Min ival)
      | 'h' -> Time (Hour ival)
      | _ -> failwith "checkval must end with e,s,m or h"

 
  (* return the time in seconds (float) for the time_desc *)
  let time_in_secs = function
    | Sec s -> float_of_int s
    | Min m -> float_of_int (m * 60)
    | Hour h -> float_of_int (h * 60 * 60)

  (* report whether or not we need a checkpoint *)
  let need_checkpoint check_points evals =
    match check_points.checktime with 
      | Eval nevals -> (evals mod nevals) = 0 && evals > 0
      | Time t -> begin
        let wait_time = time_in_secs t in
        let now = Unix.time () in
        check_points.last_save +. wait_time <= now
      end

  (* save a checkpoint of the search if needed *)
  let save_checkpoint hood search_state check_points evals =
    if need_checkpoint check_points evals then begin
      S.checkpoint hood search_state evals;
      check_points.last_save <- Unix.time ();
    end

  (* run the search until +limit+ fitness evaluations are reached *)
  let search ?check:(checktime=Eval(15))(hood : S.t) (seed : S.elem) limit =
    let check_points = {checktime = checktime; last_save = Unix.time ();} in
    let rec do_search evals search_state =
      if evals >= limit then (S.best_value hood search_state) else
      let _ = save_checkpoint hood search_state check_points evals in
      let next_eval,new_state = S.get_next_eval hood search_state in
      let fit_results = S.fitness hood next_eval in
      let new_state' = S.apply_results hood new_state fit_results in
      do_search (evals+1) new_state'
    in
    do_search 0 (S.make_state hood seed)
end

(*================================================================
 * CHOW ARGS
 *===============================================================
 * Definition of arguments that are passed to the chow allocator and
 * their internal representation in the search program.
 *---------------------------------------------------------------*)
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
    ("-n", Arg.Unit (make_bool_arg "n"), "");
  ]

  let sort_order (arg,value) =
    match arg with
      | "b" -> 1
      | "r" -> 0
      | "p" -> 2
      | "m" -> 3
      | "e" -> 4
      | "f" -> 5
      | "y" -> 6
      | "z" -> 7
      | "t" -> 8
      | "c" -> 9
      | "i" -> 10
      | "w" -> 11
      | "s" -> 12
      | "g" -> 13
      | "o" -> 14
      | "a" -> 15
      | "n" -> 16
      | "l" -> 17
      | _ -> failwith ("unknown arg: "^arg)

  let sorted_order (args : chow_arg list) =
    List.sort (fun arg1 arg2 ->
      compare (sort_order arg1) (sort_order arg2)
    ) args

  let from_string str  : chow_arg list =
    let split = Array.of_list ("dummy"::(Str.split spaces str)) in
    arg_accum := [];
    Arg.parse_argv ~current:(ref 0) split all_args (fun _ -> ()) "";
    !arg_accum

  let expand_choices (choices : arg_choices) : arg_val list =
    match choices with
      | BoolC bools -> List.map (fun b -> Bool b) bools
      | IntC    ints -> List.map (fun i -> Int i) ints
      | FloatC  floats -> List.map (fun f -> Float f) floats
      | TupleC tups -> List.map (fun t -> Tuple t) tups
   
 (*  removes the choice from choices corresponding to the current
     value of arg_val *)
 let remove_choice arg_val choices = 
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
    match arg_val with
      | Bool b   -> BoolC [not b]
      | Int i    -> IntC   (filter_int_list i choices)
      | Float f  -> FloatC (filter_float_list f choices)
      | Tuple t  -> TupleC (filter_tuple_list t choices)

end


(*===============================================================
 * CHOW CHOICES
 *===============================================================
 * Module containing the definition of paramerters that can be adapted
 * for the chow register allocator.
 *)
module ChowChoices = 
struct

  let combine list1 list2 =
    List.flatten (
      List.map (fun elem ->
        List.map (fun elem2 ->
          (elem, elem2)
        ) list2
      ) list1
    )
  let upto limit step =
    let rec aux n ns = 
      if n > limit then ns else aux (n+step) (n::ns)
    in
    List.rev (aux 0 [])

  let fixed_args = [
    ("r", ChowArgs.Int 32)
  ]
  let adaptable_args_for_k k =[
    ("b", ChowArgs.IntC [0;2;3;4;5;6;7;8;9;10;15]);
    ("l", ChowArgs.TupleC (combine (upto (k-4) 1) (upto (k-6) 2)));
    ("e", ChowArgs.BoolC  [true; false]);
    ("z", ChowArgs.BoolC  [true; false]);
    ("t", ChowArgs.BoolC  [true; false]);
    ("c", ChowArgs.IntC   [0;1;2;3]);
    ("s", ChowArgs.IntC   [0;1]);
    ("g", ChowArgs.BoolC  [true; false]);
    ("o", ChowArgs.BoolC  [true; false]);
    ("a", ChowArgs.BoolC  [true; false]);
    ("n", ChowArgs.BoolC  [true; false]);
  ]
  let adaptable_args = adaptable_args_for_k 32

  module R16 =
  struct
    let fixed_args = [("r", ChowArgs.Int 16)]
    let adaptable_args = adaptable_args
  end
  module R32 =
  struct
    let fixed_args = [("r", ChowArgs.Int 32)]
    let adaptable_args = adaptable_args
  end
end

(* ---------------------- SEARCH SPACES ----------------------*)
(*===============================================================
 * PASS SEARCH SPACE
 *===============================================================
 * Module containing the definition of the search space used to find
 * good sequences of optimizations for the iloc compiler.
 *---------------------------------------------------------------
 *)
module type PASSCONFIG =
sig
  val passes : string list
end

module StdPass =
struct
let passes = ["a"; "b"; "c"; "d"; "g"; 
              "l"; "m"; "n"; "o"; "p"; 
              "q"; "r"; "s"; "t"; "v"; "z"]
end

module PassSearchSpace(Config : PASSCONFIG) = 
struct
  type file = string
  type args = string
  type elem = file * args 

  let to_fitness_input elem = elem
  let from_fitness_input elem = elem
  let random_point (file,arg) = 
    (file, List.hd(Util.RandomList.pick 1 Config.passes))
  let all_neighbors (file,_) = 
    List.map (fun a -> (file,a)) Config.passes 
end


(*
 *==============================================================
 * CHOW SEARCH SPACE
 *===============================================================
 * Module containing the definition of the search space used to tune
 * the chow register allocator.
 *---------------------------------------------------------------
 *)
module type CHOWCONFIG =
sig
  val adaptable_args : ChowArgs.spec list
  val fixed_args     : ChowArgs.chow_arg list
end


module ChowSearchSpace(Config : CHOWCONFIG)  =
struct
  open ChowArgs
  type file = string
  type elem = file * (chow_arg list)

  (* convet the resident to a format usable with external fitness *)
  let to_fitness_input (file, args) =
    (file, Util.join (ChowArgs.sorted_order args) " " ChowArgs.to_string)

  (* convet the resident to internal format *)
  let from_fitness_input (file, arg_str) =
    (file, ChowArgs.from_string arg_str)
    

  (* choose randomly among the valid choices for an arg *)
  let take_random_choice (choices : arg_choices) : arg_val =
    match choices with
      | BoolC  lst -> Bool  (Util.RandomList.random_choice lst)
      | IntC   lst -> Int   (Util.RandomList.random_choice lst)
      | FloatC lst -> Float (Util.RandomList.random_choice lst)
      | TupleC lst -> Tuple (Util.RandomList.random_choice lst)

  (* return a random point in the search space *)
  let random_point (file,_) =
    let adaptables = 
      List.map (fun (name, choices) -> 
        (name, take_random_choice choices)
      ) Config.adaptable_args
    in
    (file, Config.fixed_args @ adaptables)

  (* return a all neighbors of a given element. neighbors of an
     element are found by taking each arg and returing all valid values
     for that arg, excluding the current choice for the arg. while
     keeping the values for the other args fixed. *)
  let all_neighbors ((file,args) : elem) : elem list =
    let same_name name (n,_) = name = n in
    let diff_name name (n,_) = name <> n in
    let get_current_choice arg_name arg_choices =
      try
        let (_,current_choice) = List.find (same_name arg_name) args in
        current_choice
      with Not_found -> (*assert that the param is a bool param *)
        assert (match arg_choices with | BoolC _ -> true | _ -> false);
        Bool false
    in
        
    List.fold_left (fun nebs (arg_name, arg_choices) ->
      let current_choice = get_current_choice arg_name arg_choices in
      let valid_choices = ChowArgs.remove_choice current_choice arg_choices in
      let remaining_args = List.filter (diff_name arg_name) args in
      (* add all new choices to remaining args *)
      List.fold_left (fun new_neighbors new_choice ->
        (file, ((arg_name,new_choice)::remaining_args))::new_neighbors
      ) nebs (ChowArgs.expand_choices valid_choices)
    ) [] Config.adaptable_args
    
end

(* -------------------------- FOR MAIN ------------------------*)
(*
module CSS32 = ChowSearchSpace(ChowChoices.R32)
module CSS16 = ChowSearchSpace(ChowChoices.R16)
module BenchSearch = BenchmarkSearch(CSS32)
module Driver = SearchDriver(BenchSearch)
*)


(* -------------------------- FOR TESTING ------------------------*)
(*
let my_passes = ["a"; "b"; "c"; "d"; "e"; "f"; "g"]
let greedy = false
module SF = 
  SingleFileSearch(PassSearchSpace(struct let passes = my_passes end))
let state =  SF.make_state ("fmin.i", "abcd") Cache.no_cache ~patience:0.5 ~greedy:greedy;; 
let state2 = {state with fitness = Some 100.0}
let state' = SF.apply_results state ("fmin.i", "abcd", 22.0);;
let (_,state'') = SF.get_next_eval state'

let ten = ChowChoices.upto 9 1 
let five = ChowChoices.upto 5 1 
let fits  = [4.76; 75.7; 14.89; 96.58; 32.12; 2.32]
let gfits = [30.0; 20.0; 10.0; 15.0; 50.0; 25.0]

let apply state times fits = 
  let next_fit i = List.nth fits i in
  List.fold_left (fun (state, states) i -> 
    let ((file,args), state') = SF.get_next_eval state in
    let fit = next_fit i in
    Printf.printf "APPLIED EVAL: %s,%s %.2f\n" file args fit;
    let state'' = SF.apply_results state' (file, args, fit) in
    state'',state''::states
  ) (state, []) times


let rp1 = CSS.random_point ("",[])
let nebs = CSS.all_neighbors rp1

let anyeq lst =
  let args_eq (_,x) (_,y) = (ChowArgs.sorted_order x) = (ChowArgs.sorted_order y) in
  List.fold_left (fun (search,found) x ->  
    if search = [] then ([], found) else
    (List.tl search, (List.exists (args_eq x) search || found))
  ) (List.tl lst, false) lst


module SFC = SingleFileSearch(CSS)
let cs = SFC.make_state rp1 Cache.no_cache ~patience:0.5 ~greedy:true;; 
let cs' = SFC.get_next_eval cs

module BS = BenchmarkSearch(SFC)
module CSD = SearchDriver(BS)
let seed = BS.throw_dart ["fmin.i"]
let bs = BS.create 0.5 Cache.no_cache ~greedy:true ~logger:(Some stdout)
*)

