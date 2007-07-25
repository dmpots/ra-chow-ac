open Search
open Cache
open Benchmarks

(* command line arguments *)
let sentinal = ""
let limit = ref 10
let seed_val = ref(-1)
let out_file = ref sentinal
let benchmarks = ref []
let cache = ref (NoCache.make_cache "")
let logger = ref stdout
let patience = ref 0.2
let greedy   = ref false
let check    = ref "15m"
let num_reg  = ref 32

(* functions for setting values from command line *)
let set_bench bench =
  benchmarks := (Benchmarks.get_files_from_name bench) :: !benchmarks

let set_db db =
  cache := (DbCache.make_cache db)

let set_log logfile =
  logger := (open_out logfile)

let not_set file_ref = !file_ref = sentinal

(* parse command line arguments *)
let check_args () =  ()
let parse_args () =
  let args = 
    [
     ("-greedy", Arg.Set greedy, 
       "make search greedy greedy (defaults to false)");
     ("-patience", Arg.Set_float patience, 
       "[PATIENCE] set patience (defaults to 0.20)");
     ("-out", Arg.Set_string out_file, "[OUTFILE] set output file");
     ("-db", Arg.String set_db , "[DBFILE] set database file");
     ("-log", Arg.String set_log , 
       "[LOGFILE] set log file  (defaults to stdout)");
     ("-check", Arg.Set_string check , 
       "[CHECKTIME] set checkpoint interval (default 15m)");
     ("-limit", Arg.Set_int limit, 
       "[EVALS] max number of fitness evaluations (default 10)");
     ("-seed", Arg.Set_int seed_val , 
       "[SEED] random number generator seed (default random)");
     ("-reg", Arg.Set_int num_reg, 
       "[K] set number of registers to use (default 32)");
     ("-run", Arg.Symbol (Benchmarks.valid_names, set_bench), 
       "[BENCHMARK] run search on this benchmark");
    ]
  in
  let usage = "ac -run <benchmark> [OPTIONS]" in
  Arg.parse args (fun _ -> ()) usage;
  try
    check_args ()
  with (Arg.Bad s) ->
    print_string s; print_newline ();
    Arg.usage args usage;
    exit 2

(* sort function used to sort results *)
let sorter = (fun (_,_,f1) (_,_,f2) -> compare f2 f1) 

(* build a search function from the options given. the search function
 * should take in a list of files included in the search and print the
 * results of the search *)
let build_search lim seed out_file cache logger patience greedy check nregs =
  (* create search modules *)
  let module BenchSearch = BenchmarkSearch(ChowSearchSpace(
    struct 
      let adaptable_args = ChowChoices.adaptable_args
      let fixed_args = [("r", ChowArgs.Int nregs)]
    end
  ))
  in
  let module Driver = SearchDriver(BenchSearch) in

  (* print header *)
  begin
    Printf.fprintf logger "*** BEGIN SEARCH CONFIGURATION ***\n";
    Printf.fprintf logger "* starttime: %s\n" (Util.timestamp ());
    Printf.fprintf logger "* patience: %0.2f\n" patience;
    Printf.fprintf logger "* greedy: %s\n" (string_of_bool greedy);
    Printf.fprintf logger "* seed: %d\n" seed;
    Printf.fprintf logger "* limit: %d\n" lim;
    Printf.fprintf logger "* checkpoint: %s\n" check;
    Printf.fprintf logger "* outfile: %s\n" out_file;
    Printf.fprintf logger "*** END SEARCH CONFIGURATION ***\n";
    flush logger;
  end;

  (* setup search params *)
  let _ = if seed <> -1 then Random.init seed else Random.self_init () in
  let outchan = if out_file = ""  then stdout else open_out out_file in
  let checkpoint = Driver.parse_check check in 

  (* search function *)
  (fun files -> 
    let seed     = BenchSearch.throw_dart files in
    let search   = 
      BenchSearch.create patience cache ~greedy:greedy ~logger:(Some logger)
    in
    let results = Driver.search search seed lim ~check:checkpoint in
    Printf.fprintf logger "*** finished search at: %s\n" (Util.timestamp ());
    List.iter (fun (file,args,fit) ->
      Printf.fprintf outchan "%s|%.0f|%s\n" file fit args
    ) (List.sort sorter results);
  )


(* main *)
let _ = 
  parse_args ();
  let search = 
    build_search !limit !seed_val !out_file 
                 !cache !logger !patience !greedy !check !num_reg
  in
    List.iter (fun files -> search files;) !benchmarks 

