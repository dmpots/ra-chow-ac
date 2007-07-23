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
    [("-limit", Arg.Set_int limit, "max number of fitness evaluations");
     ("-out", Arg.Set_string out_file, "output file");
     ("-db", Arg.String set_db , "database file");
     ("-log", Arg.String set_log , "set log file  (defaults to stdout)");
     ("-patience", Arg.Set_float patience, "patience (defaults to 0.20)");
     ("-greedy", Arg.Set greedy, "set greedy (defaults to false)");
     ("-check", Arg.Set_string check , "checkpoint interval (default 15m)");
     ("-seed", Arg.Set_int seed_val , "random number generator seed");
     ("-run", Arg.Symbol (Benchmarks.valid_names, set_bench), 
                                     "run search on this benchmark");
    ]
  in
  let usage = "ac -run <benchmark> [-limit n] [-seed n] [-out file]" in
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
let build_search lim seed out_file cache logger patience greedy check =
  let _ = if seed <> -1 then Random.init seed else Random.self_init () in
  let outchan = if out_file = ""  then stdout else open_out out_file in
  let checkpoint = Driver.parse_check check in begin
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
  (fun files -> 
    let seed     = BenchSearch.throw_dart files in
    let search   = 
      BenchSearch.create patience cache ~greedy:greedy ~logger:(Some logger)
    in
    let results = Driver.search search seed lim ~check:checkpoint in
    List.iter (fun (file,args,fit) ->
      Printf.fprintf outchan "%s|%.0f|%s\n" file fit args
    ) (List.sort sorter results);
    Printf.fprintf logger "*** finished search at: %s\n" (Util.timestamp ());
  )


(* main *)
let _ = 
  parse_args ();
  let search = 
    build_search !limit !seed_val !out_file 
                 !cache !logger !patience !greedy !check
  in
    List.iter (fun files -> search files;) !benchmarks 

