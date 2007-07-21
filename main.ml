open Search
open Cache
open Benchmarks

(* command line arguments *)
type algorithm = 
  | HC
  | SA

(* command line arguments *)
let sentinal = ""
let limit = ref 10
let alg = ref HC
let seed_val = ref(-1)
let out_file = ref sentinal
let benchmarks = ref []
let cache = ref (NoCache.make_cache "")
let logger = ref stdout

let set_alg a () = 
  match a with
  | HC -> alg := HC
  | SA     -> alg := SA

let set_bench bench =
  benchmarks := (Benchmarks.get_files_from_name bench) :: !benchmarks

let set_db db =
  cache := (DbCache.make_cache db)

let set_log logfile =
  logger := (open_out logfile)

let not_set file_ref = !file_ref = sentinal


let check_args () =  ()
let parse_args () =
  let args = 
    [("-limit", Arg.Set_int limit, "max number of iterations");
     ("-hc", Arg.Unit (set_alg HC) , "search using hill climber");
     ("-sa", Arg.Unit (set_alg SA) , "search using simulated annealing");
     ("-run", Arg.Symbol (Benchmarks.valid_names, set_bench), 
                                     "run search on this benchmark");
     ("-seed", Arg.Set_int seed_val , "random number generator seed");
     ("-out", Arg.Set_string out_file, "output file");
     ("-db", Arg.String set_db , "database file");
     ("-log", Arg.String set_log , "log file (defaults to stdout)");
    ]
  in
  let usage = "ac -run <benchmark> [-limit n] [-hc] [-seed n] [-out file]" in
  Arg.parse args (fun _ -> ()) usage;
  try
    check_args ()
  with (Arg.Bad s) ->
    print_string s; print_newline ();
    Arg.usage args usage;
    exit 2

let build_search lim alg seed out_file cache logger =
  (* search params *)
  let _ = if seed <> -1 then Random.init seed else Random.self_init () in
  let outchan = if out_file = ""  then stdout else open_out out_file in
  let saver = ChowSolution.file outchan in
  let nhood = ChowHood.create 
    (ChowChoices.fixed, ChowChoices.adaptable, cache) in
  let searchfun = 
    match alg with
      | HC -> HCC.search ~log:logger nhood
      | _ -> failwith "unsupported search algorithm"
  in
  (fun files -> 
    let gen = (fun _ -> ChowHood.random_resident nhood files) in
    let seed = gen ()  in
    Search.iter_search ~limit:lim searchfun seed saver gen
  )

let _ = 
  parse_args ();
  let search = build_search !limit !alg !seed_val !out_file !cache !logger in
    List.iter (fun files ->
      let _ = search files in ()
    ) !benchmarks 

