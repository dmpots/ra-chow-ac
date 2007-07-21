(* Caching mechanisms *)
type cache = {
  find : string -> string -> float option;
  add  : string -> string -> float -> unit;
}

module type CACHE =
sig
  type t
  val create : string -> t
  val find   : t -> string -> string ->  float option
  val add    : t -> string -> string ->  float -> unit
  val make_cache : string -> cache
end

module NoCache : CACHE =
struct
  type t = unit
  let create _ = ()
  let find _ _ _  = None 
  let add _ _ _ _  = () 

  let make_cache _ = {
    find = find ();
    add  = add ();
  }
end
let no_cache = NoCache.make_cache ""

module ListCache : CACHE =
struct
  type t = {
    mutable c : (string*string*float) list;
  }
  let create _ = {c=[]}
  let find c s1 s2 =
    try
      let _,_,f =
        List.find (fun (s', s'', _) -> (s' = s1 && s'' = s2)) c.c
      in
      Some f
    with Not_found -> None

  let add c s1 s2 f = c.c <- ((s1,s2,f)::c.c)

  let make_cache s = 
    let c = create s in 
    {
      find = find c;
      add  = add c;
    }
end

module DbCache  : CACHE =
struct
  type t = Sqlite3.db

  module Rc = Sqlite3.Rc
  let schema ="
CREATE TABLE cache (
  id        INTEGER PRIMARY KEY,
  filename  VARCHAR,
  args      VARCHAR,
  fitness   FLOAT,
  hits      INTEGER DEFAULT 0
);

CREATE INDEX cache_args_index ON cache(args);
"
  (* helpers *)
  let atoi = int_of_string
  let sql_fail status = failwith ("sql failed: "^(Rc.to_string status))
  let assert_ok status retval = 
    match status with
      | Rc.OK -> retval
      | _ -> sql_fail status

  (* create *)
  let create file = 
    let need_schema = not (Sys.file_exists file) in
    let db = Sqlite3.db_open file in
    if need_schema then 
      let status = Sqlite3.exec db schema in assert_ok status db
    else
      db

  let update_hits db (fitness,hits, row_id) = 
    match fitness with
      | None -> None
      | Some(fit) -> 
        (* update hits column *)
        let sql = Printf.sprintf "
          UPDATE cache set hits = %d where id = %d
        " (hits+1) row_id
        in
        let _ = assert_ok (Sqlite3.exec db sql) () in fitness

  (* find *)
  let find db file args = 
    let sql = Printf.sprintf "
      SELECT fitness,hits,id FROM cache 
      WHERE filename = '%s' 
      AND args = '%s'
      LIMIT 1;" file args
    in
    let hit = ref (None,-1, -1) in
    let status = 
      Sqlite3.exec_not_null_no_headers db ~cb:(fun row ->
        hit := (Some(float_of_string row.(0)), atoi row.(1), atoi row.(2))
      ) sql
    in
    let _ = assert_ok status () in
    update_hits db !hit

  (* add *)
  let add db file args fitness = 
    let sql = Printf.sprintf "
      INSERT INTO cache(filename,args,fitness) VALUES ('%s','%s',%.2f);
      " file args fitness
    in
    assert_ok (Sqlite3.exec db sql) ()


  (* make_cache *)
  let make_cache file =
    let db = create file in
    {
      find = find db;
      add = add db;
    }
end

