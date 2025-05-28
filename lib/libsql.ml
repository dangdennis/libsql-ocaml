(** LibSQL OCaml API - Idiomatic bindings for libSQL database *)

(** {1 Types} *)

(** Configuration options for database connections *)
type config = {
  auth_token : string option;
  read_your_writes : bool option;
  encryption_key : string option;
  sync_interval : float option; (* in seconds *)
}

(** Replication status information *)
type replicated = {
  frame_no : int;
  frames_synced : int;
}

(** Database connection configuration records with full config embedded *)
type local_config = {
  path : string;
  auth_token : string option;
  read_your_writes : bool option;
  encryption_key : string option;
  sync_interval : float option;
}

type remote_config = {
  url : string;
  auth_token : string option;
  read_your_writes : bool option;
  encryption_key : string option;
  sync_interval : float option;
}

type replica_config = {
  db_path : string;
  primary_url : string;
  auth_token : string option;
  read_your_writes : bool option;
  encryption_key : string option;
  sync_interval : float option;
}

(** Database connection types with named fields *)
type connection_type =
  | Local of local_config
  | Remote of remote_config  
  | EmbeddedReplica of replica_config
  | SyncedDatabase of replica_config

(** Abstract database handle - using C bindings *)
type database = Libsql_bindings.libsql_database_t

(** Abstract connection handle *)
type connection = Libsql_bindings.libsql_connection_t

(** Abstract statement handle *)
type statement = Libsql_bindings.libsql_stmt_t

(** Abstract transaction handle *)
type transaction = unit

(** SQL parameter types *)
type param = 
  | Null
  | Int of int
  | Int64 of int64
  | Float of float
  | Text of string
  | Blob of bytes

(** SQL row result *)
type row = (string * param) list

(** Query result *)
type result = {
  rows : row list;
  last_insert_rowid : int64 option;
  rows_affected : int64 option;
}

(** {1 Exceptions} *)

exception Libsql_error of int * string

(** {1 Configuration Builders} *)

let default_config = {
  auth_token = None;
  read_your_writes = None;
  encryption_key = None;
  sync_interval = None;
}

(** OCaml-idiomatic configuration using optional labeled arguments *)
let make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () = {
  auth_token;
  read_your_writes;
  encryption_key;
  sync_interval;
}

(** Update configuration using functional record update *)
let update_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval base_config =
  {
    auth_token = (match auth_token with Some v -> Some v | None -> base_config.auth_token);
    read_your_writes = (match read_your_writes with Some v -> Some v | None -> base_config.read_your_writes);
    encryption_key = (match encryption_key with Some v -> Some v | None -> base_config.encryption_key);
    sync_interval = (match sync_interval with Some v -> Some v | None -> base_config.sync_interval);
  }

(** {1 Database Operations} *)

(** Open a database connection with optional configuration *)
let open_database ?config connection_type = 
  let open Ctypes in
  let _ = config in (* Suppress unused warning for now *)
  let db_ptr = allocate (ptr void) (from_voidp void null) in
  let error_ptr = allocate (ptr char) (from_voidp char null) in
  
  let result = match connection_type with
    | Local local_config ->
        Libsql_bindings.libsql_open_file local_config.path db_ptr error_ptr
    | Remote remote_config ->
        let auth_token = Option.value remote_config.auth_token ~default:"" in
        Libsql_bindings.libsql_open_remote remote_config.url auth_token db_ptr error_ptr
    | EmbeddedReplica replica_config | SyncedDatabase replica_config ->
        let auth_token = Option.value replica_config.auth_token ~default:"" in
        let read_your_writes_char = match replica_config.read_your_writes with
          | Some true -> Char.chr 1
          | _ -> Char.chr 0
        in
        let encryption_key = Option.value replica_config.encryption_key ~default:"" in
        Libsql_bindings.libsql_open_sync 
          replica_config.db_path 
          replica_config.primary_url 
          auth_token 
          read_your_writes_char 
          encryption_key 
          db_ptr 
          error_ptr
  in
  
  if result = 0 then
    !@ db_ptr
  else
    let error_msg = 
      let err_ptr = !@ error_ptr in
      if not (is_null err_ptr) then
        coerce (ptr char) string err_ptr
      else
        "Unknown error opening database"
    in
    raise (Libsql_error (result, error_msg))

(** Convenience functions for opening specific connection types *)
let open_local ?config path = 
  open_database ?config (Local { 
    path; 
    auth_token = None; 
    read_your_writes = None; 
    encryption_key = None; 
    sync_interval = None 
  })

let open_remote ?config ?auth_token url = 
  open_database ?config (Remote { 
    url; 
    auth_token; 
    read_your_writes = None; 
    encryption_key = None; 
    sync_interval = None 
  })

let open_embedded_replica ?config ~db_path ~primary_url () = 
  let cfg = Option.value config ~default:default_config in
  open_database (EmbeddedReplica { 
    db_path; 
    primary_url; 
    auth_token = cfg.auth_token;
    read_your_writes = cfg.read_your_writes;
    encryption_key = cfg.encryption_key;
    sync_interval = cfg.sync_interval;
  })

let open_synced_database ?config ~db_path ~primary_url () = 
  let cfg = Option.value config ~default:default_config in
  open_database (SyncedDatabase { 
    db_path; 
    primary_url; 
    auth_token = cfg.auth_token;
    read_your_writes = cfg.read_your_writes;
    encryption_key = cfg.encryption_key;
    sync_interval = cfg.sync_interval;
  })

(** Close a database *)
let close_database db = 
  Libsql_bindings.libsql_close db

(** Sync a replicated database *)
let sync _db = 
  (* TODO: Implement with C bindings *)
  { frame_no = 0; frames_synced = 0 }

(** Connect to a database *)
let connect db = 
  let open Ctypes in
  let connection_ptr = allocate (ptr void) (from_voidp void null) in
  let error_ptr = allocate (ptr char) (from_voidp char null) in
  
  let result = Libsql_bindings.libsql_connect db connection_ptr error_ptr in
  
  if result = 0 then
    !@ connection_ptr
  else
    let error_msg = 
      let err_ptr = !@ error_ptr in
      if not (is_null err_ptr) then
        coerce (ptr char) string err_ptr
      else
        "Unknown error connecting to database"
    in
    raise (Libsql_error (result, error_msg))

(** Close a connection *)
let close_connection conn = 
  Libsql_bindings.libsql_disconnect conn

(** {1 Statement Operations} *)

(** Prepare a SQL statement *)
let prepare _conn _sql = 
  (* TODO: Implement with C bindings *)
  ()

(** Close a prepared statement *)
let close_statement _stmt = 
  (* TODO: Implement with C bindings *)
  ()

(** Execute a prepared statement with parameters *)
let execute _stmt _params = 
  (* TODO: Implement with C bindings *)
  { rows = []; last_insert_rowid = None; rows_affected = None }

(** Execute a prepared statement with named parameters *)
let execute_named _stmt _params = 
  (* TODO: Implement with C bindings *)
  { rows = []; last_insert_rowid = None; rows_affected = None }

(** Query with a prepared statement *)
let query _stmt _params = 
  (* TODO: Implement with C bindings *)
  []

(** Query with named parameters *)
let query_named _stmt _params = 
  (* TODO: Implement with C bindings *)
  []

(** {1 Direct Query Operations} *)

(** Execute a SQL query directly *)
let execute_query _conn _sql _params = 
  (* TODO: Implement with C bindings *)
  { rows = []; last_insert_rowid = None; rows_affected = None }

(** Execute a SQL query with named parameters *)
let execute_query_named _conn _sql _params = 
  (* TODO: Implement with C bindings *)
  { rows = []; last_insert_rowid = None; rows_affected = None }

(** Query and return rows *)
let query_rows _conn _sql _params = 
  (* TODO: Implement with C bindings *)
  []

(** Query and return rows with named parameters *)
let query_rows_named _conn _sql _params = 
  (* TODO: Implement with C bindings *)
  []

(** {1 Transaction Operations} *)

(** Begin a transaction *)
let begin_transaction _conn = 
  (* TODO: Implement with C bindings *)
  ()

(** Commit a transaction *)
let commit _tx = 
  (* TODO: Implement with C bindings *)
  ()

(** Rollback a transaction *)
let rollback _tx = 
  (* TODO: Implement with C bindings *)
  ()

(** Execute operations within a transaction *)
let with_transaction conn f =
  let tx = begin_transaction conn in
  try
    let result = f tx in
    commit tx;
    result
  with exn ->
    rollback tx;
    raise exn

(** {1 Convenience Functions} *)

(** Execute a simple query without parameters *)
let exec conn sql = 
  let _ = execute_query conn sql [] in
  ()

(** Get a single row result *)
let query_one conn sql params = 
  match query_rows conn sql params with
  | [] -> None
  | row :: _ -> Some row

(** Get a single row with named parameters *)
let query_one_named conn sql params = 
  match query_rows_named conn sql params with
  | [] -> None
  | row :: _ -> Some row

(** Get a single value from a query *)
let query_value conn sql params = 
  match query_one conn sql params with
  | None -> None
  | Some [] -> None
  | Some ((_, value) :: _) -> Some value

(** {1 Row Helpers} *)

(** Get a column value by name *)
let get_column row name = 
  List.assoc_opt name row

(** Get a column value by name, with type conversion *)
let get_int row name = 
  match get_column row name with
  | Some (Int i) -> Some i
  | _ -> None

let get_int64 row name = 
  match get_column row name with
  | Some (Int64 i) -> Some i
  | Some (Int i) -> Some (Int64.of_int i)
  | _ -> None

let get_float row name = 
  match get_column row name with
  | Some (Float f) -> Some f
  | Some (Int i) -> Some (Float.of_int i)
  | _ -> None

let get_string row name = 
  match get_column row name with
  | Some (Text s) -> Some s
  | _ -> None

let get_bytes row name = 
  match get_column row name with
  | Some (Blob b) -> Some b
  | _ -> None

(** Get a column value by name, raising exception if not found *)
let require_column row name = 
  match get_column row name with
  | Some value -> value
  | None -> failwith ("Column not found: " ^ name)

let require_int row name = 
  match get_int row name with
  | Some i -> i
  | None -> failwith ("Column not found or not an int: " ^ name)

let require_int64 row name = 
  match get_int64 row name with
  | Some i -> i
  | None -> failwith ("Column not found or not an int64: " ^ name)

let require_float row name = 
  match get_float row name with
  | Some f -> f
  | None -> failwith ("Column not found or not a float: " ^ name)

let require_string row name = 
  match get_string row name with
  | Some s -> s
  | None -> failwith ("Column not found or not a string: " ^ name)

let require_bytes row name = 
  match get_bytes row name with
  | Some b -> b
  | None -> failwith ("Column not found or not bytes: " ^ name)

(** {1 Idiomatic OCaml Connection API} *)

(** Create connection types with labeled arguments - much more OCaml-like *)
module Connection = struct
  
  (** Create a local file database connection *)
  let local ?auth_token ?read_your_writes ?encryption_key ?sync_interval path =
    let config = make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () in
    Local { 
      path; 
      auth_token = config.auth_token; 
      read_your_writes = config.read_your_writes; 
      encryption_key = config.encryption_key; 
      sync_interval = config.sync_interval 
    }, Some config
    
  (** Create a remote database connection *)  
  let remote ?auth_token ?read_your_writes ?encryption_key ?sync_interval url =
    let config = make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () in
    Remote { 
      url; 
      auth_token = config.auth_token; 
      read_your_writes = config.read_your_writes; 
      encryption_key = config.encryption_key; 
      sync_interval = config.sync_interval 
    }, Some config
    
  (** Create an embedded replica connection *)
  let embedded_replica ?auth_token ?read_your_writes ?encryption_key ?sync_interval ~db_path ~primary_url () =
    let config = make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () in
    EmbeddedReplica { 
      db_path; 
      primary_url; 
      auth_token = config.auth_token; 
      read_your_writes = config.read_your_writes; 
      encryption_key = config.encryption_key; 
      sync_interval = config.sync_interval 
    }, Some config
    
  (** Create a synced database connection *)
  let synced_database ?auth_token ?read_your_writes ?encryption_key ?sync_interval ~db_path ~primary_url () =
    let config = make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () in
    SyncedDatabase { 
      db_path; 
      primary_url; 
      auth_token = config.auth_token; 
      read_your_writes = config.read_your_writes; 
      encryption_key = config.encryption_key; 
      sync_interval = config.sync_interval 
    }, Some config
    
  (** Open any connection type *)
  let open_connection (connection_type, config) =
    open_database ?config connection_type
end

(** Alternative: Direct functions (even more concise) *)

(** Open local database with optional configuration *)
let open_local_db ?auth_token ?read_your_writes ?encryption_key ?sync_interval path =
  let config = make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () in
  open_database ~config (Local { 
    path; 
    auth_token = config.auth_token; 
    read_your_writes = config.read_your_writes; 
    encryption_key = config.encryption_key; 
    sync_interval = config.sync_interval 
  })

(** Open remote database with optional configuration *)
let open_remote_db ?auth_token ?read_your_writes ?encryption_key ?sync_interval url =
  let config = make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () in
  open_database ~config (Remote { 
    url; 
    auth_token = config.auth_token; 
    read_your_writes = config.read_your_writes; 
    encryption_key = config.encryption_key; 
    sync_interval = config.sync_interval 
  })

(** Open embedded replica with optional configuration *)
let open_embedded_replica_db ?auth_token ?read_your_writes ?encryption_key ?sync_interval ~db_path ~primary_url () =
  let config = make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () in
  open_database ~config (EmbeddedReplica { 
    db_path; 
    primary_url; 
    auth_token = config.auth_token; 
    read_your_writes = config.read_your_writes; 
    encryption_key = config.encryption_key; 
    sync_interval = config.sync_interval 
  })

(** Open synced database with optional configuration *)
let open_synced_db ?auth_token ?read_your_writes ?encryption_key ?sync_interval ~db_path ~primary_url () =
  let config = make_config ?auth_token ?read_your_writes ?encryption_key ?sync_interval () in
  open_database ~config (SyncedDatabase { 
    db_path; 
    primary_url; 
    auth_token = config.auth_token; 
    read_your_writes = config.read_your_writes; 
    encryption_key = config.encryption_key; 
    sync_interval = config.sync_interval 
  })