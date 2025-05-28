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

(** Database connection types *)
type connection_type =
  | Local of string  (* file path *)
  | Remote of string * string option  (* url * auth_token *)
  | EmbeddedReplica of string * string * config  (* db_path * primary_url * config *)
  | SyncedDatabase of string * string * config   (* db_path * primary_url * config *)

(** Abstract database handle - for now using unit, will be replaced with C bindings *)
type database = unit

(** Abstract connection handle *)
type connection = unit

(** Abstract statement handle *)
type statement = unit

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

let with_auth_token token config = 
  { config with auth_token = Some token }

let with_read_your_writes ryw config = 
  { config with read_your_writes = Some ryw }

let with_encryption key config = 
  { config with encryption_key = Some key }

let with_sync_interval interval config = 
  { config with sync_interval = Some interval }

(** {1 Database Operations} *)

(** Open a database connection *)
let open_database _connection_type = 
  (* TODO: Implement with C bindings *)
  failwith "Not implemented yet"

(** Close a database *)
let close_database _db = 
  (* TODO: Implement with C bindings *)
  ()

(** Sync a replicated database *)
let sync _db = 
  (* TODO: Implement with C bindings *)
  { frame_no = 0; frames_synced = 0 }

(** Connect to a database *)
let connect _db = 
  (* TODO: Implement with C bindings *)
  ()

(** Close a connection *)
let close_connection _conn = 
  (* TODO: Implement with C bindings *)
  ()

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

(** {1 Builder Pattern for Connection Types} *)

module ConnectionBuilder = struct
  type t = {
    connection_type : connection_type;
    config : config;
  }
  
  let local path = 
    { connection_type = Local path; config = default_config }
    
  let remote url = 
    { connection_type = Remote (url, None); config = default_config }
    
  let embedded_replica ~db_path ~primary_url = 
    { connection_type = EmbeddedReplica (db_path, primary_url, default_config); 
      config = default_config }
      
  let synced_database ~db_path ~primary_url = 
    { connection_type = SyncedDatabase (db_path, primary_url, default_config); 
      config = default_config }
  
  let with_auth_token token t = 
    let new_config = with_auth_token token t.config in
    { t with config = new_config }
    
  let with_read_your_writes ryw t = 
    let new_config = with_read_your_writes ryw t.config in
    { t with config = new_config }
    
  let with_encryption key t = 
    let new_config = with_encryption key t.config in
    { t with config = new_config }
    
  let with_sync_interval interval t = 
    let new_config = with_sync_interval interval t.config in
    { t with config = new_config }
    
  let build t = t.connection_type
end