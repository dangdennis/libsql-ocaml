(** LibSQL OCaml Bindings using ctypes - FFI only *)

open Ctypes
open Foreign

(** {1 Type Definitions} *)

(** Opaque C types - these are never directly manipulated, only passed as pointers *)
let libsql_database : unit ptr typ = ptr void
let libsql_connection : unit ptr typ = ptr void
let libsql_stmt : unit ptr typ = ptr void
let libsql_rows : unit ptr typ = ptr void
let libsql_rows_future : unit ptr typ = ptr void
let libsql_row : unit ptr typ = ptr void

(** Pointer types as used in the C API *)
type libsql_database_t = unit ptr
type libsql_connection_t = unit ptr
type libsql_stmt_t = unit ptr
type libsql_rows_t = unit ptr
type libsql_rows_future_t = unit ptr
type libsql_row_t = unit ptr

(** C struct: replicated *)
let replicated : [`replicated] structure typ = structure "replicated"
let replicated_frame_no = field replicated "frame_no" int
let replicated_frames_synced = field replicated "frames_synced" int
let () = seal replicated

(** C struct: libsql_config *)
let libsql_config : [`libsql_config] structure typ = structure "libsql_config"
let libsql_config_db_path = field libsql_config "db_path" (ptr char)
let libsql_config_primary_url = field libsql_config "primary_url" (ptr char)
let libsql_config_auth_token = field libsql_config "auth_token" (ptr char)
let libsql_config_read_your_writes = field libsql_config "read_your_writes" char
let libsql_config_encryption_key = field libsql_config "encryption_key" (ptr char)
let libsql_config_sync_interval = field libsql_config "sync_interval" int
let libsql_config_with_webpki = field libsql_config "with_webpki" char
let libsql_config_offline = field libsql_config "offline" char
let () = seal libsql_config

(** C struct: blob *)
let blob : [`blob] structure typ = structure "blob"
let blob_ptr = field blob "ptr" (ptr char)
let blob_len = field blob "len" int
let () = seal blob

(** SQL data type constants *)
let libsql_int = 1
let libsql_float = 2
let libsql_text = 3
let libsql_blob = 4
let libsql_null = 5

(** {1 Function Bindings} *)

(** Core database operations *)
let libsql_enable_internal_tracing = 
  foreign "libsql_enable_internal_tracing" (void @-> returning int)

let libsql_sync = 
  foreign "libsql_sync" (libsql_database @-> ptr (ptr char) @-> returning int)

let libsql_sync2 = 
  foreign "libsql_sync2" (libsql_database @-> ptr replicated @-> ptr (ptr char) @-> returning int)

let libsql_open_sync = 
  foreign "libsql_open_sync" 
    (string @-> string @-> string @-> char @-> string @-> 
     ptr libsql_database @-> ptr (ptr char) @-> returning int)

let libsql_open_sync_with_webpki = 
  foreign "libsql_open_sync_with_webpki"
    (string @-> string @-> string @-> char @-> string @-> 
     ptr libsql_database @-> ptr (ptr char) @-> returning int)

let libsql_open_sync_with_config = 
  foreign "libsql_open_sync_with_config"
    (libsql_config @-> ptr libsql_database @-> ptr (ptr char) @-> returning int)

let libsql_open_ext = 
  foreign "libsql_open_ext"
    (string @-> ptr libsql_database @-> ptr (ptr char) @-> returning int)

let libsql_open_file = 
  foreign "libsql_open_file"
    (string @-> ptr libsql_database @-> ptr (ptr char) @-> returning int)

let libsql_open_remote = 
  foreign "libsql_open_remote"
    (string @-> string @-> ptr libsql_database @-> ptr (ptr char) @-> returning int)

let libsql_open_remote_with_webpki = 
  foreign "libsql_open_remote_with_webpki"
    (string @-> string @-> ptr libsql_database @-> ptr (ptr char) @-> returning int)

let libsql_close = 
  foreign "libsql_close" (libsql_database @-> returning void)

(** Connection operations *)
let libsql_connect = 
  foreign "libsql_connect"
    (libsql_database @-> ptr libsql_connection @-> ptr (ptr char) @-> returning int)

let libsql_load_extension = 
  foreign "libsql_load_extension"
    (libsql_connection @-> string @-> string @-> ptr (ptr char) @-> returning int)

let libsql_reset = 
  foreign "libsql_reset"
    (libsql_connection @-> ptr (ptr char) @-> returning int)

let libsql_disconnect = 
  foreign "libsql_disconnect" (libsql_connection @-> returning void)

(** Statement operations *)
let libsql_prepare = 
  foreign "libsql_prepare"
    (libsql_connection @-> string @-> ptr libsql_stmt @-> ptr (ptr char) @-> returning int)

let libsql_bind_int = 
  foreign "libsql_bind_int"
    (libsql_stmt @-> int @-> llong @-> ptr (ptr char) @-> returning int)

let libsql_bind_float = 
  foreign "libsql_bind_float"
    (libsql_stmt @-> int @-> double @-> ptr (ptr char) @-> returning int)

let libsql_bind_null = 
  foreign "libsql_bind_null"
    (libsql_stmt @-> int @-> ptr (ptr char) @-> returning int)

let libsql_bind_string = 
  foreign "libsql_bind_string"
    (libsql_stmt @-> int @-> string @-> ptr (ptr char) @-> returning int)

let libsql_bind_blob = 
  foreign "libsql_bind_blob"
    (libsql_stmt @-> int @-> ptr uchar @-> int @-> ptr (ptr char) @-> returning int)

let libsql_query_stmt = 
  foreign "libsql_query_stmt"
    (libsql_stmt @-> ptr libsql_rows @-> ptr (ptr char) @-> returning int)

let libsql_execute_stmt = 
  foreign "libsql_execute_stmt"
    (libsql_stmt @-> ptr (ptr char) @-> returning int)

let libsql_reset_stmt = 
  foreign "libsql_reset_stmt"
    (libsql_stmt @-> ptr (ptr char) @-> returning int)

let libsql_free_stmt = 
  foreign "libsql_free_stmt" (libsql_stmt @-> returning void)

(** Direct query operations *)
let libsql_query = 
  foreign "libsql_query"
    (libsql_connection @-> string @-> ptr libsql_rows @-> ptr (ptr char) @-> returning int)

let libsql_execute = 
  foreign "libsql_execute"
    (libsql_connection @-> string @-> ptr (ptr char) @-> returning int)

(** Result operations *)
let libsql_free_rows = 
  foreign "libsql_free_rows" (libsql_rows @-> returning void)

let libsql_free_rows_future = 
  foreign "libsql_free_rows_future" (libsql_rows_future @-> returning void)

let libsql_wait_result = 
  foreign "libsql_wait_result" (libsql_rows_future @-> returning void)

let libsql_column_count = 
  foreign "libsql_column_count" (libsql_rows @-> returning int)

let libsql_column_name = 
  foreign "libsql_column_name"
    (libsql_rows @-> int @-> ptr (ptr char) @-> ptr (ptr char) @-> returning int)

let libsql_column_type = 
  foreign "libsql_column_type"
    (libsql_rows @-> libsql_row @-> int @-> ptr int @-> ptr (ptr char) @-> returning int)

let libsql_changes = 
  foreign "libsql_changes" (libsql_connection @-> returning uint64_t)

let libsql_last_insert_rowid = 
  foreign "libsql_last_insert_rowid" (libsql_connection @-> returning int64_t)

(** Row operations *)
let libsql_next_row = 
  foreign "libsql_next_row"
    (libsql_rows @-> ptr libsql_row @-> ptr (ptr char) @-> returning int)

let libsql_free_row = 
  foreign "libsql_free_row" (libsql_row @-> returning void)

let libsql_get_string = 
  foreign "libsql_get_string"
    (libsql_row @-> int @-> ptr (ptr char) @-> ptr (ptr char) @-> returning int)

let libsql_free_string = 
  foreign "libsql_free_string" (ptr char @-> returning void)

let libsql_get_int = 
  foreign "libsql_get_int"
    (libsql_row @-> int @-> ptr llong @-> ptr (ptr char) @-> returning int)

let libsql_get_float = 
  foreign "libsql_get_float"
    (libsql_row @-> int @-> ptr double @-> ptr (ptr char) @-> returning int)

let libsql_get_blob = 
  foreign "libsql_get_blob"
    (libsql_row @-> int @-> ptr blob @-> ptr (ptr char) @-> returning int)

let libsql_free_blob = 
  foreign "libsql_free_blob" (blob @-> returning void)
