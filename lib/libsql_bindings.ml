(** LibSQL OCaml Bindings using ctypes - FFI with static linking *)

open Ctypes
open Foreign

(** {1 Type Definitions} *)

(** Cypher types from libsql.h *)
type libsql_cypher_t = 
  | LIBSQL_CYPHER_DEFAULT (* = 0 *)
  | LIBSQL_CYPHER_AES256  (* = 1 *)

let libsql_cypher_t = view int
  ~read:(function 
    | 0 -> LIBSQL_CYPHER_DEFAULT 
    | 1 -> LIBSQL_CYPHER_AES256
    | _ -> failwith "Invalid cypher type")
  ~write:(function 
    | LIBSQL_CYPHER_DEFAULT -> 0 
    | LIBSQL_CYPHER_AES256 -> 1)

(** Value types from libsql.h *)
type libsql_type_t =
  | LIBSQL_TYPE_INTEGER (* = 1 *)
  | LIBSQL_TYPE_REAL    (* = 2 *)
  | LIBSQL_TYPE_TEXT    (* = 3 *)
  | LIBSQL_TYPE_BLOB    (* = 4 *)
  | LIBSQL_TYPE_NULL    (* = 5 *)

let libsql_type_t = view int
  ~read:(function 
    | 1 -> LIBSQL_TYPE_INTEGER
    | 2 -> LIBSQL_TYPE_REAL
    | 3 -> LIBSQL_TYPE_TEXT
    | 4 -> LIBSQL_TYPE_BLOB
    | 5 -> LIBSQL_TYPE_NULL
    | _ -> failwith "Invalid type")
  ~write:(function 
    | LIBSQL_TYPE_INTEGER -> 1
    | LIBSQL_TYPE_REAL -> 2
    | LIBSQL_TYPE_TEXT -> 3
    | LIBSQL_TYPE_BLOB -> 4
    | LIBSQL_TYPE_NULL -> 5)

(** Tracing levels from libsql.h *)
type libsql_tracing_level_t =
  | LIBSQL_TRACING_LEVEL_ERROR (* = 1 *)
  | LIBSQL_TRACING_LEVEL_WARN  (* = 2 *)
  | LIBSQL_TRACING_LEVEL_INFO  (* = 3 *)
  | LIBSQL_TRACING_LEVEL_DEBUG (* = 4 *)
  | LIBSQL_TRACING_LEVEL_TRACE (* = 5 *)

let libsql_tracing_level_t = view int
  ~read:(function 
    | 1 -> LIBSQL_TRACING_LEVEL_ERROR
    | 2 -> LIBSQL_TRACING_LEVEL_WARN
    | 3 -> LIBSQL_TRACING_LEVEL_INFO
    | 4 -> LIBSQL_TRACING_LEVEL_DEBUG
    | 5 -> LIBSQL_TRACING_LEVEL_TRACE
    | _ -> failwith "Invalid tracing level")
  ~write:(function 
    | LIBSQL_TRACING_LEVEL_ERROR -> 1
    | LIBSQL_TRACING_LEVEL_WARN -> 2
    | LIBSQL_TRACING_LEVEL_INFO -> 3
    | LIBSQL_TRACING_LEVEL_DEBUG -> 4
    | LIBSQL_TRACING_LEVEL_TRACE -> 5)

(** Opaque error type *)
let libsql_error_t : [`libsql_error_t] structure typ = 
  structure "libsql_error_t"
let () = seal libsql_error_t

(** libsql_slice_t structure *)
let libsql_slice_t : [`libsql_slice_t] structure typ = 
  structure "libsql_slice_t"
let slice_ptr = field libsql_slice_t "ptr" (ptr void)
let slice_len = field libsql_slice_t "len" size_t
let () = seal libsql_slice_t

(** libsql_value_union_t union *)
let libsql_value_union_t : [`libsql_value_union_t] union typ = 
  union "libsql_value_union_t"
let union_integer = field libsql_value_union_t "integer" int64_t
let union_real = field libsql_value_union_t "real" double
let union_text = field libsql_value_union_t "text" libsql_slice_t
let union_blob = field libsql_value_union_t "blob" libsql_slice_t
let () = seal libsql_value_union_t

(** libsql_value_t structure *)
let libsql_value_t : [`libsql_value_t] structure typ = 
  structure "libsql_value_t"
let value_value = field libsql_value_t "value" libsql_value_union_t
let value_type = field libsql_value_t "type" libsql_type_t
let () = seal libsql_value_t

(** libsql_log_t structure *)
let libsql_log_t : [`libsql_log_t] structure typ = 
  structure "libsql_log_t"
let log_message = field libsql_log_t "message" (ptr char)
let log_target = field libsql_log_t "target" (ptr char)
let log_file = field libsql_log_t "file" (ptr char)
let log_timestamp = field libsql_log_t "timestamp" uint64_t
let log_line = field libsql_log_t "line" size_t
let log_level = field libsql_log_t "level" libsql_tracing_level_t
let () = seal libsql_log_t

(** libsql_database_desc_t structure *)
let libsql_database_desc_t : [`libsql_database_desc_t] structure typ = 
  structure "libsql_database_desc_t"
let desc_url = field libsql_database_desc_t "url" (ptr char)
let desc_path = field libsql_database_desc_t "path" (ptr char)
let desc_auth_token = field libsql_database_desc_t "auth_token" (ptr char)
let desc_encryption_key = field libsql_database_desc_t "encryption_key" (ptr char)
let desc_sync_interval = field libsql_database_desc_t "sync_interval" uint64_t
let desc_cypher = field libsql_database_desc_t "cypher" libsql_cypher_t
let desc_disable_read_your_writes = field libsql_database_desc_t "disable_read_your_writes" bool
let desc_webpki = field libsql_database_desc_t "webpki" bool
let desc_synced = field libsql_database_desc_t "synced" bool
let desc_disable_safety_assert = field libsql_database_desc_t "disable_safety_assert" bool
let () = seal libsql_database_desc_t

(** libsql_config_t structure *)
let libsql_config_t : [`libsql_config_t] structure typ = 
  structure "libsql_config_t"
let config_logger = field libsql_config_t "logger" (funptr (libsql_log_t @-> returning void))
let config_version = field libsql_config_t "version" (ptr char)
let () = seal libsql_config_t

(** Result types - structures with err field *)
let libsql_database_t : [`libsql_database_t] structure typ = 
  structure "libsql_database_t"
let database_err = field libsql_database_t "err" (ptr libsql_error_t)
let database_inner = field libsql_database_t "inner" (ptr void)
let () = seal libsql_database_t

let libsql_connection_t : [`libsql_connection_t] structure typ = 
  structure "libsql_connection_t"
let connection_err = field libsql_connection_t "err" (ptr libsql_error_t)
let connection_inner = field libsql_connection_t "inner" (ptr void)
let () = seal libsql_connection_t

let libsql_statement_t : [`libsql_statement_t] structure typ = 
  structure "libsql_statement_t"
let statement_err = field libsql_statement_t "err" (ptr libsql_error_t)
let statement_inner = field libsql_statement_t "inner" (ptr void)
let () = seal libsql_statement_t

let libsql_transaction_t : [`libsql_transaction_t] structure typ = 
  structure "libsql_transaction_t"
let transaction_err = field libsql_transaction_t "err" (ptr libsql_error_t)
let transaction_inner = field libsql_transaction_t "inner" (ptr void)
let () = seal libsql_transaction_t

let libsql_rows_t : [`libsql_rows_t] structure typ = 
  structure "libsql_rows_t"
let rows_err = field libsql_rows_t "err" (ptr libsql_error_t)
let rows_inner = field libsql_rows_t "inner" (ptr void)
let () = seal libsql_rows_t

let libsql_row_t : [`libsql_row_t] structure typ = 
  structure "libsql_row_t"
let row_err = field libsql_row_t "err" (ptr libsql_error_t)
let row_inner = field libsql_row_t "inner" (ptr void)
let () = seal libsql_row_t

let libsql_batch_t : [`libsql_batch_t] structure typ = 
  structure "libsql_batch_t"
let batch_err = field libsql_batch_t "err" (ptr libsql_error_t)
let () = seal libsql_batch_t

let libsql_result_value_t : [`libsql_result_value_t] structure typ = 
  structure "libsql_result_value_t"
let result_value_err = field libsql_result_value_t "err" (ptr libsql_error_t)
let result_value_ok = field libsql_result_value_t "ok" libsql_value_t
let () = seal libsql_result_value_t

let libsql_sync_t : [`libsql_sync_t] structure typ = 
  structure "libsql_sync_t"
let sync_err = field libsql_sync_t "err" (ptr libsql_error_t)
let sync_frame_no = field libsql_sync_t "frame_no" uint64_t
let sync_frames_synced = field libsql_sync_t "frames_synced" uint64_t
let () = seal libsql_sync_t

let libsql_bind_t : [`libsql_bind_t] structure typ = 
  structure "libsql_bind_t"
let bind_err = field libsql_bind_t "err" (ptr libsql_error_t)
let () = seal libsql_bind_t

let libsql_execute_t : [`libsql_execute_t] structure typ = 
  structure "libsql_execute_t"
let execute_err = field libsql_execute_t "err" (ptr libsql_error_t)
let execute_rows_changed = field libsql_execute_t "rows_changed" uint64_t
let () = seal libsql_execute_t

let libsql_connection_info_t : [`libsql_connection_info_t] structure typ = 
  structure "libsql_connection_info_t"
let connection_info_err = field libsql_connection_info_t "err" (ptr libsql_error_t)
let connection_info_last_inserted_rowid = field libsql_connection_info_t "last_inserted_rowid" int64_t
let connection_info_total_changes = field libsql_connection_info_t "total_changes" uint64_t
let () = seal libsql_connection_info_t

(** {1 Function Bindings} *)

(* Setup and error handling *)
let libsql_setup = 
  foreign "libsql_setup" (libsql_config_t @-> returning (ptr libsql_error_t))

let libsql_error_message = 
  foreign "libsql_error_message" (ptr libsql_error_t @-> returning (ptr char))

(* Database operations *)
let libsql_database_init = 
  foreign "libsql_database_init" (libsql_database_desc_t @-> returning libsql_database_t)

let libsql_database_sync = 
  foreign "libsql_database_sync" (libsql_database_t @-> returning libsql_sync_t)

let libsql_database_connect = 
  foreign "libsql_database_connect" (libsql_database_t @-> returning libsql_connection_t)

(* Connection operations *)
let libsql_connection_transaction = 
  foreign "libsql_connection_transaction" (libsql_connection_t @-> returning libsql_transaction_t)

let libsql_connection_batch = 
  foreign "libsql_connection_batch" (libsql_connection_t @-> string @-> returning libsql_batch_t)

let libsql_connection_info = 
  foreign "libsql_connection_info" (libsql_connection_t @-> returning libsql_connection_info_t)

let libsql_connection_prepare = 
  foreign "libsql_connection_prepare" (libsql_connection_t @-> string @-> returning libsql_statement_t)

(* Transaction operations *)
let libsql_transaction_batch = 
  foreign "libsql_transaction_batch" (libsql_transaction_t @-> string @-> returning libsql_batch_t)

let libsql_transaction_prepare = 
  foreign "libsql_transaction_prepare" (libsql_transaction_t @-> string @-> returning libsql_statement_t)

(* Statement operations *)
let libsql_statement_execute = 
  foreign "libsql_statement_execute" (libsql_statement_t @-> returning libsql_execute_t)

let libsql_statement_query = 
  foreign "libsql_statement_query" (libsql_statement_t @-> returning libsql_rows_t)

let libsql_statement_reset = 
  foreign "libsql_statement_reset" (libsql_statement_t @-> returning void)

let libsql_statement_column_count = 
  foreign "libsql_statement_column_count" (libsql_statement_t @-> returning size_t)

let libsql_statement_bind_named = 
  foreign "libsql_statement_bind_named" (libsql_statement_t @-> string @-> libsql_value_t @-> returning libsql_bind_t)

let libsql_statement_bind_value = 
  foreign "libsql_statement_bind_value" (libsql_statement_t @-> libsql_value_t @-> returning libsql_bind_t)

(* Rows operations *)
let libsql_rows_next = 
  foreign "libsql_rows_next" (libsql_rows_t @-> returning libsql_row_t)

let libsql_rows_column_name = 
  foreign "libsql_rows_column_name" (libsql_rows_t @-> int32_t @-> returning libsql_slice_t)

let libsql_rows_column_count = 
  foreign "libsql_rows_column_count" (libsql_rows_t @-> returning int32_t)

(* Row operations *)
let libsql_row_value = 
  foreign "libsql_row_value" (libsql_row_t @-> int32_t @-> returning libsql_result_value_t)

let libsql_row_name = 
  foreign "libsql_row_name" (libsql_row_t @-> int32_t @-> returning libsql_slice_t)

let libsql_row_length = 
  foreign "libsql_row_length" (libsql_row_t @-> returning int32_t)

let libsql_row_empty = 
  foreign "libsql_row_empty" (libsql_row_t @-> returning bool)

(* Value creation *)
let libsql_integer = 
  foreign "libsql_integer" (int64_t @-> returning libsql_value_t)

let libsql_real = 
  foreign "libsql_real" (double @-> returning libsql_value_t)

let libsql_text = 
  foreign "libsql_text" (string @-> size_t @-> returning libsql_value_t)

let libsql_blob = 
  foreign "libsql_blob" (ptr uint8_t @-> size_t @-> returning libsql_value_t)

let libsql_null = 
  foreign "libsql_null" (void @-> returning libsql_value_t)

(* Cleanup functions *)
let libsql_error_deinit = 
  foreign "libsql_error_deinit" (ptr libsql_error_t @-> returning void)

let libsql_database_deinit = 
  foreign "libsql_database_deinit" (libsql_database_t @-> returning void)

let libsql_connection_deinit = 
  foreign "libsql_connection_deinit" (libsql_connection_t @-> returning void)

let libsql_statement_deinit = 
  foreign "libsql_statement_deinit" (libsql_statement_t @-> returning void)

let libsql_transaction_commit = 
  foreign "libsql_transaction_commit" (libsql_transaction_t @-> returning void)

let libsql_transaction_rollback = 
  foreign "libsql_transaction_rollback" (libsql_transaction_t @-> returning void)

let libsql_rows_deinit = 
  foreign "libsql_rows_deinit" (libsql_rows_t @-> returning void)

let libsql_row_deinit = 
  foreign "libsql_row_deinit" (libsql_row_t @-> returning void)

let libsql_slice_deinit = 
  foreign "libsql_slice_deinit" (libsql_slice_t @-> returning void)