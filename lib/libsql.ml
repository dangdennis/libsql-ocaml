(** LibSQL OCaml API - Simple bindings for libSQL database *)

open Ctypes
module Hrana_encode = Hrana_encode

(** {1 Types} *)

type database = [ `libsql_database_t ] structure
(** Abstract database handle *)

type connection = [ `libsql_connection_t ] structure
(** Abstract connection handle *)

type statement = [ `libsql_statement_t ] structure
(** Abstract statement handle *)

(** {1 Exceptions} *)

exception Libsql_error of string

(** {1 Helper Functions} *)

(** Check for errors in a result structure *)
let check_error err_ptr error_context =
  if not (is_null err_ptr) then
    let error_msg = Libsql_bindings.libsql_error_message err_ptr in
    let msg =
      if not (is_null error_msg) then coerce (ptr char) string error_msg
      else "Unknown error in " ^ error_context
    in
    raise (Libsql_error msg)

(** {1 Database Operations} *)

(** Open a local database file *)
let open_local path =
  let desc = make Libsql_bindings.libsql_database_desc_t in

  (* Set path *)
  let path_ptr = CArray.of_string path |> CArray.start in
  setf desc Libsql_bindings.desc_path path_ptr;

  (* Set other fields to NULL/default *)
  setf desc Libsql_bindings.desc_url (from_voidp char null);
  setf desc Libsql_bindings.desc_auth_token (from_voidp char null);
  setf desc Libsql_bindings.desc_encryption_key (from_voidp char null);
  setf desc Libsql_bindings.desc_sync_interval Unsigned.UInt64.zero;
  setf desc Libsql_bindings.desc_cypher Libsql_bindings.LIBSQL_CYPHER_DEFAULT;
  setf desc Libsql_bindings.desc_disable_read_your_writes false;
  setf desc Libsql_bindings.desc_webpki false;
  setf desc Libsql_bindings.desc_synced false;
  setf desc Libsql_bindings.desc_disable_safety_assert false;

  (* Initialize database *)
  let db = Libsql_bindings.libsql_database_init desc in

  (* Check for errors *)
  let err_ptr = getf db Libsql_bindings.database_err in
  check_error err_ptr "open_local";

  db

(** Connect to a database *)
let connect db =
  let conn = Libsql_bindings.libsql_database_connect db in
  let err_ptr = getf conn Libsql_bindings.connection_err in
  check_error err_ptr "connect";
  conn

(** Close a database *)
let close_database db = Libsql_bindings.libsql_database_deinit db

(** Close a connection *)
let close_connection conn = Libsql_bindings.libsql_connection_deinit conn

(** {1 Statement Operations} *)

(** Prepare a SQL statement *)
let prepare conn sql =
  let stmt = Libsql_bindings.libsql_connection_prepare conn sql in
  let err_ptr = getf stmt Libsql_bindings.statement_err in
  check_error err_ptr "prepare";
  stmt

(** Execute a statement *)
let execute stmt =
  let result = Libsql_bindings.libsql_statement_execute stmt in
  let err_ptr = getf result Libsql_bindings.execute_err in
  check_error err_ptr "execute";
  let rows_changed = getf result Libsql_bindings.execute_rows_changed in
  Unsigned.UInt64.to_int64 rows_changed

(** Close a statement *)
let close_statement stmt = Libsql_bindings.libsql_statement_deinit stmt

(** {1 Convenience Functions} *)

(** Execute a simple SQL statement *)
let exec conn sql =
  let stmt = prepare conn sql in
  let _ = execute stmt in
  close_statement stmt
