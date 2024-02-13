
open Ctypes
open Foreign

type libsql_database_t

let run () =
  let () = print_endline "attempt connect to libsql" in

  let libsql_database_t : libsql_database_t typ =
    abstract ~name:"libsql_database_t" ~size:0 ~alignment:0
  in

  (* let libsql_database_t : [ `libsql_database_t ] structure typ =
       structure "libsql_database_t"
     in *)

  (* Replace void with actual type if necessary *)
  (* let () = seal libsql_database_t in *)
  let _create_db =
    foreign "libsql_create_db" (string @-> returning (ptr libsql_database_t))
  in

  let connect_db =
    foreign "libsql_connect_db" (string @-> returning (ptr libsql_database_t))
  in

  let libsql_sync =
    foreign "libsql_sync"
      (ptr libsql_database_t @-> ptr string @-> returning int)
  in

  let _db_ptr = allocate_n libsql_database_t ~count:1 in
  let db_file_path = "./my_database.db" in
  let db_ptr = connect_db db_file_path in

  (* Allocate a pointer for the error message *)
  let errMsg = allocate string "" in

  (* Call the function *)
  let statusCode = libsql_sync db_ptr errMsg in

  (* Dereference errMsg to get the actual error message, if necessary *)
  let errorMessage = !@errMsg in
  let () = print_endline (errorMessage ^ " " ^ string_of_int statusCode) in

  ()
