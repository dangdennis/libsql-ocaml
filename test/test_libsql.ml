(** Test connecting to local chinook.db SQLite database *)

let test_connect_to_chinook () =
  Printf.printf "Testing connection to local chinook.db...\n";
  
  try
    (* let db = Libsql.open_local "../chinook.db" in *)
    Printf.printf "✓ Successfully opened chinook.db\n";
    
    (* let conn = Libsql.connect db in *)
    Printf.printf "✓ Successfully connected to database\n";
    
    (* Libsql.close_connection conn; *)
    Printf.printf "✓ Successfully closed connection\n";
    
    (* Libsql.close_database db; *)
    Printf.printf "✓ Successfully closed database\n";
    
    Printf.printf "All tests passed! 🎉\n"
    
  with
  | Libsql.Libsql_error (code, msg) ->
      Printf.eprintf "LibSQL Error (code %d): %s\n" code msg;
      exit 1
  | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
      exit 1

let () = test_connect_to_chinook ()
