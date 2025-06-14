(* open Eio.Std
open Hrana_types

type connection = {
  config: config;
  transport: Transport.HTTP_Transport.t;
  mutex: Eio.Mutex.t;
}

let create_connection ~config =
  let transport = Transport.HTTP_Transport.create ~config in
  {
    config;
    transport;
    mutex = Eio.Mutex.create ();
  }

let execute_statement conn ~net ~sw stmt =
  Eio.Mutex.use_rw conn.mutex (fun () ->
    match Transport.HTTP_Transport.send_request conn.transport ~net ~sw (Execute stmt) with
    | Ok [Ok result] -> Ok result
    | Ok [Error error] -> Error (`Sql_error error)
    | Ok [] -> Error (`Protocol_error "No result returned")
    | Ok _ -> Error (`Protocol_error "Multiple results for single statement")
    | Error e -> Error e
  )

let execute_batch conn ~net ~sw batch =
  Eio.Mutex.use_rw conn.mutex (fun () ->
    match Transport.HTTP_Transport.send_request conn.transport ~net ~sw (Batch batch) with
    | Ok results -> Ok results
    | Error e -> Error e
  )

let close_connection _conn =
  (* HTTP is stateless, no connection to close *)
  () *)
