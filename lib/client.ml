(* open Eio.Std
open Hrana_types

type client = {
  config: config;
  connection: Connection.connection;
  net: Eio.Net.t;
}

let create ~config ~net =
  let connection = Connection.create_connection ~config in
  { config; connection; net }

let execute client ~sw stmt =
  Connection.execute_statement client.connection ~net:client.net ~sw stmt

let execute_batch client ~sw batch =
  Connection.execute_batch client.connection ~net:client.net ~sw batch

let close client =
  Connection.close_connection client.connection *)
