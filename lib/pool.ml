(* open Eio.Std
open Hrana_types

type pool = {
  config: config;
  net: Eio.Net.t;
  connections: Client.client Queue.t;
  max_size: int;
  current_size: int ref;
  mutex: Eio.Mutex.t;
  condition: Eio.Condition.t;
}

let create ~config ~net ?(max_size=20) () =
  {
    config;
    net;
    connections = Queue.create ();
    max_size;
    current_size = ref 0;
    mutex = Eio.Mutex.create ();
    condition = Eio.Condition.create ();
  }

let get_connection pool ~sw =
  Eio.Mutex.use_rw pool.mutex (fun () ->
    if not (Queue.is_empty pool.connections) then
      Queue.take pool.connections
    else if !(pool.current_size) < pool.max_size then (
      incr pool.current_size;
      Client.create ~config:pool.config ~net:pool.net
    ) else (
      Eio.Condition.await_no_mutex pool.condition pool.mutex;
      get_connection pool ~sw
    )
  )

let return_connection pool client =
  Eio.Mutex.use_rw pool.mutex (fun () ->
    Queue.add client pool.connections;
    Eio.Condition.broadcast pool.condition
  )

let with_connection pool ~sw f =
  let client = get_connection pool ~sw in
  Fun.protect ~finally:(fun () -> return_connection pool client) (fun () ->
    f client
  )

let close_pool pool =
  Eio.Mutex.use_rw pool.mutex (fun () ->
    Queue.iter Client.close pool.connections;
    Queue.clear pool.connections;
    pool.current_size := 0
  ) *)
