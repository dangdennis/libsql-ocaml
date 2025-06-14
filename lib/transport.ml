(* open Eio.Std
open Hrana_types

module HTTP_Transport = struct
  type t = { base_uri : Uri.t; auth : auth option; config : config }

  let create ~config =
    let auth =
      match config.auth_token with
      | Some token -> Some (Bearer token)
      | None -> None
    in
    { base_uri = config.url; auth; config }

  let encode_request request =
    let json =
      match request with
      | Execute stmt ->
          `Assoc [ ("type", `String "execute"); ("stmt", encode_stmt stmt) ]
      | Batch batch ->
          `Assoc [ ("type", `String "batch"); ("batch", encode_batch batch) ]
      | _ -> failwith "Unsupported request type"
    in
    let payload = `Assoc [ ("requests", `List [ json ]) ] in
    Yojson.Safe.to_string payload

  and encode_stmt stmt =
    `Assoc
      [
        ("sql", `String stmt.sql);
        ( "args",
          match stmt.args with
          | Some args -> `List (List.map encode_value args)
          | None -> `Null );
        ( "named_args",
          match stmt.named_args with
          | Some named ->
              `List
                (List.map
                   (fun (k, v) ->
                     `Assoc [ ("name", `String k); ("value", encode_value v) ])
                   named)
          | None -> `Null );
        ( "want_rows",
          match stmt.want_rows with Some b -> `Bool b | None -> `Null );
      ]

  and encode_batch batch =
    `Assoc [ ("steps", `List (List.map encode_batch_step batch.steps)) ]

  and encode_batch_step step =
    `Assoc
      [
        ( "condition",
          match step.condition with
          | Some `Ok -> `String "ok"
          | Some `Error -> `String "error"
          | Some `Not -> `String "not"
          | None -> `Null );
        ("stmt", encode_stmt step.stmt);
      ]

  and encode_value = function
    | Null -> `Null
    | Integer i -> `Int (Int64.to_int i)
    | Real f -> `Float f
    | Text s -> `String s
    | Blob b -> `String (Base64.encode_string (Bytes.to_string b))

  let decode_response json =
    let open Yojson.Safe.Util in
    let results = json |> member "results" |> to_list in
    let decoded_results =
      List.map (fun result ->
          try
            let result_data = result |> member "result" in
            let cols =
              result_data |> member "cols" |> to_list |> List.map decode_col
            in
            let rows =
              result_data |> member "rows" |> to_list
              |> List.map (fun row -> row |> to_list |> List.map decode_value)
            in
            let affected_row_count =
              result_data
              |> member "affected_row_count"
              |> to_int |> Int64.of_int
            in
            let last_insert_rowid =
              result_data |> member "last_insert_rowid" |> to_string_option
            in
            Ok { cols; rows; affected_row_count; last_insert_rowid }
          with _ ->
            let error = result |> member "error" in
            let message = error |> member "message" |> to_string in
            let code = error |> member "code" |> to_string_option in
            Error { message; code })
    in
    decoded_results

  and decode_col json =
    let open Yojson.Safe.Util in
    {
      name = json |> member "name" |> to_string;
      decltype = json |> member "decltype" |> to_string_option;
    }

  and decode_value json =
    match json with
    | `Null -> Null
    | `Int i -> Integer (Int64.of_int i)
    | `Float f -> Real f
    | `String s -> Text s
    | _ -> failwith "Invalid value type"

  let send_request t ~net ~sw request =
    let endpoint = Uri.with_path t.base_uri "/v2/pipeline" in
    let body = encode_request request in

    let headers =
      [ ("Content-Type", "application/json"); ("Accept", "application/json") ]
      @
      match t.auth with
      | Some (Bearer token) -> [ ("Authorization", "Bearer " ^ token) ]
      | Some (Basic (user, pass)) ->
          [
            ( "Authorization",
              "Basic " ^ Base64.encode_string (user ^ ":" ^ pass) );
          ]
      | None -> []
    in

    (* Simple HTTP POST implementation using Eio *)
    let host = Uri.host_with_default endpoint in
    let port =
      match Uri.port endpoint with
      | Some p -> p
      | None -> if Uri.scheme endpoint = Some "https" then 443 else 80
    in
    let path = Uri.path_and_query endpoint in

    try
      let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
      let socket = Eio.Net.connect ~sw net addr in

      let request_line = Printf.sprintf "POST %s HTTP/1.1\r\n" path in
      let header_lines =
        List.fold_left
          (fun acc (k, v) -> acc ^ k ^ ": " ^ v ^ "\r\n")
          ""
          (("Host", host)
          :: ("Content-Length", string_of_int (String.length body))
          :: headers)
      in
      let http_request = request_line ^ header_lines ^ "\r\n" ^ body in

      Eio.Flow.copy_string http_request socket;

      (* Read response *)
      let response_buf = Buffer.create 4096 in
      let buf = Bytes.create 4096 in
      let buf_read = Eio.Buf_read.of_flow socket ~max_size:4096 in
      let rec read_until_complete () =
        try
          let line = Eio.Buf_read.line buf_read in
          if String.trim line = "" then
            let body = Eio.Buf_read.take_all buf_read in
            body
          else line ^ "\n" ^ read_until_complete ()
        with End_of_file -> ""
      in

      let response = read_until_complete () in
      let parts = String.split_on_char '\n' response in

      (* Parse HTTP response *)
      match parts with
      | status_line :: _ when String.contains status_line "200" ->
          let body_start =
            String.index response '\r' |> fun i ->
            String.index_from response (i + 4) '\r' |> fun j -> j + 2
          in
          let response_body =
            String.sub response body_start (String.length response - body_start)
          in
          let json = Yojson.Safe.from_string response_body in
          let results = decode_response json in
          Ok results
      | status_line :: _ ->
          Error (`Http_error (500, "HTTP error: " ^ status_line))
      | [] -> Error (`Network_error "Empty response")
    with exn -> Error (`Network_error (Printexc.to_string exn))
end

module WebSocket_Transport = struct
  open Websocket_eio

  type t = {
    base_uri : Uri.t;
    auth : auth option;
    config : config;
    mutable connection : WebSocket_Client.t option;
    request_counter : int32 ref;
    pending_requests : (int32, stream_response Promise.u) Hashtbl.t;
    mutex : Eio.Mutex.t;
  }

  let create ~config =
    let auth =
      match config.auth_token with
      | Some token -> Some (Bearer token)
      | None -> None
    in
    let ws_uri =
      let scheme = if config.tls then "wss" else "ws" in
      Uri.with_scheme config.url (Some scheme)
    in
    {
      base_uri = ws_uri;
      auth;
      config;
      connection = None;
      request_counter = ref 0l;
      pending_requests = Hashtbl.create 16;
      mutex = Eio.Mutex.create ();
    }

  let connect t ~net ~sw =
    Eio.Mutex.use_rw t.mutex (fun () ->
        match t.connection with
        | Some conn when WebSocket_Client.is_connected conn -> Ok conn
        | _ -> (
            let headers =
              [ ("Sec-WebSocket-Protocol", "hrana3") ]
              @
              match t.auth with
              | Some (Bearer token) -> [ ("Authorization", "Bearer " ^ token) ]
              | _ -> []
            in

            let ws_client =
              WebSocket_Client.create ~uri:t.base_uri ~headers ()
            in
            match WebSocket_Client.connect ws_client ~net ~sw with
            | Ok client -> (
                (* Send Hrana hello message *)
                let hello_msg =
                  match t.auth with
                  | Some (Bearer token) ->
                      `Assoc
                        [ ("type", `String "hello"); ("jwt", `String token) ]
                  | _ -> `Assoc [ ("type", `String "hello") ]
                in
                let hello_json = Yojson.Safe.to_string hello_msg in

                try
                  WebSocket_Client.send_text client hello_json;

                  (* Wait for hello response *)
                  let response_frame = WebSocket_Client.recv_frame client in
                  let response_json =
                    Yojson.Safe.from_string
                      (Websocket.Frame.content response_frame)
                  in
                  let open Yojson.Safe.Util in
                  match response_json |> member "type" |> to_string with
                  | "hello_ok" ->
                      t.connection <- Some client;
                      Ok client
                  | "hello_error" ->
                      let error_msg =
                        response_json |> member "error" |> member "message"
                        |> to_string
                      in
                      WebSocket_Client.close client ();
                      Error (`Auth_error error_msg)
                  | _ ->
                      WebSocket_Client.close client ();
                      Error (`Protocol_error "Invalid hello response")
                with exn ->
                  WebSocket_Client.close client ();
                  Error (`Network_error (Printexc.to_string exn)))
            | Error (WebSocket_Client.WebSocket_error msg) ->
                Error (`Network_error msg)
            | Error exn -> Error (`Network_error (Printexc.to_string exn))))

  let send_request t ~net ~sw request =
    match connect t ~net ~sw with
    | Error e -> Error e
    | Ok client -> (
        let request_id = !(t.request_counter) in
        incr t.request_counter;

        let request_json =
          match request with
          | Execute stmt ->
              `Assoc
                [
                  ("type", `String "request");
                  ("request_id", `Int (Int32.to_int request_id));
                  ( "request",
                    `Assoc
                      [
                        ("type", `String "execute");
                        ("stmt", HTTP_Transport.encode_stmt stmt);
                      ] );
                ]
          | Batch batch ->
              `Assoc
                [
                  ("type", `String "request");
                  ("request_id", `Int (Int32.to_int request_id));
                  ( "request",
                    `Assoc
                      [
                        ("type", `String "batch");
                        ("batch", HTTP_Transport.encode_batch batch);
                      ] );
                ]
          | _ -> failwith "Unsupported request type"
        in

        let json_string = Yojson.Safe.to_string request_json in

        try
          WebSocket_Client.send_text client json_string;

          (* Wait for response *)
          let response_frame = WebSocket_Client.recv_frame client in
          let response_json =
            Yojson.Safe.from_string (Websocket.Frame.content response_frame)
          in
          let open Yojson.Safe.Util in
          let response_type = response_json |> member "type" |> to_string in
          match response_type with
          | "response" ->
              let result = response_json |> member "result" in
              let decoded_result =
                HTTP_Transport.decode_response
                  (`Assoc [ ("results", `List [ result ]) ])
              in
              Ok decoded_result
          | "error" ->
              let error_msg =
                response_json |> member "error" |> member "message" |> to_string
              in
              let error_code =
                response_json |> member "error" |> member "code"
                |> to_string_option
              in
              Error (`Sql_error { message = error_msg; code = error_code })
          | _ ->
              Error
                (`Protocol_error ("Unexpected response type: " ^ response_type))
        with
        | WebSocket_Client.WebSocket_error msg -> Error (`Network_error msg)
        | exn -> Error (`Network_error (Printexc.to_string exn)))

  let close t =
    Eio.Mutex.use_rw t.mutex (fun () ->
        match t.connection with
        | Some client ->
            WebSocket_Client.close client ();
            t.connection <- None
        | None -> ())
end *)
