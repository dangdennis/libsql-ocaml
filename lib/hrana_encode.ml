type 'a encoder = Buffer.t -> 'a -> unit

let create_buffer () = Buffer.create 256

let write_string buf s =
  Buffer.add_char buf '"';
  (* Simple JSON string escaping *)
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let write_number buf n = Buffer.add_string buf (string_of_float n)
let write_int buf n = Buffer.add_string buf (string_of_int n)
let write_bool buf b = Buffer.add_string buf (if b then "true" else "false")

type writer_state = { mutable is_first : bool }

let write_key buf state name =
  if state.is_first then state.is_first <- false else Buffer.add_char buf ',';
  write_string buf name;
  Buffer.add_char buf ':'

let write_field_string buf state name value =
  write_key buf state name;
  write_string buf value

let write_field_string_raw buf state name value =
  write_key buf state name;
  Buffer.add_char buf '"';
  Buffer.add_string buf value;
  Buffer.add_char buf '"'

let write_field_number buf state name value =
  write_key buf state name;
  write_number buf value

let write_field_int buf state name value =
  write_key buf state name;
  write_int buf value

let write_field_bool buf state name value =
  write_key buf state name;
  write_bool buf value

let write_field_object buf state name value encoder =
  write_key buf state name;
  Buffer.add_char buf '{';
  let nested_state = { is_first = true } in
  encoder buf nested_state value;
  Buffer.add_char buf '}'

let write_field_array_objects buf state name values encoder =
  write_key buf state name;
  Buffer.add_char buf '[';
  List.iteri
    (fun i value ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_char buf '{';
      let nested_state = { is_first = true } in
      encoder buf nested_state value;
      Buffer.add_char buf '}')
    values;
  Buffer.add_char buf ']'

let write_json_object encoder value =
  let buf = create_buffer () in
  Buffer.add_char buf '{';
  let state = { is_first = true } in
  encoder buf state value;
  Buffer.add_char buf '}';
  Buffer.contents buf

(* Convenience functions for common patterns *)
let encode_simple_object fields =
 fun buf state () ->
  List.iter (fun (name, write_fn) -> write_fn buf state name) fields

let encode_record encoder_fn =
 fun buf state record -> encoder_fn buf state record
