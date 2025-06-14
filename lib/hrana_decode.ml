type json_value =
  | String of string
  | Number of float
  | Bool of bool
  | Null
  | Array of json_value list
  | Object of (string * json_value) list

exception Decode_error of string

let type_error value expected =
  let received =
    match value with
    | String _ -> "string"
    | Number _ -> "number"
    | Bool _ -> "boolean"
    | Null -> "null"
    | Array _ -> "array"
    | Object _ -> "object"
  in
  Decode_error (Printf.sprintf "Expected %s, received %s" expected received)

let missing_property_error expected =
  Decode_error
    (Printf.sprintf "Expected %s, but the property was missing" expected)

let string = function
  | String s -> s
  | value -> raise (type_error value "string")

let string_opt = function
  | None -> None
  | Some Null -> None
  | Some (String s) -> Some s
  | Some value -> raise (type_error value "string or null")

let number = function
  | Number n -> n
  | value -> raise (type_error value "number")

let boolean = function
  | Bool b -> b
  | value -> raise (type_error value "boolean")

let array = function
  | Array arr -> arr
  | value -> raise (type_error value "array")

let object_map = function
  | Object obj -> obj
  | value -> raise (type_error value "object")

let get_field obj key = try Some (List.assoc key obj) with Not_found -> None

let get_field_exn obj key =
  try List.assoc key obj with Not_found -> raise (missing_property_error key)

let array_objects_map values decode_fn =
  let arr = array values in
  List.map
    (fun value ->
      let obj = object_map value in
      decode_fn obj)
    arr

let decode_string obj key = string (get_field_exn obj key)
let decode_string_opt obj key = string_opt (get_field obj key)
let decode_number obj key = number (get_field_exn obj key)
let decode_boolean obj key = boolean (get_field_exn obj key)
let decode_array obj key = array (get_field_exn obj key)
let decode_object obj key = object_map (get_field_exn obj key)

let decode_array_objects obj key decode_fn =
  let arr_value = get_field_exn obj key in
  array_objects_map arr_value decode_fn

let read_json_object value decode_fn =
  let obj = object_map value in
  decode_fn obj

(* Convenience functions for optional fields *)
let decode_string_opt_field obj key =
  match get_field obj key with
  | None -> None
  | Some value -> Some (string value)

let decode_number_opt_field obj key =
  match get_field obj key with
  | None -> None
  | Some value -> Some (number value)

let decode_boolean_opt_field obj key =
  match get_field obj key with
  | None -> None
  | Some value -> Some (boolean value)

let decode_array_opt_field obj key =
  match get_field obj key with None -> None | Some value -> Some (array value)

let decode_object_opt_field obj key =
  match get_field obj key with
  | None -> None
  | Some value -> Some (object_map value)

type 'a decoder = (string * json_value) list -> 'a

let decode_field decoder obj key =
  let value = get_field_exn obj key in
  match value with
  | Object nested_obj -> decoder nested_obj
  | _ -> raise (type_error value "object")

let decode_field_opt decoder obj key =
  match get_field obj key with
  | None -> None
  | Some Null -> None
  | Some (Object nested_obj) -> Some (decoder nested_obj)
  | Some value -> raise (type_error value "object or null")

(* Example usage:

   type user = { name: string; age: int; active: bool option }

   let decode_user obj =
     {
       name = decode_string obj "name";
       age = int_of_float (decode_number obj "age");
       active = decode_boolean_opt_field obj "active";
     }

   let decode_response obj =
     {
       users = decode_array_objects obj "users" decode_user;
       total = int_of_float (decode_number obj "total");
     }
*)
