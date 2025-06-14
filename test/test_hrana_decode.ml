(* Test suite for Hrana.Decode module *)
open Libsql.Hrana_decode

(* Helper function to create test JSON values *)
let make_string s = String s
let make_number n = Number n
let make_bool b = Bool b
let make_null () = Null
let make_array arr = Array arr
let make_object obj = Object obj

(* Basic type extraction tests *)
let test_string_success () =
  let value = make_string "hello" in
  let result = string value in
  Alcotest.(check string) "string extraction" "hello" result

let test_string_failure () =
  let value = make_number 42.0 in
  Alcotest.check_raises "string type error"
    (Decode_error "Expected string, received number") (fun () ->
      ignore (string value))

let test_string_opt_some () =
  let value = Some (make_string "hello") in
  let result = string_opt value in
  Alcotest.(check (option string)) "string_opt some" (Some "hello") result

let test_string_opt_none () =
  let value = None in
  let result = string_opt value in
  Alcotest.(check (option string)) "string_opt none" None result

let test_string_opt_null () =
  let value = Some (make_null ()) in
  let result = string_opt value in
  Alcotest.(check (option string)) "string_opt null" None result

let test_string_opt_failure () =
  let value = Some (make_number 42.0) in
  Alcotest.check_raises "string_opt type error"
    (Decode_error "Expected string or null, received number") (fun () ->
      ignore (string_opt value))

let test_number_success () =
  let value = make_number 42.5 in
  let result = number value in
  Alcotest.(check (float 0.001)) "number extraction" 42.5 result

let test_number_failure () =
  let value = make_string "hello" in
  Alcotest.check_raises "number type error"
    (Decode_error "Expected number, received string") (fun () ->
      ignore (number value))

let test_boolean_true () =
  let value = make_bool true in
  let result = boolean value in
  Alcotest.(check bool) "boolean true" true result

let test_boolean_false () =
  let value = make_bool false in
  let result = boolean value in
  Alcotest.(check bool) "boolean false" false result

let test_boolean_failure () =
  let value = make_string "true" in
  Alcotest.check_raises "boolean type error"
    (Decode_error "Expected boolean, received string") (fun () ->
      ignore (boolean value))

let test_array_success () =
  let arr = [ make_string "a"; make_string "b"; make_string "c" ] in
  let value = make_array arr in
  let result = array value in
  Alcotest.(check int) "array length" 3 (List.length result)

let test_array_failure () =
  let value = make_string "not an array" in
  Alcotest.check_raises "array type error"
    (Decode_error "Expected array, received string") (fun () ->
      ignore (array value))

let test_object_success () =
  let obj = [ ("name", make_string "John"); ("age", make_number 30.0) ] in
  let value = make_object obj in
  let result = object_map value in
  Alcotest.(check int) "object fields" 2 (List.length result)

let test_object_failure () =
  let value = make_string "not an object" in
  Alcotest.check_raises "object type error"
    (Decode_error "Expected object, received string") (fun () ->
      ignore (object_map value))

(* Field extraction tests *)
let test_get_field_existing () =
  let obj = [ ("name", make_string "John"); ("age", make_number 30.0) ] in
  let result = get_field obj "name" in
  match result with
  | Some (String s) -> Alcotest.(check string) "get_field existing" "John" s
  | _ -> Alcotest.fail "Expected Some (String \"John\")"

let test_get_field_missing () =
  let obj = [ ("name", make_string "John") ] in
  let result = get_field obj "age" in
  Alcotest.(check (option reject)) "get_field missing" None result

let test_get_field_exn_existing () =
  let obj = [ ("name", make_string "John") ] in
  let result = get_field_exn obj "name" in
  match result with
  | String s -> Alcotest.(check string) "get_field_exn existing" "John" s
  | _ -> Alcotest.fail "Expected String \"John\""

let test_get_field_exn_missing () =
  let obj = [ ("name", make_string "John") ] in
  Alcotest.check_raises "get_field_exn missing"
    (Decode_error "Expected age, but the property was missing") (fun () ->
      ignore (get_field_exn obj "age"))

let test_decode_string () =
  let obj = [ ("name", make_string "John") ] in
  let result = decode_string obj "name" in
  Alcotest.(check string) "decode_string" "John" result

let test_decode_string_opt_some () =
  let obj = [ ("name", make_string "John") ] in
  let result = decode_string_opt obj "name" in
  Alcotest.(check (option string)) "decode_string_opt some" (Some "John") result

let test_decode_string_opt_missing () =
  let obj = [ ("age", make_number 30.0) ] in
  let result = decode_string_opt obj "name" in
  Alcotest.(check (option string)) "decode_string_opt missing" None result

let test_decode_number () =
  let obj = [ ("age", make_number 30.0) ] in
  let result = decode_number obj "age" in
  Alcotest.(check (float 0.001)) "decode_number" 30.0 result

let test_decode_boolean () =
  let obj = [ ("active", make_bool true) ] in
  let result = decode_boolean obj "active" in
  Alcotest.(check bool) "decode_boolean" true result

let test_decode_array () =
  let arr = [ make_string "a"; make_string "b" ] in
  let obj = [ ("items", make_array arr) ] in
  let result = decode_array obj "items" in
  Alcotest.(check int) "decode_array length" 2 (List.length result)

let test_decode_object () =
  let nested = [ ("inner", make_string "value") ] in
  let obj = [ ("nested", make_object nested) ] in
  let result = decode_object obj "nested" in
  Alcotest.(check int) "decode_object fields" 1 (List.length result)

(* Array objects mapping tests *)
let test_array_objects_map () =
  let objects =
    [
      make_object [ ("id", make_number 1.0); ("name", make_string "first") ];
      make_object [ ("id", make_number 2.0); ("name", make_string "second") ];
    ]
  in
  let arr = make_array objects in
  let decode_fn obj =
    let id = int_of_float (decode_number obj "id") in
    let name = decode_string obj "name" in
    (id, name)
  in
  let result = array_objects_map arr decode_fn in
  Alcotest.(check int) "array_objects_map length" 2 (List.length result);
  match result with
  | [ (1, "first"); (2, "second") ] -> ()
  | _ -> Alcotest.fail "Unexpected array_\nobjects_map result"

let test_decode_array_objects () =
  let objects =
    [
      make_object [ ("id", make_number 1.0) ];
      make_object [ ("id", make_number 2.0) ];
    ]
  in
  let obj = [ ("items", make_array objects) ] in
  let decode_fn obj_item = int_of_float (decode_number obj_item "id") in
  let result = decode_array_objects obj "items" decode_fn in
  Alcotest.(check (list int)) "decode_array_objects" [ 1; 2 ] result

(* Optional field tests *)
let test_decode_string_opt_field_some () =
  let obj = [ ("name", make_string "John") ] in
  let result = decode_string_opt_field obj "name" in
  Alcotest.(check (option string))
    "decode_string_opt_field some" (Some "John") result

let test_decode_string_opt_field_none () =
  let obj = [ ("age", make_number 30.0) ] in
  let result = decode_string_opt_field obj "name" in
  Alcotest.(check (option string)) "decode_string_opt_field none" None result

let test_decode_number_opt_field_some () =
  let obj = [ ("age", make_number 30.0) ] in
  let result = decode_number_opt_field obj "age" in
  Alcotest.(check (option (float 0.001)))
    "decode_number_opt_field some" (Some 30.0) result

let test_decode_boolean_opt_field_some () =
  let obj = [ ("active", make_bool true) ] in
  let result = decode_boolean_opt_field obj "active" in
  Alcotest.(check (option bool))
    "decode_boolean_opt_field some" (Some true) result

(* Integration tests *)
let test_read_json_object () =
  let user_obj =
    make_object
      [
        ("name", make_string "Alice");
        ("age", make_number 30.0);
        ("active", make_bool true);
      ]
  in
  let decode_user obj =
    let name = decode_string obj "name" in
    let age = int_of_float (decode_number obj "age") in
    let active = decode_boolean obj "active" in
    (name, age, active)
  in
  let result = read_json_object user_obj decode_user in
  match result with
  | "Alice", 30, true -> ()
  | _ -> Alcotest.fail "Unexpected read_json_object result"

let test_complex_nested_object () =
  let user1 =
    make_object [ ("id", make_number 1.0); ("name", make_string "Alice") ]
  in
  let user2 =
    make_object [ ("id", make_number 2.0); ("name", make_string "Bob") ]
  in
  let response =
    make_object
      [
        ("users", make_array [ user1; user2 ]);
        ("total", make_number 2.0);
        ("success", make_bool true);
      ]
  in

  let decode_user obj =
    let id = int_of_float (decode_number obj "id") in
    let name = decode_string obj "name" in
    (id, name)
  in

  let decode_response obj =
    let users = decode_array_objects obj "users" decode_user in
    let total = int_of_float (decode_number obj "total") in
    let success = decode_boolean obj "success" in
    (users, total, success)
  in

  let result = read_json_object response decode_response in
  match result with
  | [ (1, "Alice"); (2, "Bob") ], 2, true -> ()
  | _ -> Alcotest.fail "Unexpected complex nested object result"

(* Error handling tests *)
let test_missing_required_field () =
  let obj = [ ("age", make_number 30.0) ] in
  Alcotest.check_raises "missing required field"
    (Decode_error "Expected name, but the property was missing") (fun () ->
      ignore (decode_string obj "name"))

let test_wrong_type_in_field () =
  let obj = [ ("name", make_number 30.0) ] in
  Alcotest.check_raises "wrong type in field"
    (Decode_error "Expected string, received number") (fun () ->
      ignore (decode_string obj "name"))

let test_empty_object () =
  let obj = [] in
  let result = decode_string_opt_field obj "name" in
  Alcotest.(check (option string)) "empty object" None result

let basic_tests =
  [
    ("string_success", `Quick, test_string_success);
    ("string_failure", `Quick, test_string_failure);
    ("string_opt_some", `Quick, test_string_opt_some);
    ("string_opt_none", `Quick, test_string_opt_none);
    ("string_opt_null", `Quick, test_string_opt_null);
    ("string_opt_failure", `Quick, test_string_opt_failure);
    ("number_success", `Quick, test_number_success);
    ("number_failure", `Quick, test_number_failure);
    ("boolean_true", `Quick, test_boolean_true);
    ("boolean_false", `Quick, test_boolean_false);
    ("boolean_failure", `Quick, test_boolean_failure);
    ("array_success", `Quick, test_array_success);
    ("array_failure", `Quick, test_array_failure);
    ("object_success", `Quick, test_object_success);
    ("object_failure", `Quick, test_object_failure);
  ]

let field_tests =
  [
    ("get_field_existing", `Quick, test_get_field_existing);
    ("get_field_missing", `Quick, test_get_field_missing);
    ("get_field_exn_existing", `Quick, test_get_field_exn_existing);
    ("get_field_exn_missing", `Quick, test_get_field_exn_missing);
    ("decode_string", `Quick, test_decode_string);
    ("decode_string_opt_some", `Quick, test_decode_string_opt_some);
    ("decode_string_opt_missing", `Quick, test_decode_string_opt_missing);
    ("decode_number", `Quick, test_decode_number);
    ("decode_boolean", `Quick, test_decode_boolean);
    ("decode_array", `Quick, test_decode_array);
    ("decode_object", `Quick, test_decode_object);
  ]

let array_tests =
  [
    ("array_objects_map", `Quick, test_array_objects_map);
    ("decode_array_objects", `Quick, test_decode_array_objects);
  ]

let optional_field_tests =
  [
    ("decode_string_opt_field_some", `Quick, test_decode_string_opt_field_some);
    ("decode_string_opt_field_none", `Quick, test_decode_string_opt_field_none);
    ("decode_number_opt_field_some", `Quick, test_decode_number_opt_field_some);
    ("decode_boolean_opt_field_some", `Quick, test_decode_boolean_opt_field_some);
  ]

let integration_tests =
  [
    ("read_json_object", `Quick, test_read_json_object);
    ("complex_nested_object", `Quick, test_complex_nested_object);
  ]

let error_tests =
  [
    ("missing_required_field", `Quick, test_missing_required_field);
    ("wrong_type_in_field", `Quick, test_wrong_type_in_field);
    ("empty_object", `Quick, test_empty_object);
  ]

let () =
  Alcotest.run "Hrana.Decode"
    [
      ("basic", basic_tests);
      ("fields", field_tests);
      ("arrays", array_tests);
      ("optional_fields", optional_field_tests);
      ("integration", integration_tests);
      ("errors", error_tests);
    ]
