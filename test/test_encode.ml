(* Test suite for Hrana.Encode module *)
open Libsql.Hrana_encode

let test_write_string () =
  let buf = create_buffer () in
  write_string buf "hello";
  let result = Buffer.contents buf in
  Alcotest.(check string) "simple string" "\"hello\"" result

let test_write_string_with_escapes () =
  let buf = create_buffer () in
  write_string buf "hello \"world\"\n\t\\";
  let result = Buffer.contents buf in
  Alcotest.(check string)
    "escaped string" "\"hello \\\"world\\\"\\n\\t\\\\\"" result

let test_write_number () =
  let buf = create_buffer () in
  write_number buf 42.5;
  let result = Buffer.contents buf in
  Alcotest.(check string) "number" "42.5" result

let test_write_int () =
  let buf = create_buffer () in
  write_int buf 123;
  let result = Buffer.contents buf in
  Alcotest.(check string) "integer" "123" result

let test_write_bool_true () =
  let buf = create_buffer () in
  write_bool buf true;
  let result = Buffer.contents buf in
  Alcotest.(check string) "boolean true" "true" result

let test_write_bool_false () =
  let buf = create_buffer () in
  write_bool buf false;
  let result = Buffer.contents buf in
  Alcotest.(check string) "boolean false" "false" result

let test_write_field_string () =
  let buf = create_buffer () in
  let state = { is_first = true } in
  write_field_string buf state "name" "value";
  let result = Buffer.contents buf in
  Alcotest.(check string) "field string" "\"name\":\"value\"" result

let test_write_multiple_fields () =
  let buf = create_buffer () in
  let state = { is_first = true } in
  write_field_string buf state "first" "hello";
  write_field_int buf state "second" 42;
  write_field_bool buf state "third" true;
  let result = Buffer.contents buf in
  Alcotest.(check string)
    "multiple fields" "\"first\":\"hello\",\"second\":42,\"third\":true" result

let test_write_field_string_raw () =
  let buf = create_buffer () in
  let state = { is_first = true } in
  write_field_string_raw buf state "raw" "unescaped\"content";
  let result = Buffer.contents buf in
  Alcotest.(check string)
    "raw string field" "\"raw\":\"unescaped\"content\"" result

let test_write_field_object () =
  let encoder buf state value = write_field_string buf state "inner" value in
  let buf = create_buffer () in
  let state = { is_first = true } in
  write_field_object buf state "obj" "test" encoder;
  let result = Buffer.contents buf in
  Alcotest.(check string) "nested object" "\"obj\":{\"inner\":\"test\"}" result

let test_write_field_array_objects () =
  let encoder buf state value = write_field_int buf state "id" value in
  let buf = create_buffer () in
  let state = { is_first = true } in
  write_field_array_objects buf state "items" [ 1; 2; 3 ] encoder;
  let result = Buffer.contents buf in
  Alcotest.(check string)
    "array of objects" "\"items\":[{\"id\":1},{\"id\":2},{\"id\":3}]" result

let test_write_field_array_objects_empty () =
  let encoder buf state value = write_field_int buf state "id" value in
  let buf = create_buffer () in
  let state = { is_first = true } in
  write_field_array_objects buf state "items" [] encoder;
  let result = Buffer.contents buf in
  Alcotest.(check string) "empty array" "\"items\":[]" result

let test_write_json_object_simple () =
  let encoder buf state value = write_field_string buf state "message" value in
  let result = write_json_object encoder "hello world" in
  Alcotest.(check string) "simple object" "{\"message\":\"hello world\"}" result

let test_write_json_object_complex () =
  let encoder buf state (name, age, active) =
    write_field_string buf state "name" name;
    write_field_int buf state "age" age;
    write_field_bool buf state "active" active
  in
  let result = write_json_object encoder ("Alice", 30, true) in
  Alcotest.(check string)
    "complex object" "{\"name\":\"Alice\",\"age\":30,\"active\":true}" result

let test_write_json_object_nested () =
  let inner_encoder buf state value =
    write_field_string buf state "inner_field" value
  in
  let outer_encoder buf state (outer_val, inner_val) =
    write_field_string buf state "outer_field" outer_val;
    write_field_object buf state "nested" inner_val inner_encoder
  in
  let result = write_json_object outer_encoder ("outer", "inner") in
  Alcotest.(check string)
    "nested object"
    "{\"outer_field\":\"outer\",\"nested\":{\"inner_field\":\"inner\"}}" result

let test_write_json_object_with_array () =
  let item_encoder buf state item = write_field_string buf state "item" item in
  let encoder buf state (title, items) =
    write_field_string buf state "title" title;
    write_field_array_objects buf state "items" items item_encoder
  in
  let result = write_json_object encoder ("Test", [ "a"; "b"; "c" ]) in
  Alcotest.(check string)
    "object with array"
    "{\"title\":\"Test\",\"items\":[{\"item\":\"a\"},{\"item\":\"b\"},{\"item\":\"c\"}]}"
    result

let test_encode_simple_object () =
  let fields =
    [
      ("name", fun buf state name -> write_field_string buf state name "John");
      ("age", fun buf state name -> write_field_int buf state name 25);
    ]
  in
  let encoder = encode_simple_object fields in
  let result = write_json_object encoder () in
  Alcotest.(check string)
    "simple object helper" "{\"name\":\"John\",\"age\":25}" result

let test_empty_object () =
  let encoder _ _ () = () in
  let result = write_json_object encoder () in
  Alcotest.(check string) "empty object" "{}" result

let basic_tests =
  [
    ("write_string", `Quick, test_write_string);
    ("write_string_with_escapes", `Quick, test_write_string_with_escapes);
    ("write_number", `Quick, test_write_number);
    ("write_int", `Quick, test_write_int);
    ("write_bool_true", `Quick, test_write_bool_true);
    ("write_bool_false", `Quick, test_write_bool_false);
  ]

let field_tests =
  [
    ("write_field_string", `Quick, test_write_field_string);
    ("write_multiple_fields", `Quick, test_write_multiple_fields);
    ("write_field_string_raw", `Quick, test_write_field_string_raw);
    ("write_field_object", `Quick, test_write_field_object);
    ("write_field_array_objects", `Quick, test_write_field_array_objects);
    ( "write_field_array_objects_empty",
      `Quick,
      test_write_field_array_objects_empty );
  ]

let object_tests =
  [
    ("write\n_json_object_simple", `Quick, test_write_json_object_simple);
    ("write_json_object_complex", `Quick, test_write_json_object_complex);
    ("write_json_object_nested", `Quick, test_write_json_object_nested);
    ("write_json_object_with_array", `Quick, test_write_json_object_with_array);
    ("encode_simple_object", `Quick, test_encode_simple_object);
    ("empty_object", `Quick, test_empty_object);
  ]

let () =
  Alcotest.run "Hrana.Encode"
    [
      ("basic", basic_tests); ("fields", field_tests); ("objects", object_tests);
    ]
