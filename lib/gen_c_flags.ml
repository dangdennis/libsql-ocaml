#!/usr/bin/env ocaml
#load "unix.cma";;

(* Generate platform-specific C linking flags for libsql *)

let get_os_arch () =
  let ic = Unix.open_process_in "uname -s" in
  let os = input_line ic in
  let _ = Unix.close_process_in ic in
  let ic = Unix.open_process_in "uname -m" in
  let arch = input_line ic in
  let _ = Unix.close_process_in ic in
  (String.lowercase_ascii os, arch)

let normalize_arch = function
  | "x86_64" | "amd64" -> "amd64"
  | "arm64" | "aarch64" -> "arm64"
  | arch -> arch

let normalize_os = function
  | "darwin" -> "darwin"
  | "linux" -> "linux"
  | os -> os

let get_lib_dir () =
  let (os, arch) = get_os_arch () in
  let norm_os = normalize_os os in
  let norm_arch = normalize_arch arch in
  Printf.sprintf "%s_%s" norm_os norm_arch

let () =
  try
    let lib_dir = get_lib_dir () in
    let lib_path = Printf.sprintf "%s/%s" (Sys.getcwd ()) lib_dir in
    
    (* Check if the directory exists *)
    if Sys.file_exists lib_path && Sys.is_directory lib_path then
      let flags = [
        Printf.sprintf "-L%s" lib_path;
        "-lsql_experimental";
        "-lsqlite3";
      ] in
      let additional_flags = 
        let (os, _) = get_os_arch () in
        if normalize_os os = "darwin" then
          ["-framework"; "Security"; "-framework"; "CoreFoundation"]
        else
          ["-lm"; "-ldl"; "-lpthread"]
      in
      let all_flags = flags @ additional_flags in
      Printf.printf "(%s)\n" (String.concat " " (List.map (Printf.sprintf "\"%s\"") all_flags))
    else
      Printf.eprintf "Warning: Library directory %s not found\n" lib_path;
      Printf.printf "()\n"
  with
  | exn ->
    Printf.eprintf "Error generating flags: %s\n" (Printexc.to_string exn);
    Printf.printf "()\n"
