(* Value types for SQL parameters and results *)
type value = 
  | Null
  | Integer of int64
  | Real of float
  | Text of string
  | Blob of bytes

(* SQL Statement *)
type stmt = {
  sql: string;
  args: value list option;
  named_args: (string * value) list option;
  want_rows: bool option;
}

(* Column metadata *)
type col = {
  name: string;
  decltype: string option;
}

(* Statement result *)
type stmt_result = {
  cols: col list;
  rows: value list list;
  affected_row_count: int64;
  last_insert_rowid: string option;
}

(* Batch operations *)
type batch_step = {
  condition: [`Ok | `Error | `Not] option;
  stmt: stmt;
}

type batch = {
  steps: batch_step list;
}

(* Stream request types - supports both HTTP and WebSocket *)
type stream_request = 
  | Execute of stmt
  | Batch of batch
  | Sequence of string option
  | Describe of string

(* Stream response types *)
type stream_response =
  | StepResult of stmt_result
  | StepError of hrana_error
  | BatchResult of { results: (stmt_result, hrana_error) result list }
  | SequenceResult of stmt_result list
  | DescribeResult of { params: col list; cols: col list }

and hrana_error = {
  message: string;
  code: string option;
}

(* Authentication *)
type auth = 
  | Bearer of string
  | Basic of string * string

(* Connection configuration - supports both HTTP and WebSocket *)
type config = {
  url: Uri.t;
  auth_token: string option;
  timeout: float;
  max_retries: int;
  transport: [`Http | `WebSocket | `Auto];
  tls: bool;
}