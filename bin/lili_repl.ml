open Kernel.Term
open Kernel.Checker
open Language.Parser
open Sexplib

let () =
  print_string "$> ";
  flush_all ();
  let cy = "\x1B[33m" in
  let cn = "\x1B[0m" in
  let term = read_line () |> Sexp.of_string |> parse_term in
  match type_check term new_env with
  | Some t ->
    Printf.printf "term = %s : %s(%s)%s\n" (pretty_term_inline term) cy (pretty_type t) cn
  | None -> failwith "type error !"
