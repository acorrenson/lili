open Lili.Lili_term
open Lili.Lili_parser
open Lili.Lili_typing
open Sexplib

let () =
  print_string "$> ";
  flush_all ();
  let term = 
    read_line ()
    |> Sexp.of_string
    |> parse_term
  in
  match infer term [] with
  | Some t ->
    Printf.printf "term = %s : (%s)\n" (pretty_term term) (pretty_type t)
  | None -> failwith "type error !"
