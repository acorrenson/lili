open Lili.Lili_typing
open Lili.Lili_term
open Lili.Lili_parser
open Sexplib

let () =
  let cr = "\x1B[31m" in
  let cg = "\x1B[32m" in
  let cy = "\x1B[33m" in
  let cn = "\x1B[0m" in
  let p, q = Sexp.input_sexp (open_in "./examples/test.scm") |> parse_script in
  Printf.printf "Target : %s%s%s\n" cy (pretty_type p) cn;
  Printf.printf "Proof  :\n%s%s%s\n" cy (pretty_term q) cn;
  let t = infer q [] in
  match t with
  | Some tt when tt = p -> Printf.printf "status : %sprooved%s\n" cg cn
  | Some tt ->
    Printf.printf "status : proof term was expected of type %s%s%s, got %s%s%s\n"
      cg (pretty_type p) cn cr (pretty_type tt) cn
  | None -> 
    Printf.printf "status : proof term %sisn't typable%s\n" cr cn
