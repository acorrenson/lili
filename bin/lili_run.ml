open Language.Parser
open Kernel.Term
open Kernel.Checker
open Sexplib

let () =
  let cr = "\x1B[31m" in
  let cg = "\x1B[32m" in
  let cy = "\x1B[33m" in
  let cn = "\x1B[0m" in
  let target, proof = Sexp.input_sexp (open_in "./examples/test2.scm") |> parse_script in
  Printf.printf "Target : %s%s%s\n" cy (pretty_type target) cn;
  Printf.printf "Proof  :\n%s%s%s\n" cy (pretty_term proof) cn;
  let t = type_check proof new_env in
  match t with
  | Some tt when tt = target ->
    Printf.printf "%sProoved%s !\n" cg cn;
  | Some tt ->
    Printf.printf "status : %sincompatible types%s. Proof term has type %s but was expected of type %s\n"
      cr cn (pretty_type tt) (pretty_type target);
  | None -> Printf.printf "status : proof term %sisn't typable%s\n" cr cn
