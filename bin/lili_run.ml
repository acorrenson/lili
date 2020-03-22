open Language.Parser
open Language.Parsing
open Kernel.Term
open Kernel.Checker

let cr = "\x1B[31m"
let cg = "\x1B[32m"
let cy = "\x1B[33m"
let cn = "\x1B[0m"

let process_entity e =
  match e with
  | Axiom p -> Printf.printf "assuming : %s%s%s\n" cy (pretty_type p) cn
  | Target (target, proof) ->
    Printf.printf "New target : %s%s%s\n" cy (pretty_type target) cn;
    Printf.printf "Proof      :\n%s%s%s\n" cy (pretty_term proof) cn;
    match type_check proof new_env with
    | Some tt when tt = target ->
      Printf.printf "%sProoved%s !\n" cg cn;
    | Some tt ->
      Printf.printf "status : %sincompatible types%s. Proof term has type %s but was expected of type %s\n"
        cr cn (pretty_type tt) (pretty_type target);
    | None -> Printf.printf "status : proof term %sisn't typable%s\n" cr cn



let () =
  let pres = read_all (open_in "./examples/test.scm") |> do_parse parse_script in
  match pres with
  | None -> failwith "Parse error"
  | Some elist -> List.iter process_entity elist