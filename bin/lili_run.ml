open Language.Parser
open Language.Parsing
open Kernel.Term
open Kernel.Checker

let cr = "\x1B[31m"
let cg = "\x1B[32m"
let cy = "\x1B[33m"
let cn = "\x1B[0m"

let process_entity env e =
  Printf.printf "%s\n" (String.make 60 '-');
  match e with
  | Axiom (name, ax) ->
    begin
      match extend_by_axiom name ax env with
      | Ok env' -> 
        Printf.printf "assuming %s%s%s : %s%s%s\n" cg name cn cy (pretty_type ax) cn;
        env'
      | Error err ->
        match err with
        | Overwrite ->
          Printf.printf "error : Overwriting axiom %s%s%s !\n" cr name cn;
          exit 3
        | _ -> assert false
    end
  | Target ((name, target), proof) ->
    Printf.printf "New target %s%s%s : %s%s%s\n" cg name cn cy (pretty_type target) cn;
    Printf.printf "Proof :\n%s%s%s\n" cy (pretty_term proof) cn;
    match extend_env name target proof env with
    | Ok env' ->
      Printf.printf "%sProoved%s !\n" cg cn;
      env'
    | Error err ->
      match err with
      | Bad_type t ->
        Printf.printf "error : %sincompatible types%s. Proof term has type %s but was expected of type %s\n"
          cr cn (pretty_type t) (pretty_type target);
        exit 1
      | Not_typable ->
        Printf.printf "error : proof term %sisn't typable%s\n" cr cn;
        exit 2
      | Unification ->
        Printf.printf "error : proof %sdo not match the target%s\n" cr cn;
        exit 3
      | Overwrite ->
        Printf.printf "error : Overwriting statement %s%s%s\n" cr name cn;
        exit 4

let () =
  let pres = read_all (open_in Sys.argv.(1)) |> do_parse parse_script in
  match pres with
  | None -> failwith "Parse error"
  | Some elist -> ignore (List.fold_left process_entity new_env elist)