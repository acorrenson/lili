open Lili.Lili_typing
open Lili.Lili_term
open Lili.Lili_parser
open Lili.Lili_unification
open Sexplib

let () =
  let cr = "\x1B[31m" in
  let cg = "\x1B[32m" in
  let cy = "\x1B[33m" in
  let cn = "\x1B[0m" in
  let p, q = Sexp.input_sexp (open_in "./examples/test4.scm") |> parse_script in
  Printf.printf "Target : %s%s%s\n" cy (pretty_type p) cn;
  Printf.printf "Proof  :\n%s%s%s\n" cy (pretty_term q) cn;
  let t = infer q [] in
  match t with
  | Some tt ->
    begin
      match unify [(make_type_var tt), p] with
      | Some (u) ->
        Printf.printf "status : %sprooved%s using unifier : %s%s%s\n" 
          cg cn cy (pretty_unifier u) cn;
      | None ->
        Printf.printf "status : unification %sfailed%s\n" cr cn;
        Printf.printf "\tproof term was expected of type %s%s%s, got %s%s%s\n"
          cg (pretty_type p) cn cr (pretty_type tt) cn
    end
  | None -> 
    Printf.printf "status : proof term %sisn't typable%s\n" cr cn
