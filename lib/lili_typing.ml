open Lili_term
open Option
open Lili_unification

type type_env = (string * type_info) list

let (let*) = Option.bind

let rec infer t env =
  match t with
  | Lambda (Bind (x, tx), y) ->
    let env' = List.remove_assoc x env in
    let* ty = infer y ((x,tx)::env') in
    Some (Type_arrow (tx, ty))
  | Var x ->
    List.assoc_opt x env
  | Application (t1, t2) ->
    let* tt1 = infer t1 env in
    let* tt2 = infer t2 env in
    begin
      match tt1, tt2 with
      | Type_arrow (a, b), c ->
        begin 
          match unify [a |> make_type_var, c] with
          | Some (u) -> Some(apply_unifier u b)
          | None -> None
        end
      | _ -> None
    end