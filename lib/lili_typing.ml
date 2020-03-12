open Lili_term
open Option
open Lili_unification

type type_env = (string * type_info) list

let (let*) = Option.bind


let rec type_check t env =
  match t with
  | Lambda (Bind (_, Type_atom "?"), _) -> None
  | Lambda (Bind (x, tx), y) ->
    let env' = List.remove_assoc x env in
    let* ty = type_check y ((x,tx)::env') in
    Some (Type_arrow (tx, ty))
  | Var x ->
    List.assoc_opt x env
  | Application (t1, t2) ->
    let* tt1 = type_check t1 env in
    let* tt2 = type_check t2 env in
    (* match tt1, tt2 with
       | Type_arrow (a, b), c when a = c -> Some(b)
       | _ -> None *)
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

let counter = ref 0
let new_type () =
  let s = Printf.sprintf "'%c" (char_of_int (!counter + 65)) in
  incr counter;
  Var_atom s


(* let rec infer t env =
   match t with
   | Lambda (Bind (x, Type_atom "?"), y) ->
    let* ty = infer y env in
    let* tx = infer 
        match unify [nt, t] with
        |
          | _ -> None *)