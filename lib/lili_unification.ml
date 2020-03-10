(* open Lili_typing *)
open Lili_term

let (>>=) = Option.bind
let (let*) = (>>=)

type type_var =
  | Var_fixed of type_info
  | Var_atom of string
  | Var_arrow of type_var * type_var

let rec make_type_var t =
  match t with
  | Type_atom a -> Var_atom a
  | Type_arrow (a, b) -> Var_arrow (make_type_var a, make_type_var b)

let make_equation (l) =
  List.map (fun (a, b) -> make_type_var a, b) l

let rec substitute a b t =
  match t with
  | Var_atom x when x = a -> Var_fixed b
  | Var_arrow (x, y) ->
    Var_arrow (substitute a b x, substitute a b y)
  | _ -> t

let substitute_all a b lt =
  List.map (fun (x, y) -> substitute a b x, y) lt

let unify l =
  let rec step l sol =
    match l with
    (* Decompose *)
    | (Var_arrow (a, b), Type_arrow (a', b'))::rest ->
      print_endline "decompose";
      step ((a, a')::(b, b')::rest) sol
    (* Eliminate *)
    | (Var_atom a, b)::rest ->
      print_endline "eliminate";
      step (substitute_all a b rest) ((a, b)::sol)
    (* Clash_2 *)
    | (Var_arrow _, Type_atom _)::_ ->
      print_endline "clash_2";
      None
    | (Var_fixed x, y)::rest ->
      if (x = y) then step rest sol
      else (
        print_endline "clash_2";
        None
      )
    | [] -> Some (sol)
  in
  step l []

let pretty_unifier u =
  List.map (fun (s, a) -> s ^ ":" ^ (pretty_type a) ^ " ") u
  |> List.fold_left (^) ""

let rec apply_unifier u t =
  match t with
  | Type_atom x ->
    begin
      match List.assoc_opt x u with
      | Some y -> y
      | None -> t
    end
  | Type_arrow (a, b) ->
    Type_arrow (apply_unifier u a, apply_unifier u b)

let _ =
  let t1 = Type_arrow (Type_atom "A", Type_atom "B") |> make_type_var in
  let t2 = Type_atom "A" in
  unify [t1, t2]

let _ =
  let t1 = Type_arrow (Type_arrow (Type_atom "A", Type_atom "B"), Type_atom "A") |> make_type_var in
  let t2 = Type_arrow (Type_arrow (Type_atom "p", Type_atom "q"), Type_atom "r") in
  unify [t1, t2]
