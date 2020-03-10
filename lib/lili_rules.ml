open Lili_term

let rules = [
  "mp", Lambda (
    Bind ("x", Type_arrow (Type_atom "X", Type_atom "Y")),
    Lambda (Bind ("y", Type_atom "X"), Application (Var "x", Var "y"))
  )
]

let get_rule r =
  match List.assoc_opt r rules with
  | Some x -> x
  | _ -> failwith ("no rules named " ^ r)
