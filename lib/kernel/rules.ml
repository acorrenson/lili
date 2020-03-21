open Term

type thm = Thm of ptype

let rule_modus_ponens (Thm t1) (Thm t2) =
  match (t1, t2) with T_arrow (a, b), c when a = c -> Some (Thm b) | _ -> None

let axiom_K =
  Thm (ge "A" @@ ge "B" @@ ge "A")

let axiom_S =
  Thm ((ge "A" @@ ge "B" @@ ge "C") @@ (ge "A" @@ ge "B") @@ ge "A" @@ ge "B")

let axiom_I =
  Thm (ge "A" @@ ge "A")

module VariableSet = Set.Make (String)

let rec free_vars t =
  match t with
  | Var x -> VariableSet.singleton x
  | App (a, b) -> VariableSet.union (free_vars a) (free_vars b)
  | Lam (Bind (x, _), t) -> VariableSet.remove x (free_vars t)

let nifv x t = not (VariableSet.mem x (free_vars t))

let ifv x t = VariableSet.mem x (free_vars t)

let rec ski_rewrite t =
  match t with
  | Var x -> Var x
  | App (t1, t2) -> App (ski_rewrite t1, ski_rewrite t2)
  | Lam (Bind (x, _), Var y) when x = y -> Var "I"
  | Lam (Bind (x, _), t1) when nifv x t1 -> App (Var "K", ski_rewrite t1)
  | Lam ((Bind (x, _) as bx), Lam (by, t1)) when ifv x t1 ->
    ski_rewrite (Lam (bx, ski_rewrite (Lam (by, t1))))
  | Lam (b, App (e1, e2)) ->
    let t1 = ski_rewrite (Lam (b, e1)) in
    let t2 = ski_rewrite (Lam (b, e2)) in
    App (App (Var "S", t1), t2)
  | _ -> failwith "S-K-I basis reduction error"
