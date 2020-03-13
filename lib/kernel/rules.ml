open Term
type thm = Thm of ptype

let rule_modus_ponens (Thm t1) (Thm t2) =
  match t1, t2 with
  | T_arrow (a, b), c when a = c -> Some (Thm b)
  | _ -> None

let axiom_K =
  Thm (ge "A" @@ ge "B" @@ ge "A")

let axiom_S =
  Thm ((ge "A" @@ ge "B" @@ ge "C") @@ (ge "A" @@ ge "B") @@ ge "A" @@ ge "B")

let axiom_I =
  Thm (ge "A" @@ ge "A")