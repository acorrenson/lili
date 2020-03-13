
(* open Term

     module VSet = Set.Make(String)

     let axiom_K =
     Lambda (Bind ("x", Type_atom "'A"), Var "x")

     let axiom_S =
     let tp a = Type_atom a in
     let app a b = Application (a, b) in
     let bnd x y = Bind (x, y) in
     let (@@) a b = Type_arrow (a, b) in
     Lambda (
      bnd "x" (tp "'A" @@ tp "'B" @@ tp "'C"),
      Lambda (bnd "y" (tp "'A " @@ tp "'B"),
              Lambda (bnd "z" (tp "'A"),
                      app (app (Var "x") (Var "z")) (app (Var "y") (Var "z")))))

     let rec fv t =
     match t with
     | Var x -> VSet.singleton x
     | Application (a, b) -> VSet.union (fv a) (fv b)
     | Lambda (Bind (x, _), t) -> VSet.remove x (fv t)

     let nifv x t = not (VSet.mem x (fv t))
     let ifv x t = VSet.mem x (fv t)

     let rec ft t =
     match t with
     | Var x -> Var x
     | Application (t1, t2) ->
      Application (ft t1, ft t2)
     | Lambda (Bind (x, _), Var y) when x = y ->
      Var "I"
     | Lambda (Bind (x, _), t1) when nifv x t1 ->
      Application (Var "K", ft t1)
     | Lambda (Bind (x, _) as bx, Lambda (by, t1)) when ifv x t1 ->
      ft (Lambda (bx, ft (Lambda (by, t1))))
     | Lambda (b, Application (e1, e2)) ->
      let t1 = ft (Lambda (b, e1)) in
      let t2 = ft (Lambda (b, e2)) in
      Application (Application (Var "S", t1), t2)
     | _ -> failwith "s_k_basis reduction error" *)