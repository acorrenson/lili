(** {2 Lambda Terms and Types } *)

(** Propositions/types *)
type ptype = 
  | T_gen of string
  | T_atom of string
  | T_and of ptype * ptype
  | T_or of ptype * ptype
  | T_arrow of ptype * ptype
[@@deriving variants]

type binding = Bind of string * ptype [@@deriving variants]

(** Lambda terms *)
type term = Lam of binding * term | App of term * term | Var of string
[@@deriving variants]

(** {2 Simple notations } *)

let ( @@ ) = t_arrow

let ge = t_gen

let at = t_atom

let ar = t_arrow

module VarSet = Set.Make(String)

let rec fv t =
  match t with
  | T_arrow (a, b)
  | T_and (a, b)
  | T_or (a, b) -> VarSet.union (fv a) (fv b)
  | T_gen x -> VarSet.add x (VarSet.empty)
  | _ -> VarSet.empty

(** {2 String conversions } *)

(** Stringify a type
    @param t  A value of type {!ptype} *)
let rec str_ty t =
  let open Printf in
  match t with
  | T_gen s
  | T_atom s -> s
  | T_arrow ((T_arrow _) as a, b) ->
    sprintf "(%s) -> %s" (str_ty a) (str_ty b)
  | T_arrow (a, b) ->
    sprintf "%s -> %s" (str_ty a) (str_ty b)
  | T_and ((T_arrow _ as a), (T_arrow _ as b)) ->
    sprintf "(%s) /\\ (%s)" (str_ty a) (str_ty b)
  | T_and ((T_arrow _) as a, b) ->
    sprintf "(%s) /\\ %s" (str_ty a) (str_ty b)
  | T_and ((T_or _ as a), (T_or _ as b)) ->
    sprintf "(%s) /\\ (%s)" (str_ty a) (str_ty b)
  | T_and ((T_or _) as a, b) ->
    sprintf "(%s) /\\ %s" (str_ty a) (str_ty b)
  | T_and (a, b) ->
    sprintf "%s /\\ %s" (str_ty a) (str_ty b)
  | T_or ((T_arrow _ as a), (T_arrow _ as b)) ->
    sprintf "(%s) \\/ (%s)" (str_ty a) (str_ty b)
  | T_or ((T_arrow _ as a), b) ->
    sprintf "(%s) \\/ %s" (str_ty a) (str_ty b)
  | T_or (a, b) ->
    sprintf "%s \\/ %s" (str_ty a) (str_ty b)

let pretty_type t =
  let f = fv t in
  if f = VarSet.empty then (
    str_ty t
  ) else (
    let prefix = VarSet.fold (fun a b -> b ^ " " ^ a) f "forall" in
    Printf.sprintf "%s, %s" prefix (str_ty t)
  )


(** Stringify a term (with indentation)
    @param t  A value of type {!term} *)
let pretty_term t =
  let rec rec_pretty_term t p n =
    match t with
    | Lam (Bind (x, _), t') ->
      Printf.sprintf "%sλ%s.\n%s%s" (String.make p ' ') x
        (String.make (n + 2 + p) ' ')
        (rec_pretty_term t' 0 (n + 2 + p))
    | App (a, b) -> Printf.sprintf "(%s %s)" (rec_pretty_term a 0 (n + 2)) (rec_pretty_term b 0 (n + 2))
    | Var x -> x
  in
  rec_pretty_term t 0 0

(** Stringify a term (without indentation)
    @param t  A value of type {!term} *)
let rec pretty_term_inline t =
  match t with
  | Lam (Bind (x, _), t') ->
    Printf.sprintf "λ%s.%s" x (pretty_term_inline t')
  | App (Var s, b) -> Printf.sprintf "(%s %s)" s (pretty_term_inline b)
  | App (a, b) ->
    Printf.sprintf "(%s %s)" (pretty_term_inline a) (pretty_term_inline b)
  | Var x -> x