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

(** {2 String conversions } *)

(** Stringify a type
    @param t  A value of type {!ptype} *)
let rec pretty_type t =
  match t with
  | T_gen s -> Printf.sprintf "'%s" s
  | T_atom s -> s
  | T_arrow (T_atom a, b) -> Printf.sprintf "%s -> %s" a (pretty_type b)
  | T_arrow (T_gen a, b) -> Printf.sprintf "'%s -> %s" a (pretty_type b)
  | T_arrow (a, b) ->
    Printf.sprintf "(%s) -> %s" (pretty_type a) (pretty_type b)
  | T_and (a, b) ->
    Printf.sprintf "(%s /\\ %s)" (pretty_type a) (pretty_type b)
  | T_or (a, b) ->
    Printf.sprintf "(%s \\/ %s)" (pretty_type a) (pretty_type b)

(** Stringify a term (with indentation)
    @param t  A value of type {!term} *)
let pretty_term t =
  let rec rec_pretty_term t p n =
    match t with
    | Lam (Bind (x, tx), t') ->
      Printf.sprintf "%sλ%s:(%s).\n%s%s" (String.make p ' ') x
        (pretty_type tx)
        (String.make (n + 2 + p) ' ')
        (rec_pretty_term t' 0 (n + 2 + p))
    | App (Var s, b) -> Printf.sprintf "(%s %s)" s (rec_pretty_term b 0 (n + 2))
    | App (a, b) ->
      Printf.sprintf "(%s\n%s)"
        (rec_pretty_term a 0 (n + 2))
        (rec_pretty_term b 1 (n + 2))
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
