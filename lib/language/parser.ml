open Kernel.Term
open Parsing

type entity =
  | Axiom of ptype
  | Target of ptype * term
[@@deriving variants]

let parse_type =
  let rec atom_type inp =
    inp --> (t_atom <$> stringlitu <|> parenthesized '(' ~~ptype ')')

  and arrow_type inp =
    inp --> (t_arrow <$> ~~atom_type <*> (spaced arrow) *> ~~ptype)

  and ptype inp =
    inp --> (~~arrow_type <|> ~~atom_type)
  in ~~ptype


let parse_prop_stmt =
  literal "Prop" *> spaced (char ':') *> parse_type

let parse_bind = bind <$> stringlitl <*> spaced (char ':') *> parse_type

(* 
  term ::=
    | var
    | [ bind ] => term
    | (term term)
 *)

let parse_term =
  let rec pterm inp =
    inp --> trimed (~~variable <|> ~~abstraction <|> ~~application)

  and variable inp =
    inp --> (var <$> (spaced stringlitl))

  and abstraction inp =
    inp --> (lam <$> parenthesized '[' parse_bind ']' <*> (spaced (literal "=>")) *> ~~pterm)

  and application inp =
    inp --> (app <$> char '(' *> ~~pterm <*> ~~pterm <* char ')')
  in
  ~~pterm

let parse_proof_stmt =
  literal "Proof" *> spaced (char ':') *> parse_term

let parse_script =
  many (target <$> parse_prop_stmt <*> blanks *> parse_proof_stmt)


let read_all f =
  let rec loop acc =
    try
      loop (acc ^ (input_line f) ^ "\n")
    with End_of_file -> acc
  in
  loop ""

(* open Kernel
   open Term
   open Parsing
   open Sexplib

   let rec parse_atom_type inp =
   inp --> (t_atom <$> stringlitu <|> parenthesized '{' ~~parse_type '}')

   and parse_type inp =
   inp
   --> ( t_arrow <$> ~~parse_atom_type <*> arrow *> ~~parse_type
        <|> ~~parse_atom_type )

   and parse_annotation inp =
   inp --> (bind <$> stringlitl <*> (String.make 1 <$> char ':') *> ~~parse_type)

   let rec parse_term s =
   let open Sexp in
   match s with
   | Atom s -> Var s
   | List [ Atom "lambda"; List [ Atom binding ]; expr ] ->
    Lam (parse_binding binding, parse_term expr)
   | List [ a; b ] -> App (parse_term a, parse_term b)
   | _ -> failwith ("parsing error " ^ to_string_hum s)

   and parse_binding b =
   match do_parse ~~parse_annotation b with
   | Some x -> x
   | None -> failwith ("parsing error " ^ b)

   let parse_prop p =
   let open Sexp in
   match p with
   | Atom s -> (
      match do_parse ~~parse_type s with
      | Some t -> t
      | _ -> failwith ("invalid proposition " ^ s) )
   | _ -> failwith "invalid syntax : missing valid 'Prop' clause"

   let parse_script s =
   let open Sexp in
   match s with
   | List [ List [ Atom "Prop"; p ]; List [ Atom "Proof"; q ] ] ->
    let prop = parse_prop p in
    let proof = parse_term q in
    (prop, proof)
   | _ -> failwith "Syntax error in the script" *)
