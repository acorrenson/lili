open Kernel.Term
open Parsing

type entity =
  | Axiom of string * ptype
  | Target of (string * ptype) * term
[@@deriving variants]

let named_prop a b = (a, b)

let parse_type =
  let rec atom_type inp =
    inp --> (t_atom <$> stringlitu <|> parenthesized '(' ~~ptype ')')

  and arrow_type inp =
    inp --> (t_arrow <$> ~~atom_type <*> (spaced arrow) *> ~~ptype)

  and ptype inp =
    inp --> (~~arrow_type <|> ~~atom_type)
  in ~~ptype


let parse_prop_stmt =
  named_prop <$> literal "Prop" *> (spaced stringlitl) <*> spaced (char ':') *> parse_type
  |> trimed

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
  |> trimed

let parse_axiom =
  axiom <$> literal "Axiom" *> (spaced stringlitl) <*> spaced (char ':') *> parse_type
  |> trimed

let parse_script =
  many ((target <$> parse_prop_stmt <*> blanks *> parse_proof_stmt) <|> parse_axiom)


let read_all f =
  let rec loop acc =
    try
      loop (acc ^ (input_line f) ^ "\n")
    with End_of_file -> acc
  in
  loop ""