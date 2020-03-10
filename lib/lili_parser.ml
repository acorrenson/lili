open Lili_term
open Lili_parsing
open Sexplib


let read_term s =
  Sexp.of_string s

let rec parse_atom inp = inp --> begin
    (type_atom <$> stringlitu)
    <|>
    (parenthesized '{' ~~parse_type '}')
  end
and parse_type inp = inp --> begin
    (type_arrow <$> ~~parse_atom <*> arrow *> ~~parse_type)
    <|>
    ~~parse_atom
  end
and parse_annotation inp = inp --> begin
    bind <$> stringlitl <*> ((String.make 1) <$> char ':') *> ~~parse_type
  end


let rec parse_term s =
  let open Sexp in
  match s with
  | Atom s -> Var s
  | List [Atom "lambda"; List [Atom binding]; expr] ->
    Lambda (parse_binding binding, parse_term expr)
  | _ -> failwith ("parsing error " ^ (to_string_hum s))


and parse_binding b =
  match do_parse ~~parse_annotation b with
  | Some x -> x
  | None -> failwith ("parsing error " ^ b)

