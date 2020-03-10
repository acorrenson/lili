open Lili.Lili_term
open Lili.Lili_parser

let () =
  print_endline "Lili - A minimalist proof assistant";
  print_string "$> ";
  flush_all ();
  read_line ()
  |> read_term
  |> parse_term
  |> pretty_term |> print_endline