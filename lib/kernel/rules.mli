open Term

(** Theorems *)
type thm

val rule_modus_ponens : thm -> thm -> thm option
(** Modus Ponens inference rule *)

val axiom_S : thm
(** Axiom S (S combinator) *)

val axiom_K : thm
(** Axiom K (K combinator) *)

val axiom_I : thm
(** Axiom I (I combinator) *)

val ski_rewrite : term -> term
(** Rewrite a term using only I, K and S combinators *)