# Lili

Lili is a minimalist proof checker based on a simply typed lambda calculus.
I made this project to discover **type theory** and the wonderful world of formal proofs. The goal behind it is purely educational. I hope this project will evolve into a nice tool for curious students to experiments around logic and proof theory.

## What can we do (for now) with Lili ?

The current implementation of Lili consist of a tiny **type checker** capable of certifying proofs of propositions in a logical system similar to *Hilbert-style minimal logic*.

To use Lili, programmers need to provide a script declaring targets propositions and their proof. Proofs are written directly as explicitly typed lambda-terms in a lisp-like syntax :

```scheme
(
  ;; A very stupid proof : proving that forall propositions A and B, (A -> B) -> A -> B
  (Prop ({A->B}->A->B)
  (Proof
    (lambda (x:{A->B}) (lambda (y:A) ((x y))))))
)
```

## Work in progress

The current state of Lili may appear a little bit disappointing : we can only manipulate tautologies, there is no way to define theorems and combine them etc... Of course such features are soon to come ! I currently work on many interesting ones :

+ A clean **unification algorithm** to introduce universal quantifiers and generic propositions
+ An **extended language** to define multiple theorems in one script and use them in proofs
+ A *type inference* algorithm (thus, explicitly typed lambda-term will not be required anymore)
+ An **interactive interface** to build proofs without explicitly writing terms but only by calling inference rules in sequence.

<!-- **Axioms** :
  + `A -> B -> A` (**K**)
  + `(A -> B -> C) -> (A -> B) -> A -> C` (**S**)
  + `A -> A` (**I**)

**Rules** :
  + Modus Ponens only (if one can derive `A` and `A -> B` from context, so `B` can be derived)

**Notes** :
  + From axioms **K** and **S** we can derive a third axiom **I** : `A -> A` -->

*I'd like to thank my friend Lison for all the amazing discussions we had. I named this project after her*