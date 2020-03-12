# lili

Lili is a minimalist proof checker based on a simply typed lambda calculus.
I made this project to discover **type theory** and the wonderfull world of formal proofs. The goal behind it is purely educational. I hope this project will evolve into a nice tool for currrious students to experiments arround logic and proof theory.

## What can we do (for now) with Lili ?

The current implementation of lili consist of a tiny **type checker** capable of certifying proofs of propositions in a logical system similar to **Hilbert-style minimal logic*.

**Lili Axioms** :
  + `A -> B -> A` (**K**)
  + `(A -> B -> C) -> (A -> B) -> A -> C` (**S**) 

**Lili Rules** : 
  + Modus Ponens only (if one can derive `A` and `A -> B` from context, so `B` can be derived)

**Notes** :
  + From axioms **K** and **S** we can derive a third axiom **I** : `A -> A`
