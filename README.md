# Lili

Lili is a minimalist proof checker based on a simply typed lambda calculus.
I made this project to discover **type theory** and the wonderful world of formal proofs. The goal behind it is purely educational. I hope this project will evolve into a nice tool for curious students like me to experiment around logic and proof theory.

## What can we do (for now) with Lili ?

The current implementation of Lili consist of a tiny **type checker** capable of verifying proofs of propositions expressed in a logical system similar to *minimal logic*. Such proofs are expressed as explicitly typed lambda-terms in a minimalist meta-language inspired by Coq and OCaml notations.

### Lili logic's syntax

```
proposition ::=
  | [A..Z _]+
  | <proposition> -> <proposition>
  | ( <proposition> )
```

### Lili meta-language syntax

```
ident ::= [a..z _]+

type_annotation ::= <proposition>

axiom_declaration ::= Axiom <ident> : <proposition>

prop_statement ::= <prop_declaration> <prop_proof>

prop_declaration ::= Prop <ident> : <proposition>

prop_proof ::= Proof : proof_term

proof_term ::=
  | <ident>
  | [ <ident> : <type_annotation> ] => term
  | (term term)
```

## A detailed example

We introduce a small detailed example to demonstrate how to use Lili.
Let's suppose we want to proof the tautology `(A -> B) -> (A -> B)`.
First, we declare a new proposition :

```coq
Prop a_tautology : (A -> B) -> (A -> B)
```

Then we provide a proof

```coq
Proof :
  [ x : A -> B ] => [ y : A ] => (x y)
```
*This proof simply says that if I know `A -> B`, then if y know `A`, i can deduce `B` by a trivial application of **modus ponens***

Users can also define axioms and use them inside proofs.

```coq
(* We assume that A stands *)
Axiom axiom_a : A
(* We assume that A implies B *)
Axiom axiom_a_impl_b : A -> B

(* We prove B *)
Prop b_is_true : B
Proof: (axiom_a_impl_b axiom_a)
```





## Work in progress

The current state of Lili may appear a little bit disappointing : we can only manipulate tautologies, there is no way to define theorems and combine them etc... Of course such features are soon to come ! I currently work on many interesting ones :

+ A clean **unification algorithm** to introduce universal quantifiers and generic propositions
+ A **type inference** algorithm (thus, explicitly typed lambda-term will not be required anymore)
+ An **interactive interface** to build proofs without explicitly writing terms but only by calling inference rules in sequence.