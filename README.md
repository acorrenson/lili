# Lili

Lili is a minimalist proof checker based on a simply typed lambda calculus.
I made this project to discover **type theory** and the wonderful world of formal proofs. The goal behind it is purely educational. I hope this project will evolve into a nice tool for curious students like me to experiment around logic and proof theory.

## What can we do (for now) with Lili ?

The current implementation of Lili consist of a tiny **type checker** capable of verifying proofs of propositions expressed in a simple logical system. Such proofs are expressed as explicitly typed lambda-terms in a minimalist meta-language inspired by Coq and OCaml notations.

### Lili's logic syntax

```
propname ::= [A..Z _]+

proposition ::=
  | <propname>
  | '<propname>
  | <proposition> -> <proposition>
  | <proposition> /\ <proposition>
  | <proposition> \/ <proposition>
  | ( <proposition> )
```

### Lili's meta-language syntax

```
ident ::= [a..z _]+

type_annotation ::= <proposition>

axiom_declaration ::= Axiom <ident> : <proposition>

prop_statement ::= <prop_declaration> <prop_proof>

prop_declaration ::= Prop <ident> : <proposition>

prop_proof ::= Proof : term

term ::=
  | <ident>
  | [ <ident> : <type_annotation> ] => <term>
  | (<term> <term>)
```

### More on Lili's logic

The Logic of Lili can be described by 6 axioms :

| axiom | type                                          |
| :---: | :-------------------------------------------- |
|  fst  | 'A /\ 'B -> 'A                                |
|  snd  | 'A /\ 'B -> 'B                                |
|  and  | 'A -> 'B -> 'A /\ 'B                          |
| case  | ('A -> 'P) -> ('B -> 'P) -> ('A \\/ 'B -> 'P) |
| or_l  | 'A -> ('A \\/ 'B)                             |
| or_r  | 'B -> ('A \\/ 'B)                             |

These axioms can be used directly inside proofs.

## A detailed example

Let's consider the following example to demonstrate how to use Lili.

```coq
Axiom fst : 'A /\ 'B -> 'A
Axiom snd : 'A /\ 'B -> 'B
Axiom and : 'A -> 'B -> ('A /\ 'B)

Prop and_commut : 'A /\ 'B -> 'B /\ 'A
Proof:
  [ x : 'A /\ 'B ] => ((and (snd x)) (fst x))
```

The 3 first lines are axioms declarations. We assume 3 things :
+ For all propositions A and B, if we know that `A /\ B` is true, then `A` is necessarily true.
+ For all propositions A and B, if we know that `A /\ B` is true, then `B` is necessarily true.
+ For all propositions A and B, if we know that `A` is true and `B` is true, then `A /\ B` is necessarily true.

Provided such axioms, we now proof that `A /\ B -> B /\ A` for all propositions A and B. The proof is written as an explicitly typed functional program whose type annotations correspond to the proposition we want to prove.

```coq
[ x : 'A /\ 'B ] => ((and (snd x)) (fst x))
```

This piece of code is to be read as a function which map any proof `x` of the proposition `A /\ B` to a proof of `B /\ A`, obtained by combining the functions (proofs) `fst`, `snd` and `and`.

A more complete example can be found [here](./examples/logic_prop.lili)

## TODOS

The current implementation of Lili lacks of many things

+ [x] Unification and meta properties
+ [ ] A more expressive logic (we do not have TOP nor BOTTOM, seriously ?)
+ [ ] Type inference (no need to explicitly type lambda terms)
+ [ ] A command line interface
+ [ ] A GUI
