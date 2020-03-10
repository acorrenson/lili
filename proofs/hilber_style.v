
Axiom axiom_k:
  forall p, forall q, p -> q -> p.

Axiom axiom_s:
  forall p, forall q, forall r, 
  (p -> q -> r) -> (q -> r) -> q -> r.

Lemma B:
  forall p, forall q, forall r,
  (p -> q) -> (r -> p) -> r -> q.
Proof.
  intros.
  apply axiom_s.
  apply X0.
  auto.
Qed.