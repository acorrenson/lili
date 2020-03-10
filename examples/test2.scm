((Prop {A->B}->{B->C}->A->C)
  (Proof
    (lambda (x:{A->B})
      (lambda (y:{B->C})
        (lambda (z:A)
            (y (x z)))))))