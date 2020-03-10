((Prop {P->Q}->{R->P}->R->Q)
  (Proof
    (lambda (x:{P->Q})
      (lambda (y:{R->P})
        (lambda (z:R)
            (x (y z)))))))