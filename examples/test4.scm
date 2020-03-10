((Prop {{A->B}->{C->D}}->{{E->F}->{A->B}}->{E->F}->{C->D})
  (Proof
    (lambda (x:{W->X})
      (lambda (y:{Y->W})
        (lambda (z:{Y})
          (x (y z)))))))