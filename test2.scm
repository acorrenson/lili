(
  (Prop {A->B}->{B->C}->A->B->C)
  (Proof
    (lambda (x:{A->B}) 
      (lambda (y:{B->C}) 
        (lambda (z:A) 
          (lambda (t:B) (y (x z)))
)))))