(define merge
  (lambda (l r)
    (cond
      ((null? l) r)
      ((null? r) l)
      ((< (car l) (car r)) (cons (car l) (merge (cdr l) r)))
      (else (cons (car r) (merge l (cdr r)))))))

(define evens
  (lambda (ev l)
    (cond
      ((null? l) l)
      (ev (evens (not ev) (cdr l)))
      (else (cons (car l) (evens (not ev) (cdr l)))))))

(define Mergesort
  (lambda (l)
    (if (or (null? l) (null? (cdr l))) l
        (merge (Mergesort (evens #t l)) (Mergesort (evens #f l))))))
