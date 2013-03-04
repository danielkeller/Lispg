(define id (lambda (v) v))

(define stuff
  (letrec ((foo 5)
         (fac (lambda (v) (if (zero? v) 1 (* v (fac (- v 1))))))
         (baz (cons foo (fac foo))))
        baz))

(define len
  (lambda (l k)
    (if (null? l)
        (k 0)
        (len (cdr l) (lambda (v) (k (+ 1 v)))))))

(define len*
  (lambda (l)
    (cond 
      ((null? l) 0)
      ((list? (car l)) (+ (len* (car l)) (len* (cdr l))))
      (else (+ 1 (len* (cdr l)))))))

(define member?
  (lambda (a l k)
    (if (null? l)
      (k #f)
      (member? a (cdr l) (lambda (v) (k (or v (eq? a (car l)))))))))

(define set?
  (lambda (l k)
    (if (null? l)
        (k #t)
        (set? (cdr l) (lambda (v) (k (and v (not (member? (car l) (cdr l) id)))))))))
